package regexenginego

import (
	"fmt"
	"strconv"
	"strings"
)

type tokenType uint8 // <1> type alias for the character type

const ( // <2> constants for specifiying the type of the token we are working with; think enums.
	group           tokenType = iota
	bracket         tokenType = iota
	or              tokenType = iota
	repeat          tokenType = iota
	literal         tokenType = iota
	groupUncaptured tokenType = iota
)

type token struct { // <3> The main token struct, it has a type and a value which can be anything depending on the given type
	tokenType tokenType
	// the payload required for each token will be different
	// so we need to be flexible with the type
	value interface{}
}

type parseContext struct { // <4> Will help us keep track of our positions and will hold the tokens parsed so far
	// the index of the character we're processing
	// in the regex string
	pos    int
	tokens []token
}

type repeatPayload struct {
	min   int
	max   int
	token token
}

// Parsing Algorithm
// We loop through each character and process it until we reach to the end of the regex string
func parse(regex string) *parseContext {
	// Initialize our struct with a pointer to parseContext
	ctx := &parseContext{
		pos:    0,
		tokens: []token{},
	}
	for ctx.pos < len(regex) {
		process(regex, ctx)
		ctx.pos++
	}

	return ctx
}

func process(regex string, ctx *parseContext) {
	ch := regex[ctx.pos]
	if ch == '(' { // <1> if curr char is (, the opening paranthesis, we know we're dealing with a gropu, thus we try to parse the next couple of characters as a group.
		groupCtx := &parseContext{
			pos:    ctx.pos,
			tokens: []token{},
		}
		parseGroup(regex, groupCtx)
		ctx.tokens = append(ctx.tokens, token{
			tokenType: group,
			value:     groupCtx.tokens,
		})
	} else if ch == '[' { // <2> if it is [, the opening bracket, we know that it is a bracket expression, so we proceed accordingly
		parseBracket(regex, ctx)
	} else if ch == '|' { // <3> if it is a vertical pipe - |, that is an Or expression (alternative)
		parseOr(regex, ctx)
	} else if ch == '*' || ch == '?' || ch == '+' { // <4> A repitition character of *, +, or ?. Although ? means optional, in terms of repitition, ? simply means that the character repeats once
		parseRepeat(regex, ctx)
	} else if ch == '{' { // <5> Curly braces specify repetition as well. In fact, the previous repition options can all be specified using braces
		parseRepeatSpecified(regex, ctx)
	} else { // <6> if character does not match with anything, we consider it as a literal.
		// literal
		t := token{
			tokenType: literal,
			value:     ch,
		}

		ctx.tokens = append(ctx.tokens, t)
	}
}

// Parse a group by calling on process function for each position except closing paranthesis
func parseGroup(regex string, ctx *parseContext) {
	ctx.pos += 1 // get past the left parenthesis (
	for regex[ctx.pos] != ')' {
		process(regex, ctx)
		ctx.pos += 1
	}
}

// Parse a bracket
func parseBracket(regex string, ctx *parseContext) {
	ctx.pos++ // get past the left bracket
	var literals []string
	for regex[ctx.pos] != ']' { // <1> Go through each char till right bracket
		ch := regex[ctx.pos]

		if ch == '-' { // <2> Check if char is - because that is range indicator e.g. a-zK-Z2-8
			next := regex[ctx.pos+1]                                    // <3-1> Take the next char from the regex string
			prev := literals[len(literals)-1][0]                        // <3-1> and take the previous char from the list of literals (last char in that list)
			literals[len(literals)-1] = fmt.Sprintf("%c%c", prev, next) // <3-2> Then save it back to the list as 2 chars together
			ctx.pos++                                                   // to consume the 'next' char
		} else { // <4> If not a range indicator, then consider as a single literal. Save to literals list
			literals = append(literals, fmt.Sprintf("%c", ch))
		}
		ctx.pos++ // <5> Move to next char
	}

	literalsSet := map[uint8]bool{}

	for _, l := range literals { // <6> Go over the literals list
		for i := l[0]; i <= l[len(l)-1]; i++ { // <7> Add all the characters between the first and last characters of the saved value
			literalsSet[i] = true
		}
	}

	ctx.tokens = append(ctx.tokens, token{ // <8> In single literals, only itself will be added. In ranges, it will add everything from first char till last char (inclusive). Finally, once all dups are removed, we save the bracket token to our list of tokens
		tokenType: bracket,
		value:     literalsSet,
	})
}

// Parse Alternations
func parseOr(regex string, ctx *parseContext) {
	// <1:start> This is the following syntax <left>|<right> or (<left>|<right>). By the
	// time the | symbol is encountered, the left side has already been parsed. Now, it is time to parse
	// everything to the right. Similar code to parsing groups by creating a right hand specific context.
	// Collect all tokens in that context until the end of the regex string or until facing closing parenthesis.
	rhsContext := &parseContext{
		pos:    ctx.pos,
		tokens: []token{},
	}
	rhsContext.pos += 1 // get past |
	for rhsContext.pos < len(regex) && regex[rhsContext.pos] != ')' {
		process(regex, rhsContext)
		rhsContext.pos += 1
	}
	// <1:end>

	// both sides of the OR expression
	left := token{
		tokenType: groupUncaptured,
		value:     ctx.tokens, // <2> Once done parsing both sides, create alternation token. Left side is just original context tokens, since already parsed.
	}

	right := token{ // <3>
		tokenType: groupUncaptured,
		value:     rhsContext.tokens,
	}
	ctx.pos = rhsContext.pos // <4> Update position of original context

	ctx.tokens = []token{{ // <5> Create alternation token and add to original context. Do not keep old tokens as they are contained in this alternation token.
		tokenType: or,
		value:     []token{left, right},
	}}
}

const repeatInfinity = -1 // Used as 'infinity'

func parseRepeat(regex string, ctx *parseContext) {
	ch := regex[ctx.pos]
	var min, max int
	if ch == '*' {
		min = 0
		max = repeatInfinity
	} else if ch == '?' {
		min = 0
		max = 1
	} else {
		// ch == '+'
		min = 1
		max = repeatInfinity
	}
	// we need to wrap the last token with the quantifier data
	// so that we know what the min and max apply to
	lastToken := ctx.tokens[len(ctx.tokens)-1]
	ctx.tokens[len(ctx.tokens)-1] = token{
		tokenType: repeat,
		value: repeatPayload{
			min:   min,
			max:   max,
			token: lastToken,
		},
	}
}

func parseRepeatSpecified(regex string, ctx *parseContext) {
	// +1 because we skip left curly { at the beginning
	start := ctx.pos + 1
	// proceed until we reach to the end of the curly braces
	for regex[ctx.pos] != '}' {
		ctx.pos++
	}

	boundariesStr := regex[start:ctx.pos]       // <1> Get the string expression for boundaries (everything between curly braces)
	pieces := strings.Split(boundariesStr, ",") // <2> Split boundariesStr by a comma ,
	var min, max int
	if len(pieces) == 1 { // <3> If there is only one element {1}, then min and max are the same
		if value, err := strconv.Atoi(pieces[0]); err != nil {
			panic(err.Error())
		} else {
			min = value
			max = value
		}
	} else if len(pieces) == 2 { // <4> If there are two pieces, the first value is min, and the second one is max. If second value is missing, then it will be repeatInfinity
		if value, err := strconv.Atoi(pieces[0]); err != nil {
			panic(err.Error())
		} else {
			min = value
		}
		if pieces[1] == "" {
			max = repeatInfinity
		} else if value, err := strconv.Atoi(pieces[1]); err != nil {
			panic(err.Error())
		} else {
			max = value
		}
	} else {
		panic(fmt.Sprintf("There must be either 1 or 2 values specified for the quantifier: provided '%s'", boundariesStr))
	}

	// Wrap the last token with the quantifier data
	// so that we know what the min and max apply to
	// <5> Take the last parsed token, wrap it in a repeat token and appropriate boundaries. Then, save it back to the same position in the tokens list
	lastToken := ctx.tokens[len(ctx.tokens)-1]
	ctx.tokens[len(ctx.tokens)-1] = token{
		tokenType: repeat,
		value: repeatPayload{
			min:   min,
			max:   max,
			token: lastToken,
		},
	}
}

// start indicates whether this state is an entrypoint or not
// terminal indicates whether this state is the final state. Reaching this state means the input matches the regex.
// transitions is a map where a char maps to a list of different states
type state struct {
	start       bool
	terminal    bool
	transitions map[uint8][]*state
}

const epsilonChar uint8 = 0 // empty character

func toNfa(ctx *parseContext) *state {
	startState, endState := tokenToNfa(&ctx.tokens[0]) // <1> Take first token, and convert that to an NFA. tokenToNFA returns start and end states of newly created NFA.

	for i := 1; i < len(ctx.tokens); i++ { // <2> Go over the rest of the tokens
		startNext, endNext := tokenToNfa(&ctx.tokens[i]) // <3> Call tokenToNfa for each token and save start and end states
		endState.transitions[epsilonChar] = append(
			endState.transitions[epsilonChar],
			startNext,
		) // <4> Link the end state from previous tokenToNfa call to the start state of the new NFA.
		endState = endNext // <5> Save end state, because will be useful for next iteration
	}

	start := &state{ // <6> Create previously mentioned start state. This state now has an epsilon transition to the start state of the very first NFA created.
		transitions: map[uint8][]*state{
			epsilonChar: {startState},
		},
		start: true,
	}
	end := &state{ // <7> Create the terminal state
		transitions: map[uint8][]*state{},
		terminal:    true,
	}

	endState.transitions[epsilonChar] = append(
		endState.transitions[epsilonChar],
		end,
	) // <8> End state of the last NFA created now has an epsilon transition to the terminal state.

	return start // <9> Finally, return the start state due to being the entrypoint
}

// returns (start, end)
func tokenToNfa(t *token) (*state, *state) {
	start := &state{
		transitions: map[uint8][]*state{},
	}
	end := &state{
		transitions: map[uint8][]*state{},
	}

	switch t.tokenType {
	case literal:
		ch := t.value.(uint8)
		start.transitions[ch] = []*state{end}
	case or:
		values := t.value.([]token)
		left := values[0]
		right := values[1]

		s1, e1 := tokenToNfa(&left)  // <1> Create left NFA
		s2, e2 := tokenToNfa(&right) // <1> Create right NFA

		start.transitions[epsilonChar] = []*state{s1, s2} // <2> Connecting the start state with the starts of both NFAs with an epsilon transition
		e1.transitions[epsilonChar] = []*state{end}       // <3> Connecting end state e1 with end state of the Or NFA
		e2.transitions[epsilonChar] = []*state{end}       // <3> Connecting end state e2 with end state of the Or NFA
	case bracket:
		literals := t.value.(map[uint8]bool)

		for l := range literals { // <1> Go through each literal in the bracket
			start.transitions[l] = []*state{end} // <2> Add a transition from start state to end state
		}
	case group, groupUncaptured:
		// no difference between these two cases in our engine
		// this case concatenates all individual NFAs created from the tokens inside the group.
		tokens := t.value.([]token)
		start, end = tokenToNfa(&tokens[0])
		for i := 1; i < len(tokens); i++ {
			ts, te := tokenToNfa(&tokens[i])
			end.transitions[epsilonChar] = append(
				end.transitions[epsilonChar],
				ts,
			)
			end = te
		}
	case repeat:
		p := t.value.(repeatPayload)

		if p.min == 0 { // <1> if min is 0, then need an epsilon transition from start state to end state
			start.transitions[epsilonChar] = []*state{end}
		}

		var copyCount int // <2> variable that will hold the max number of times need to create an NFA from p.token

		if p.max == repeatInfinity {
			if p.min == 0 {
				copyCount = 1
			} else {
				copyCount = p.min
			}
		} else {
			copyCount = p.max
		}

		from, to := tokenToNfa(&p.token)         // <3> Concatenate multiple NFAs, start by creating the first copy of this NFA.
		start.transitions[epsilonChar] = append( // <4> start state is connected to the start of this new NFA
			start.transitions[epsilonChar],
			from,
		)

		for i := 2; i <= copyCount; i++ { // <5> Iterate over the remaining amount of times
			s, e := tokenToNfa(&p.token)

			// connect the end of the previous one
			// to the start of this one
			to.transitions[epsilonChar] = append( // <6> The concatenation step. Connect the end of the previous NFA to the start of the current NFA.
				to.transitions[epsilonChar],
				s,
			)

			// keep track of the previous NFA's entry and exit states
			from = s // <7> Save start of current NFA
			to = e   // <7> Save end of current NFA

			// after the minimum required amount of repitions
			// the rest must be optional, thus we add an
			// epsilon transition to the start of each NFA
			// so that we can skip them if needed
			if i > p.min { // <8> Once finished with creating min number of NFAs, rest must be optional. Thus, add epsilon transition from start to end state for each.
				s.transitions[epsilonChar] = append(
					s.transitions[epsilonChar],
					end,
				)
			}
		}

		to.transitions[epsilonChar] = append( // <9> connect end of the last NFA, to end state
			to.transitions[epsilonChar],
			end,
		)

		if p.max == repeatInfinity { // <10> If upper bound is infinity, add an epsilon transition from end state to the start fo the last NFA
			end.transitions[epsilonChar] = append(
				end.transitions[epsilonChar],
				from,
			)
		}
	default:
		panic("unknown type of token")
	}

	return start, end
}

const (
	startOfText uint8 = 1
	endOfText   uint8 = 2
)

func getChar(input string, pos int) uint8 {
	if pos >= len(input) {
		return endOfText
	}

	if pos < 0 {
		return startOfText
	}

	return input[pos]
}

func (s *state) check(input string, pos int) bool { // <1> (s *state) a way to add a func to the state struct
	ch := getChar(input, pos) // <2> get the current char to check for. Can be a valid char from the string, startOfText, or endOfText

	if ch == endOfText && s.terminal { // <3> If at end of string and at the terminal state, that means the string matches the regex, so return true
		return true
	}

	if states := s.transitions[ch]; len(states) > 0 { // <4> For current char, check if there is any transition specified.
		nextState := states[0]
		if nextState.check(input, pos+1) { // <5> If such transition exists, grab the next state, increment the position pointer and check. If check is a success, return true
			return true
		}
	}

	for _, state := range s.transitions[epsilonChar] { // <6> If no char transition, check for empty/epsilon transitions
		if state.check(input, pos) { // <7> For all such transitions, try to check the next state with the same input and position. Since an empty transition, there is no need for incrementing the position. If check is success, return true.
			return true
		}

		if ch == startOfText && state.check(input, pos+1) { // <8> If the chance is that at the start of the text and stuck. This condition will prevent that
			return true
		}
	}

	return false // <9> If nothing matches, it is a failure, return false.
}
