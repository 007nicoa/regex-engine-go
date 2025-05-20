package regex

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
