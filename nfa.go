package regex

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
