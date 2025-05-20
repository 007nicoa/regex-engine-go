package regex

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
