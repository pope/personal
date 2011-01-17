if has("python")
python << EOF
import vim
def _fallout3_hack_score(word, guess):
    """
    Returns the number of characters these words have in common
    """
    num_matches = 0;
    for i in xrange(0, min(len(word), len(guess))):
        if word[i] == guess[i]:
            num_matches += 1
    return num_matches


def fallout3_hack_sort():
    """
    Goes through the buffer and sorts the words with the word with the most
    common characters at the top
    """

    # Create a dict to store the word and it's score
    word_to_score = {}
    cb = vim.current.buffer
    cb_len = len(cb)

    for i in xrange(0, cb_len):
        for j in xrange(0, cb_len):
            if cb[i] != cb[j]:
                word_to_score[cb[i]] = max(word_to_score.get(cb[i], 0),
                                           _fallout3_hack_score(cb[i], cb[j]))

    # Python 2.3 complient....ugh
    # Make a list to sort said list by score.  Then just pull out the key
    ordered_words = [(v,k) for (k,v) in word_to_score.items()]
    ordered_words.sort()
    ordered_words.reverse()
    ordered_words = [k for (v,k) in ordered_words]

    # Update the buffer
    for i in xrange(0, len(ordered_words)):
        cb[i] = ordered_words[i]


def fallout3_hack_guess():
    """
    Enter the result of your guess

    Uses the current line as your guess and asks you for how many characters
    you got right.  It then goes through the list to see what else matches and
    adjusts the file to only show those lines.  This also re-sorts the file
    with the word with the most common characters at the top
    """

    # Rather than ask, assume...this is gonna bite me, I know it
    guess = vim.current.line

    # Ask for user input
    vim.command('call inputsave()')
    vim.command('let user_input = input("How many characters were correct? ")')
    vim.command('call inputrestore()')
    num_correct = int(vim.eval('user_input'))

    def is_valid(word):
        if word == guess:
            return False
        return _fallout3_hack_score(word, guess) == num_correct

    cb = vim.current.buffer
    cb_len = len(cb)

    # Create a list of the remaining valid words
    valid_words = [cb[x] for x in xrange(0, cb_len) if is_valid(cb[x])]
    valid_words_len = len(valid_words)

    if valid_words_len < cb_len:
        # Update the buffer
        for x in xrange(0, valid_words_len):
            cb[x] = valid_words[x]
        # Remove the left-over lines
        del cb[valid_words_len:cb_len]

    #Always resort the list when we're done
    fallout3_hack_sort()
EOF

map <Leader>fo3s :python fallout3_hack_sort()<CR>
map <Leader>fo3g :python fallout3_hack_guess()<CR>

endif
