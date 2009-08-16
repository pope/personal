python << EOF
import vim
def _fallout3_hack_score(word, guess):
    num_matches = 0;
    for i in xrange(0, min(len(word), len(guess))):
        if word[i] == guess[i]:
            num_matches += 1
    return num_matches

def fallout3_hack_sort():
    word_to_score = {}
    cb = vim.current.buffer
    cb_len = len(cb)
    for i in xrange(0, cb_len):
        for j in xrange(0, cb_len):
            if cb[i] != cb[j]:
                word_to_score[cb[i]] = max(word_to_score.get(cb[i], 0),
                                           _fallout3_hack_score(cb[i], cb[j]))
    ordered_words = [k for (k,v) in sorted(word_to_score.items(),
                                           key=lambda (k,v):(v,k),
                                           reverse=True)]
    for i in xrange(0, len(ordered_words)):
        cb[i] = ordered_words[i]


def fallout3_hack_guess():
    guess = vim.current.line

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
    valid_words = [cb[x] for x in xrange(0, cb_len) if is_valid(cb[x])]
    valid_words_len = len(valid_words)
    if valid_words_len < cb_len:
        for x in xrange(0, valid_words_len):
            cb[x] = valid_words[x]
        del cb[valid_words_len:cb_len]
EOF

map <Leader>fo3s :python fallout3_hack_sort()<CR>
map <Leader>fo3g :python fallout3_hack_guess()<CR>
