syn include @gitcommitDiff syntax/diff.vim
syn region gitcommitDiff start=/\%(^diff --git \)\@=/ end=/^$\|^#\@=/ contains=@gitcommitDiff

syn region gitLine start=/^#/ end=/$/
syn region gitCommit start=/^# Changes to be committed:$/ end=/^#$/ contains=gitHead,gitCommitFile,gitcommitDiff
syn region gitHead contained start=/^#   (.*)/ end=/^#$/
syn region gitChanged start=/^# Changed but not updated:/ end=/^#$/ contains=gitHead,gitChangedFile,gitcommitDiff
syn region gitUntracked start=/^# Untracked files:/ end=/^#$/ contains=gitHead,gitUntrackedFile,gitcommitDiff

syn match gitCommitFile contained /^#\t.*/hs=s+2
syn match gitChangedFile contained /^#\t.*/hs=s+2
syn match gitUntrackedFile contained /^#\t.*/hs=s+2

hi def link gitLine Comment
hi def link gitCommit Comment
hi def link gitChanged Comment
hi def link gitHead Comment
hi def link gitUntracked Comment
hi def link gitCommitFile Type
hi def link gitChangedFile Constant
hi def link gitUntrackedFile Constant
