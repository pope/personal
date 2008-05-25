" Description: Extends tail.vim with some stuff that I like

" Be Warned...with great power means a great chance that you could have your
" Christmas ruined if you call this function for a file that you didn't want
" erased
"
" TODO: Make this function only available if someone has opened a tail
function tail_extension#Clear ()                  " {{{1
  setlocal modifiable
  silent execute "g/^.*$/d"
  silent w!
  "setlocal nomodifiable
  call tail#Refresh ()
endfunction                              " }}}1

