" OpenDefect() {{{1
" Open up the Jira defect
function! OpenDefect()
	!open "http://jira.wdpro.wdig.com/browse/<cword>"
    echo "Cheers!"
endfunction
" }}}1
" mappings {{{1
map <buffer> <localleader>co :call OpenDefect()<cr>
" }}}1
