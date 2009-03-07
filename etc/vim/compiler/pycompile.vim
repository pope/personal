" vim compiler file
" Compiler:		pycompile

if exists("current_compiler")
  finish
endif
let current_compiler = "python"

let s:cpo_save = &cpo
set cpo-=C

setlocal makeprg=python\ -c\ \"import\ py_compile;\ py_compile.compile(r'%')\"
setlocal efm=%C\ %.%#,%A\ \ File\ \"%f\"\\,\ line\ %l%.%#,%Z%[%^\ ]%\\@=%m

let &cpo = s:cpo_save
unlet s:cpo_save

"vim: ft=vim
