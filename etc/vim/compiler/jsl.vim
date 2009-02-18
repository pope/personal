" Vim compiler file for JavaScript using jsl

if exists("current_compiler")
  finish
endif
let current_compiler = "jsl"

if !exists("g:jsl_conf")
  let g:jsl_conf = $HOME . "/.jsl.conf"
endif

exec "CompilerSet makeprg=jsl\\ -nologo\\ -nofilelisting\\ -nosummary\\ -nocontext\\ -conf\\ " . g:jsl_conf . "\\ -process\\ %"

CompilerSet errorformat=%f(%l):\ %m

" vim: nowrap sw=2 sts=2 ts=8 ff=unix:
