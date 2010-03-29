" Vim compiler file for JavaScript using the jslint plugin

if exists("current_compiler")
  finish
endif
let current_compiler = "maven_jslint"

CompilerSet makeprg=mvn\ -Dwdpro-dev\ jslint:jslint
CompilerSet errorformat=%f:%l:%c:\ %m

" vim: nowrap sw=2 sts=2 ts=8 ff=unix:
