" Vim syntax file
" Language:         Tea
" Maintainer:       K. Adam Christensen <adam.christensen@dig.com>
" URL:              http://happygiraffe.net/blog/archives/2006/01/25/vim-syntax-for-textile
" Latest Revision:  2006-01-25

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

if !exists("main_syntax")
  let main_syntax = 'tea'
endif

if version < 600
  so <sfile>:p:h/html.vim
else
  runtime! syntax/html.vim
  unlet b:current_syntax
endif

syn case match

syn keyword teaStorageClasses template
syn keyword teaSupportFunctions isNotEmptyOrNull isEmptyOrNull call dynamicTemplateCall
syn keyword teaControlKeywords if else foreach in break
syn keyword teaLogicalOperatorKeywords and or isa not
syn keyword teaLanguageConstants true false null
syn match teaNumericaConstants /\b(0[xX]\h(?>_?\h)*|\d(?>_?\d)*(\.(?![^[:space:][:digit:]])(?>_?\d)*)?([eE][-+]?\d(?>_?\d)*)?|0[bB][01]+)\b/
syn match teaVariables /(b|obj|list|map|int|dte|dto|str)[A-Z0-9][a-zA-Z0-9]*/

syn region teaStringWithDoubleQuotes start=/"/ skip=/\\"/ end=/"/
syn region teaStringWithSingleQuotes start=/'/ skip=/\\'/ end=/'/
syn cluster teaStrings contains=teaStringWithDoubleQuotes,teaStringWithSingleQuotes

syn region teaBlockComments start=/\/\*/ end=/\*\//
syn match teaLineComments /\/\/.*$/
syn cluster teaComments contains=teaBlockComments,teaLineComments

syn region teaCode matchgroup=teaDelimiter start="<%" end="%>"  contains=@teaComments,teaStorageClasses,teaSupportFunctions,@teaStrings,teaControlKeywords,teaLogicalOperatorKeywords,teaNumericaConstants,teaLanguageConstants,teaVariables

if version >= 508 || !exists("did_tea_syn_inits")
    if version < 508
        let did_tea_syn_inits = 1
        command -nargs=+ HiLink hi link <args>
    else
        command -nargs=+ HiLink hi def link <args>
    endif

    HiLink teaSupportFunctions            Function
    HiLink teaStorageClasses              Function
    HiLink teaControlKeywords             Conditional
    HiLink teaLogicalOperatorKeywords     Operator
    HiLink teaLanguageConstants           Boolean
    HiLink teaBlockComments               Comment
    HiLink teaLineComments                Comment
    HiLink teaStringWithDoubleQuotes      String
    HiLink teaStringWithSingleQuotes      String
    HiLink teaNumericaConstants           Number
    HiLink teaVariables                   Identifier
    HiLink teaDelimiter                   Delimiter

    delcommand HiLink
endif

" vim: set ai et sw=4 :
