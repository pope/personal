" Vim syntax file for Tea
" Language:         Tea
" Maintainer:       K. Adam Christensen
" Latest Revision:  2008-05-22

if exists("b:current_syntax")
    finish
endif

if !exists("main_syntax")
    let main_syntax = 'tea'
endif

runtime! syntax/html.vim
unlet b:current_syntax

syn case match

syn keyword teaStorageClasses template contained
syn keyword teaSupportFunctions isNotEmptyOrNull isEmptyOrNull call dynamicTemplateCall logDebug logWarn logError logInfo contained
syn keyword teaControlKeywords if else foreach in break define import contained
syn keyword teaLogicalOperatorKeywords and or isa not contained
syn keyword teaLanguageConstants true false null contained
syn match teaNumericaConstants /\b(0[xX]\h(?>_?\h)*|\d(?>_?\d)*(\.(?![^[:space:][:digit:]])(?>_?\d)*)?([eE][-+]?\d(?>_?\d)*)?|0[bB][01]+)\b/ contained
syn match teaVariables /\(b\|obj\|list\|map\|int\|dte\|dt\|dto\|str\|arr\)[A-Z0-9][a-zA-Z0-9]*/ contained

syn region teaStringWithDoubleQuotes start=/"/ skip=/\\"/ end=/"/ contained
syn region teaStringWithSingleQuotes start=/'/ skip=/\\'/ end=/'/ contained
syn cluster teaStrings contains=teaStringWithDoubleQuotes,teaStringWithSingleQuotes

syn region teaBlockComments start=/\/\*/ end=/\*\// contained
syn match teaLineComments /\/\/.*$/ contained
syn cluster teaComments contains=teaBlockComments,teaLineComments

syn region teaCode matchgroup=teaDelimiter start="<%" end="%>"  contains=@teaComments,teaStorageClasses,teaSupportFunctions,@teaStrings,teaControlKeywords,teaLogicalOperatorKeywords,teaNumericaConstants,teaLanguageConstants,teaVariables

syn cluster htmlPreproc add=teaCode

hi def link teaSupportFunctions            Function
hi def link teaStorageClasses              Function
hi def link teaControlKeywords             Conditional
hi def link teaLogicalOperatorKeywords     Operator
hi def link teaLanguageConstants           Boolean
hi def link teaBlockComments               Comment
hi def link teaLineComments                Comment
hi def link teaStringWithDoubleQuotes      String
hi def link teaStringWithSingleQuotes      String
hi def link teaNumericaConstants           Number
hi def link teaVariables                   Identifier
hi def link teaDelimiter                   Delimiter

" vim: set ai et sw=4 :
