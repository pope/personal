"syn match typedefed /\w\+_t/ contained
"syn match typedefedCE /\W\+\w\+_t$/ contains=typedefed
"syn match typedefedCS /^\w\+_t\W\+/ contains=typedefed
"syn match typedefedCW /^\w\+_t$/ contains=typedefed
"syn match typedefedCM /[ *;]\w\+_t[ *;]/ contains=typedefed
"hi def link typedefed Type
