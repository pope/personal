setlocal shiftwidth=4
setlocal tabstop=4
setlocal softtabstop=4
setlocal expandtab

setlocal path+=$PWD/**
setlocal includeexpr='Templates/'.substitute(v:fname,'\\.','/','g').'.tea'

compiler tea
