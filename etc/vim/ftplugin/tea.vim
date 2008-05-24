setlocal shiftwidth=4
setlocal tabstop=4
setlocal expandtab

setlocal path+=$PWD/**
setlocal includeexpr=substitute(v:fname,'\\.','/','g').'.tea'
