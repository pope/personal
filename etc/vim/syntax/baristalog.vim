syn match baristalogWarn /^*W[^>]*>/
syn match baristalogError /^*E[^>]*>/
syn match baristalogDebug /^ D[^>]*>/
syn match baristalogInfo /^ I[^>]*>/

syn match baristalogBizRuleInput /\[Input Rule:.*\];/
syn match baristalogBizRuleOutput /\[Ouput Rule:.*\];/

if &background == "dark"
    highlight baristalogDebug ctermfg=darkgrey guifg=darkgrey
    highlight baristalogInfo ctermfg=blue guifg=#96CBFE
    highlight baristalogWarn ctermfg=yellow guifg=#FFFFB6
    highlight baristalogError ctermfg=red guifg=#FF6C60
    highlight baristalogBizRuleOutput ctermfg=green guifg=#A8FF60
    highlight baristalogBizRuleInput ctermfg=green guifg=#CEFFAB
else
    highlight baristalogDebug ctermfg=lightgrey guifg=#888a85
    highlight baristalogInfo ctermfg=blue guifg=#3465ae
    highlight baristalogWarn ctermfg=yellow guifg=#f57900
    highlight baristalogError ctermfg=red guifg=#a40000
    highlight baristalogBizRuleOutput ctermfg=green guifg=#73d216
    highlight baristalogBizRuleInput ctermfg=green guifg=#459a06
endif
