syn match baristalogWarn /^*W[^>]*>/
syn match baristalogError /^*E[^>]*>/
syn match baristalogDebug /^ D[^>]*>/
syn match baristalogInfo /^ I[^>]*>/

syn match baristalogBizRuleInput /\[Input Rule:.*\];/
syn match baristalogBizRuleOutput /\[Ouput Rule:.*\];/

highlight baristalogDebug ctermfg=darkgrey guifg=darkgrey
highlight baristalogInfo ctermfg=blue guifg=#96CBFE
highlight baristalogWarn ctermfg=yellow guifg=#FFFFB6
highlight baristalogError ctermfg=red guifg=#FF6C60
highlight baristalogBizRuleOutput ctermfg=green guifg=#A8FF60
highlight baristalogBizRuleInput ctermfg=green guifg=#CEFFAB
