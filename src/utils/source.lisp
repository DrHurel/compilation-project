
(tagbody ;;cond equivalent
    (when 'cond 'expr (go 'end))
    (when 'cond 'expr (go 'end))
    (when 'cond 'expr (go 'end))
    (when 'cond 'expr (go 'end))
    (when 'cond 'expr (go 'end))
    end
)


(loop while condition do (body))
(tagbody ;;loop equivalent
    before
    (when 
        condition 
        (body) 
        (go before)
    )
)