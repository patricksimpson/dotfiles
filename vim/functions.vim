" Functions
function! HLNext (blinktime)
    highlight HighlightStyle ctermfg=none ctermbg=160 cterm=none
    let [bufnum, lnum, col, off] = getpos('.')
    let matchlen = strlen(matchstr(strpart(getline('.'),col-1),@/))
    let target_pat = '\c\%#'.@/
    let ring = matchadd('HighlightStyle', target_pat, 101)
    redraw
    exec 'sleep ' . float2nr(a:blinktime * 500) . 'm'
    call matchdelete(ring)
    redraw
endfunction

function! VisualFindAndReplace()
    :OverCommandLine%s/
    :w
endfunction
function! VisualFindAndReplaceWithSelection() range
    :'<,'>OverCommandLine s/
    :w
endfunction
