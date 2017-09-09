" AUTO COMMANDS
" autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=black   ctermbg=black
" autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#111111 ctermbg=235
" autocmd bufenter * :ColorVPreview

" autocmd bufenter * :IndentGuidesEnable
au BufRead,BufNewFile * :IndentGuidesEnable

" CoffeeScript file type
au BufRead,BufNewFile *.coffee set ft=coffee

au BufEnter * set cursorline
au BufNew,BufLeave * set cursorline!

command! W w
command! Q q

" Tmux/Vim workaround for 256 color support.
" if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  " set t_ut=
" endif

" Setting handlebars file type
if has("autocmd")
  au BufNewFile,BufRead *.handlebars,*.hbs set filetype=handlebars
  au BufNewFile,BufRead *.cshtml set filetype=html
endif
let g:ycm_path_to_python_interpreter = '/usr/bin/python'

" Presistent undo
if has('persistent_undo')
  set undofile
  set undodir=~/.vim/.undo
endif

let g:syntastic_javascript_checkers = ['eslint']
