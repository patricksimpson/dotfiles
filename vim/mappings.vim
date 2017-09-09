nnoremap j gj
nnoremap k gk

nnoremap <leader>n :set rnu<CR>
nnoremap <leader>m :set number<CR>
set pastetoggle=<leader>p
" nnoremap <leader>p :set pastetoggle<CR>
nnoremap <leader>o :set nopaste<CR>
nnoremap <leader>r :redraw!<CR>

" NERDTreeToggle
nnoremap <leader>z :NERDTreeToggle<CR>\|<C-w>=

" Toggle spell checking on and off with `,s`
map <silent> <leader>s :set spell!<CR>

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>

"easy indent
nmap <D-[> <<
nmap <D-]> >>
vmap <D-[> <gv
vmap <D-]> >gv

" Move around splits with <c-hjkl>
" Taken care of by tmux-navigator
" nmap <c-j> <c-w>j
" nmap <c-k> <c-w>k
" nmap <c-h> <c-w>h
" nmap <c-l> <c-w>l

map <silent> <F5> :call gruvbox#bg_toggle()<CR>
imap <silent> <F5> <ESC>:call gruvbox#bg_toggle()<CR>a
vmap <silent> <F5> <ESC>:call gruvbox#bg_toggle()<CR>gv

" Y yanks from current cursor position to end of line
nmap Y y$
"
" " Remap Q (annoying Ex mode) to last-used macro
nmap Q @@
vmap Q @@

" Alternate escape
inoremap jk <Esc>

nmap <leader>j :set cursorline<CR>
nmap <leader>k :set cursorline!<CR>
map <leader>, :e#<CR>

" Allow saving of files as sudo when I forgot to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

let g:dbgPavimKeyRun = '<leader>r'
let g:dbgPavimKeyStepOver = '<leader>o'
let g:dbgPavimKeyStepInto = '<leader>i'
let g:dbgPavimKeyStepOut = '<leader>p'
let g:dbgPavimKeyPropertyGet = '<leader>g'
let g:dbgPavimKeyContextGet = '<leader>c'
let g:dbgPavimKeyToggleBp = '<leader>b'

let g:switch_custom_definitions =
  \ [
  \   ['foo', 'bar', 'baz'],
  \   ['light', 'dark'],
  \   ['bottom', 'top'],
  \   ['left', 'right'],
  \   ['one', 'two', 'three'],
  \   ['red', 'green'],
  \   ['width', 'height'],
  \   ['red', 'green', 'blue'],
  \   ['white', 'gray', 'black'],
  \   ['sm', 'md', 'lg'],
  \   ['primary', 'secondary'],
  \   ['block', 'none'],
  \   ['disable', 'enable'],
  \   ['padding', 'margin'],
  \   ['->', '=>']
  \ ]

vmap <C-s>  <Plug>VisualIncrement
nmap <silent> <F5> ggVG"+y

command! -nargs=1 SilentCmd
      \ | execute ':silent !'.<q-args>
      \ | execute ':redraw!'

nnoremap <leader><leader>o :call SystemOpenCurrentFile()<cr>
function! SystemOpenCurrentFile()
  :SilentCmd open %
endfunction

" Vimux
nnoremap <leader>l :call VimuxRunLastCommand()<CR>


nmap <leader>nf :NERDTreeFind<cr>
" Use system clipboard
" set clipboardunnamedplus=

let g:dbgPavimPort = 9009
let g:dbgPavimBreakAtEntry = 0

vnoremap <C-c> "*y

nnoremap <Leader>fr :call VisualFindAndReplace()<CR>
xnoremap <Leader>fr :call VisualFindAndReplaceWithSelection()<CR>

if executable('ag')
    " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
    " HatTip: http://robots.thoughtbot.com/faster-grepping-in-vim and
    " @ethanmuller
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'

    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
  endif


" Move to the end of the line in visual/normal
imap <C-e> <C-o>$
imap <C-a> <C-o>0

let g:syntastic_enable_signs = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': [],
                           \ 'passive_filetypes': [] }
let g:syntastic_html_checkers = ['handlebars']
let g:syntastic_javascript_checkers = ['jshint', 'eslint']
let g:syntastic_aggregate_errors = 1

" Hat tip http://git.io/SPIBfg
let g:syntastic_error_symbol = '✗'
let g:syntastic_warning_symbol = 'w'
let g:syntastic_full_redraws = 1
