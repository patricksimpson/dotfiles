" ===========
" C O L O R S
" ===========

syntax enable             " enable syntax highlighting (previously syntax on).
if has('gui_running')
  set background=light
else
  set background=dark
endif
colorscheme base16-tomorrow
let base16colorspace=256

set t_Co=256

" ===========
" C O N F I G
" ===========

set backspace=2
set nobackup              " no backup files, git hub takes care of this for us...
set nowritebackup         " no write backup
set nu                    " Normal line numbers.
set noswapfile            " Speaks for itself.
set laststatus=2          " last window always has a statusline
set showcmd               " Show incomplete cmds down the bottom
filetype indent on        " activates indenting for files
set nohlsearch            " Don't continue to highlight searched phrases.
set incsearch             " But do highlight as you type your search.
set ignorecase            " Make searches case-insensitive.
set ruler                 " Always show info along bottom.
set autoindent            " auto-indent
set tabstop=2             " tab spacing
set softtabstop=2         " unify
set shiftwidth=2          " indent/outdent by 2 columns
set shiftround            " always indent/outdent to the nearest tabstop
set expandtab             " use spaces instead of tabs
set nowrap                " wrap text
" set hlsearch            " Highlight the last used search pattern

" Leader: , (Set before other things).
let mapleader = ","
noremap \ ,

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Set region to THE UNITIED STATES OF AMERICA!!
set spell spelllang=en_us

highlight clear SpellBad
highlight SpellBad term=standout ctermfg=1 term=undercurl cterm=undercurl
highlight clear SpellCap
highlight SpellCap term=undercurl cterm=undercurl
highlight clear SpellRare
highlight SpellRare term=undercurl cterm=undercurl
highlight clear SpellLocal
highlight SpellLocal term=undercurl cterm=undercurl

"Searching
set ignorecase
set smartcase
set incsearch

"MISC SETTINGS
set listchars=eol:$,tab:>-,trail:~,extends:>,precedes:(

let g:indent_guides_auto_colors = 0

set wildmenu
let g:maplocalleader = ';'

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall

set nrformats=alpha

let g:syntastic_javascript_checkers = ['jshint']
let base16colorspace=256

let g:ackprg = 'ag --nogroup --nocolor --column'
let g:ag_working_path_mode="r"
