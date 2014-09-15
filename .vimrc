filetype off
set rtp+=~/.vim/vundle
call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'bling/vim-airline'
Plugin 'Raimondi/delimitMate'
Plugin 'myusuf3/numbers.vim'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'tpope/vim-surround'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'sjl/gundo.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'pangloss/vim-javascript'
Plugin 'tpope/vim-markdown'
Plugin 'guns/vim-clojure-static'
Plugin 'majutsushi/tagbar'
Plugin 'chrisbra/SudoEdit.vim'
Plugin 'ratazzi/blackboard.vim'
Plugin 'tpope/vim-fireplace'
Plugin 'briancollins/vim-jst'
Plugin 'mattn/emmet-vim'

call vundle#end()

filetype plugin indent on
syntax on
colorscheme darkblue

set guioptions-=T
set guioptions-=m
set guifont=terminus\ 11
set encoding=utf-8

set incsearch
set ignorecase
set smartcase
set nocompatible
set nofoldenable
set confirm
set t_Co=256
set number
set showmatch
set showcmd
set title
set laststatus=2
set matchtime=2
set nowrap
set linebreak
set nolist
set autoindent
set smartindent     
set tabstop=4
set softtabstop=4   
set shiftwidth=4    
set smarttab
set expandtab       

" easy-motion
let g:EasyMotion_leader_key = ','

" NerdTree
let NERDChristmasTree=1
let NERDTreeWinSize=30
let NERDTreeChDirMode=2
let NERDTreeIgnore=['\~$', '\.pyc$', '\.swp$']
let NERDTreeWinPos = "right"

" NerdCommenter
let NERDSpaceDelims=1
let NERDCompactSexyComs=1

" CtrlP
set wildignore+=*/tmp/*,*.so,*.o,*.a,*.obj,*.swp,*.zip,*.pyc,*.pyo,*.class  " MacOSX/Linux
let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$\|node_modules'
let g:ctrlp_extensions = ['line']
let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")' : ['<c-t>'],
    \ 'AcceptSelection("t")' : ['<CR>'],
    \ }
let g:ctrlp_clear_cache_on_exit = 1

" Keybindings
nmap <F5> :TagbarToggle<cr>
nmap <F6> :NERDTreeToggle<cr>
nmap <F3> :GundoToggle<cr>
nmap <F4> :IndentGuidesToggle<cr>
nmap <F7> :NumbersToggle<cr>
nmap <silent> <A-Up> :wincmd k<CR>
nmap <silent> <A-Down> :wincmd j<CR>
nmap <silent> <A-Left> :wincmd h<CR>
nmap <silent> <A-Right> :wincmd l<CR>

nmap <A-k> :tabnext<CR>
nmap <A-j> :tabprev<CR>

cabbrev nt tabnew
cabbrev date r! date +\%a\ \%d\-\%m\-\%Y\ \%R


autocmd BufReadPost *
      \ if ! exists("g:leave_my_cursor_position_alone") |
      \     if line("'\"") > 0 && line ("'\"") <= line("$") |
      \         exe "normal g'\"" |
      \     endif |
      \ endif


function! MoveToPrevTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() != 1
    close!
    if l:tab_nr == tabpagenr('$')
      tabprev
    endif
    sp
  else
    close!
    exe "0tabnew"
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

function! MoveToNextTab()
  "there is only one window
  if tabpagenr('$') == 1 && winnr('$') == 1
    return
  endif
  "preparing new window
  let l:tab_nr = tabpagenr('$')
  let l:cur_buf = bufnr('%')
  if tabpagenr() < tab_nr
    close!
    if l:tab_nr == tabpagenr('$')
      tabnext
    endif
    sp
  else
    close!
    tabnew
  endif
  "opening current buffer in new window
  exe "b".l:cur_buf
endfunc

nnoremap <A-.> :call MoveToNextTab()<CR><C-w>L
nnoremap <A-,> :call MoveToPrevTab()<CR><C-w>H
nnoremap <silent><A-o> :set paste<CR>o<Esc>:set nopaste<CR>
nnoremap <silent><A-O> :set paste<CR>O<Esc>:set nopaste<CR>

" Rainbow parenthesis
let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0
au VimEnter * RainbowParenthesesToggle

" Clojure
au FileType clojure call ConfigClojure()

function! ConfigClojure()
    let b:delimitMate_quotes = "\""
    RainbowParenthesesLoadRound
    RainbowParenthesesLoadSquare
endfunc

" Markdown
au FileType text,markdown,json set wrap
set t_vb=
set mouse=
