set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
set shell=/bin/bash

call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-eunuch'
Plugin 'bling/vim-airline'
Plugin 'chrisbra/SudoEdit.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'fatih/vim-go'
Plugin 'mattn/emmet-vim'
Plugin 'vimwiki/vimwiki'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/syntastic'
Plugin 'tikhomirov/vim-glsl'
Plugin 'dart-lang/dart-vim-plugin'

if filereadable(glob("~/.vimrc.plugins"))
    source ~/.vimrc.plugins
endif

call vundle#end()

filetype plugin indent on
syntax on
colorscheme desert

set encoding=utf-8
set shortmess=I

set nowrap
set relativenumber

set laststatus=2
set ttimeoutlen=50

set autoindent
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set guioptions-=T
set guioptions-=m
set guifont=

set autoread

" clean up swap files
set backupdir=~/.backup,/tmp,.
set directory=~/.backup,/tmp,.

" window movement
nmap <silent> <A-Up> :wincmd k<CR>
nmap <silent> <A-Down> :wincmd j<CR>
nmap <silent> <A-Left> :wincmd h<CR>
nmap <silent> <A-Right> :wincmd l<CR>

" strip trailing whitespace
au BufWritePre * :%s/\s\+$//e

au FileType vimwiki setl tw=80
au FileType * setl formatoptions-=o

" set filetype options
au BufRead,BufNewFile *.json setl ft=javascript
au BufRead,BufNewFile *.sol setl ft=javascript
au BufRead,BufNewFile *.md setl ft=markdown tw=80
au BufRead,BufNewFile *.js,*.jsx setl sw=2 sts=2 et

" autoformat hooks
au BufWritePost *.dart DartFmt

command! MakeDir !mkdir -p $(dirname %)
command! InsertDate r! date +"\%d-\%m-\%Y"

nnoremap <F2> :w<cr>
nnoremap <F3> :SyntasticToggle<cr>
nnoremap <F4> :NERDTreeToggle<cr>
nnoremap <F5> :Gstatus<cr>

let NERDTreeShowBookmarks = 1

let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|\.git'

let g:vimwiki_list = [ { 'path': '~/wiki/', 'syntax': 'markdown', 'ext': '.vmd' } ]

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_javascript_checkers = [ 'eslint' ]
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [], 'passive_filetypes': [] }

let g:go_fmt_command = "goimports"

if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
