set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
set shell=/bin/bash

call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'tpope/vim-surround'
Plugin 'bling/vim-airline'
Plugin 'chrisbra/SudoEdit.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'fatih/vim-go'
Plugin 'mattn/emmet-vim'
Plugin 'vimwiki/vimwiki'
Plugin 'scrooloose/nerdtree'
Plugin 'tpope/vim-fugitive'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'scrooloose/syntastic'

if filereadable(glob("~/.vimrc.plugins"))
    source ~/.vimrc.plugins
endif

call vundle#end()

filetype plugin indent on
syntax on

set encoding=utf-8
set shortmess=I

set nowrap
set number

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

" strip trailing whitespace
au BufWritePre * :%s/\s\+$//e

au FileType vimwiki setl tw=80
au FileType * setl formatoptions-=o

au BufRead,BufNewFile *.json setl ft=javascript
au BufRead,BufNewFile *.sol setl ft=javascript
au BufRead,BufNewFile *.md setl ft=markdown tw=80
au BufRead,BufNewFile *.js,*.jsx setl sw=2 sts=2 et

cabbrev WriteHook au BufWritePost * exec

command! MakeDir !mkdir -p $(dirname %)
command! InsertDate r! date +"\%d-\%m-\%Y"

nnoremap <F2> :Gstatus<cr>
nnoremap <F3> :NERDTreeToggle<cr>
nnoremap <F3> :SyntasticCheck<cr>

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

if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
