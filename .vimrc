set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
set shell=/bin/bash

call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'chrisbra/SudoEdit.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'mattn/emmet-vim'
Plugin 'sheerun/vim-polyglot'

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

" swap file directory
set backupdir=~/.backup,/tmp,.
set directory=~/.backup,/tmp,.

" window movement
nmap <silent> <A-Up> :wincmd k<CR>
nmap <silent> <A-Down> :wincmd j<CR>
nmap <silent> <A-Left> :wincmd h<CR>
nmap <silent> <A-Right> :wincmd l<CR>

" strip trailing whitespace
au BufWritePre * :%s/\s\+$//e

" set filetype options
au BufRead,BufNewFile *.json setl ft=javascript
au BufRead,BufNewFile *.sol setl ft=javascript
au BufRead,BufNewFile *.md setl ft=markdown tw=80
au BufRead,BufNewFile *.js,*.jsx setl sw=2 sts=2 et
au BufRead,BufNewFile *.dart setl sw=2 sts=2 et

" misc autoformat hooks
au FileType * setl formatoptions-=o
au BufWritePost *.dart DartFmt

" Custom commands
command! InsertDate r! date +"\%d-\%m-\%Y"

nnoremap <F3> :Gstatus<cr>

let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|\.git'
let g:go_fmt_command = "goimports"

" source local overrides
if filereadable(glob("~/.vimrc.local"))
    source ~/.vimrc.local
endif
