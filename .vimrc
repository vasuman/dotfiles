set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle

call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'bling/vim-airline'
Plugin 'chrisbra/SudoEdit.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'fatih/vim-go'

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

