set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle

call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'bling/vim-airline'
Plugin 'chrisbra/SudoEdit.vim'

call vundle#end()

filetype plugin indent on
syntax on

set shortmess=I
set incsearch
set showmatch
set nowrap
set number
set laststatus=2
set ttimeoutlen=50

set autoindent
set smartindent     
set tabstop=4
set softtabstop=4   
set shiftwidth=4    
set smarttab
set expandtab       

set guioptions-=T
set guioptions-=m
set encoding=utf-8
