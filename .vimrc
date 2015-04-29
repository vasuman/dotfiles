set nocompatible
filetype off
set rtp+=~/.vim/vundle
call vundle#begin()

Plugin 'gmarik/vundle'
Plugin 'bling/vim-airline'
"Plugin 'Raimondi/delimitMate'
"Plugin 'myusuf3/numbers.vim'
"Plugin 'Lokaltog/vim-easymotion'
"Plugin 'tpope/vim-surround'
"Plugin 'kien/rainbow_parentheses.vim'
"Plugin 'scrooloose/nerdcommenter'
"Plugin 'sjl/gundo.vim'
"Plugin 'nathanaelkane/vim-indent-guides'
"Plugin 'scrooloose/nerdtree'
"Plugin 'kien/ctrlp.vim'
"Plugin 'tpope/vim-fugitive'
"Plugin 'pangloss/vim-javascript'
"Plugin 'tpope/vim-markdown'
"Plugin 'guns/vim-clojure-static'
"Plugin 'majutsushi/tagbar'
"Plugin 'chrisbra/SudoEdit.vim'
"Plugin 'ratazzi/blackboard.vim'
"Plugin 'tpope/vim-fireplace'
"Plugin 'briancollins/vim-jst'
"Plugin 'mattn/emmet-vim'

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
