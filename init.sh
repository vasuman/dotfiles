#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ln -s  $DIR/bashrc      $HOME/.bashrc && echo "!bashrc"
ln -s  $DIR/vimrc       $HOME/.vimrc && echo "!vimrc"
ln -sT $DIR/vim         $HOME/.vim && echo "!vim"
ln -s  $DIR/tmux.conf   $HOME/.tmux.conf && echo "!tmux"
ln -sT $DIR/emacs        $HOME/.emacs.d && echo "!emacs"
