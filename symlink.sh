#! /bin/bash

install_list=(".zshrc" ".tmux.conf" ".emacs.d/init.el")

for item in ${install_list[@]}
do
    if [ ! -e $HOME/$item ]; then
	echo "symlink "$item
        ln -s $HOME/.dotfiles/$item $HOME/$item
    fi
done
