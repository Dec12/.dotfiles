#! /bin/bash

dir_list=(".zsh.d" ".emacs.d")

for item in ${dir_list[@]}
do
    if [ ! -e $HOME/$item ]; then
	echo "make dir "$item
        mkdir -p $HOME/$item
    fi
done
