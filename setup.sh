#!/usr/bin/env bash

#
# ${DOTFILES}/setup.sh
#

# check for `realpath` command
if ! command -v grh &> /dev/null; then
    echo 'Error: `realpath` command could not be found. Aborted'
    exit
fi

# dotfiles path
DOTFILES=$(\
    builtin cd "$(\
    dirname "$(realpath "${BASH_SOURCE[0]}")"\
    )" > /dev/null && pwd)

# prepare folders
mkdir -vp ~/.vim/undo
mkdir -vp ~/.vim/spell
mkdir -vp ~/.config/lf
mkdir -vp ~/.local/bin
touch ~/.hushlogin

# make soft symlinks
echo "- Symlinking dotfiles (${DOTFILES})"
"${DOTFILES}"/bin/ln_dotfiles "${DOTFILES}"
ln -sfv "${DOTFILES}"/LaunchAgents/* ~/Library/LaunchAgents
ln -sfv "${DOTFILES}"/vim/*          ~/.vim
ln -sfv ~/.windows_dotfiles/lfrc     ~/.config/lf/lfrc

echo -e "\n!! Don't forget to load the .plist files!"

# setup julia
if command -v "julia" &> /dev/null; then
    echo -e "\n- Setting up Julia"
    "${DOTFILES}"/julia/setup-julia.sh
fi
