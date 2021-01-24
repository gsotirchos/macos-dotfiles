#!/opt/pkg/bin/bash

#
# ${DOTFILES}/setup.sh
#

# dotfiles path
DOTFILES=$(builtin cd "$(dirname "${BASH_SOURCE[0]}")" > /dev/null && pwd)

# prepare folders
mkdir -vp ~/.vim/undo
mkdir -vp ~/.vim/spell
mkdir -vp ~/.config/lf
mkdir -vp ~/.local/bin

# make soft symlinks
echo "- Symlinking dotfiles"
bin/ln_dotfiles ${DOTFILES}
ln -sfv ${DOTFILES}/bin/*              ~/.bin
ln -sfv ${DOTFILES}/LaunchAgents/com.* ~/Library/LaunchAgents
ln -sfv ${DOTFILES}/vim/*              ~/.vim
ln -sfv ~/.windows_dotfiles/lfrc       ~/.config/lf/lfrc

echo -e "\nDon't forget to load the .plist files!"
