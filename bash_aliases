# vim: set ft=sh:

# rm wrapper to move file(s) to trash folder
trash() {
    flags=""
    for arg in "$@"; do
        if [[ ${arg} == -* ]]; then
            flags="${flags} ${arg}"
            continue
        fi

        # move file to trash folder, or delete /tmp/* files
        if [[ ${arg} == "/tmp/"* ]]; then
            #echo "unlink ${arg}"
            env rm -rf "${arg}"
        else
            #echo "mv -v ${flags} ${arg} ${TRASH}"
            env mv ${flags} -v --backup=numbered "${arg}" "${TRASH}"
        fi
    done
}

# function to empty trash
empty_trash() {
    read -p "Empty trash? [y/N] " -r
    if [[ ${REPLY} =~ ^[Yy]$ ]]; then
        for file in "${TRASH}"/*; do
            if [[ -e "${file}" ]]; then
                env rm -rf "${file}" && \
                echo "Deleted: ${file}"
            fi
        done
    fi
}

# aliases
alias rm=trash  # trash file instead of deleting
alias mv="mv -iv"  # confirmatory, verbose move
alias cp="cp -iv"  # confirmatory, verbose copy
alias ln="ln -iv"  # confirmatory, verbose symlink creaton
alias ls="ls --color"  # colors in ls
alias tree="tree -lNFC -L 2 \
    --dirsfirst --filelimit 15 \
    -I '.DS_Store|.localized|._*' --matchdirs"  # cleaner tree
alias sftp="with-readline sftp"
alias dunnet="clear && emacs -batch -l dunnet"
alias pkg_list="pkg_info -u | sed 's/\(.*\)-[0-9].*/\1/g'"
alias vimrc="vim ~/.vim/vimrc"
alias py="python3"
alias pip-upgrade='conda upgrade --all && conda list | grep "pypi" | cut -d " " -f 1 | xargs pip install --upgrade'
alias drl="conda activate drl"
