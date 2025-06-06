# shellcheck disable=SC1090

#
# ~/.bash_profile
#

# if not running interactively, don't do anything
case $- in
    *i*)
        ;;
    *)
        return
        ;;
esac

# homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# dark theme toggle (run in background)
#(source ~/.dotfiles/etc/toggle_dark_mode.sh &) &> /dev/null

# source bashrc
if [[ -f ~/.bashrc ]]; then
    source ~/.bashrc
fi

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniforge/base/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniforge/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

