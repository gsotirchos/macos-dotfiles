#
# ~/.conda/conda_init.sh
#

case "$(uname -s)" in
    Linux*)
        __conda_path="/opt/miniforge" # NOTE: custom location
        ;;
    Darwin*)
        __conda_path="${HOMEBREW_PREFIX}/Caskroom/miniforge/base"
        ;;
esac

if __conda_setup="$("${__conda_path}/bin/conda" "shell.bash" "hook" 2> /dev/null)"; then
    eval "${__conda_setup}"
else
    if [ -f "${__conda_path}/etc/profile.d/conda.sh" ]; then
        source "${__conda_path}/etc/profile.d/conda.sh"
    elif [[ -d "${__conda_path}/bin" ]]; then
        export PATH="${__conda_path}/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "${__conda_path}/etc/profile.d/mamba.sh" ]; then
    source "${__conda_path}/etc/profile.d/mamba.sh"
fi
unset __conda_path

export MAMBA_NO_BANNER=1
