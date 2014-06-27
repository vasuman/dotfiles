alias l='ls -l --color=yes'
alias cls='clear'
export EDITOR=vim
export ANDROID_SDK=/opt/android-sdk
export PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin/:$HOME/code/bin/:$ANDROID_SDK/tools/:$ANDROID_SDK/platform-tools/
export PROMPT_COMMAND='prompt_status="$? "; if [[ $prompt_status == "0 " ]]; then prompt_status=; fi; PS1="\[$(tput bold)\]\[$(tput setaf 1)\]$prompt_status\[$(tput setaf 4)\]\u\[$(tput setaf 2)\]@\H \[$(tput setaf 5)\]\W \[$(tput setaf 3)\]\\$ \[$(tput sgr0)\]"'

function set_proxy() {
    export http_proxy="http://localhost:3128/"
    export https_proxy="http://localhost:3128/"
    export ftp_proxy="http://localhost:3128/"
}

function unset_proxy() {
    unset http_proxy
    unset https_proxy
    unset ftp_proxy
}

function serve_dir() {
    python2 -m SimpleHTTPServer $1
}
