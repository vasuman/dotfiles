alias l='ls -l --color=yes'
alias cls='clear'

alias _='sudo'

alias sys='sudo systemctl'
alias start='sudo systemctl start'
alias stahp='sudo systemctl stop'
alias reinit='sudo systemctl restart'
alias abut='sudo systemctl status'

alias .eb='$EDITOR $HOME/.bashrc && source $HOME/.bashrc'

alias pacs='sudo pacman -S'
alias pacss='pacman -Ss'
alias pacr='sudo pacman -R'

alias ssh_proxy='ssh -L3128:localhost:3128'

export EDITOR=vim
export ANDROID_SDK=/opt/android-sdk
export PROMPT_COMMAND='prompt_status="$? "; if [[ $prompt_status == "0 " ]]; then prompt_status=; fi; PS1="\[$(tput bold)\]\[$(tput setaf 1)\]$prompt_status\[$(tput setaf 4)\]\u\[$(tput setaf 2)\]@\H \[$(tput setaf 5)\]\W \[$(tput setaf 3)\]\\$ \[$(tput sgr0)\]"'
export GOPATH=$HOME/code/go
export PATH=$PATH:$HOME/.gem/ruby/2.1.0/bin/:$HOME/code/bin/:$ANDROID_SDK/tools/:$ANDROID_SDK/platform-tools/:$GOPATH/bin:$HOME/.cabal/bin/:$HOME/.install/go_appengine

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

function pack_textures() {
    java -cp $HOME/packages/gdx.jar:$HOME/packages/gdx-tools.jar com.badlogic.gdx.tools.texturepacker.TexturePacker $1 $2
}

function start_hotspot() {
    _ hotspot.sh wlp3s0 enp4s0f2 $1 $2
}

function github_get() {
    cd $HOME/code/github.com/
    if [[ ! -d $1 ]]; then
        mkdir -p $1
    fi
    cd $1
    git clone https://github.com/$1/$2.git
    cd $2
}
