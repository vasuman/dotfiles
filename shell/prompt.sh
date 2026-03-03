# prompt
autoload -Uz vcs_info
setopt prompt_subst
zstyle ':vcs_info:*' formats ' (%b)'
zstyle ':vcs_info:*' actionformats ' (%b|%a)'
precmd() { vcs_info }
PROMPT='%F{117}%1~%f%F{114}${vcs_info_msg_0_}%f %F{183}❯%f '
