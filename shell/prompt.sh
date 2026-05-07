# prompt (bash). Zsh-only prompt overrides live in ~/.shell/*.zsh.
[[ -n "${BASH_VERSION:-}" ]] && [[ $- == *i* ]] && PS1='\[\e[31m\]\h\[\e[0m\] · \[\e[38;5;117m\]\W\[\e[0m\] \[\e[38;5;183m\]❯\[\e[0m\] '
