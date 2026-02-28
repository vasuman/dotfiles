# Git aliases
alias gs='git status'
alias gco='git commit'
alias gl='git log --oneline -20'
alias gp='git push'
alias gpu='git pull'
alias gpo='git push -u origin HEAD'
alias gpom='git pull origin main --recurse-submodules'
alias gcam='git commit -am'
alias gcnv='git commit --no-verify -am'

# GitHub aliases
alias ghpr='gh pr create'
alias ghprf='gh pr create --fill'

# Checkout from arg; or fuzzy
gc() {
  if [[ -z "$1" ]]; then
    local branch=$(git branch -a --sort=-committerdate --format='%(refname:short)' | sed 's|^origin/||' | awk '!seen[$0]++' | fzf)
    [[ -n "$branch" ]] && git checkout "$branch"
  else
    git checkout "$@"
  fi
}

# Create new branch from origin/main
gnb() {
  git fetch origin main
  git checkout -b varav/$1 origin/main --no-track
}

