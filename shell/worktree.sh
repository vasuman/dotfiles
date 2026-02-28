# === WORKTREES ===
#
# Run .worktree-hooks from source repo in dest
_wt_setup() {
  [[ -f "$1/.worktree-hooks" ]] && (cd "$2" && source "$1/.worktree-hooks")
}

# Get branch name for a directory
_wt_branch() {
  local b=$(git -C "$1" rev-parse --abbrev-ref HEAD 2>/dev/null)
  if [[ "$b" == "HEAD" ]]; then
    echo "\033[33m@$(git -C "$1" rev-parse --short HEAD 2>/dev/null)\033[0m"
  else
    echo "${b:-unknown}"
  fi
}

# ~/.work/{prefix}{num}/{repos}

WT_PREFIX="${WT_PREFIX:-wt-}"
WT_REPOS=("${WT_REPOS[@]}")

wt() {
  local base=~/.work

  case "$1" in
  new)
    local n=0
    while [[ -d "$base/$WT_PREFIX$n" ]]; do ((n++)); done
    local dest="$base/$WT_PREFIX$n"
    mkdir -p "$dest"
    for src in "${WT_REPOS[@]}"; do
      git -C "$src" fetch origin main
      git -C "$src" worktree add "$dest/${src:t}" origin/main --detach && _wt_setup "$src" "$dest/${src:t}"
    done
    cd "$dest"
    ;;
  rm)
    [[ ! "$2" =~ ^[0-9]+$ ]] && {
      echo "Usage: wt rm <num>"
      return 1
    }
    local dest="$base/$WT_PREFIX$2"
    for src in "${WT_REPOS[@]}"; do
      git -C "$src" worktree remove "$dest/${src:t}" 2>/dev/null
    done
    rm -rf "$dest"
    ;;
  [0-9]*)
    [[ -d "$base/$WT_PREFIX$1" ]] && cd "$base/$WT_PREFIX$1" || echo "Not found"
    ;;
  *)
    local here=$(pwd -P)
    printf "\033[1mmain\033[0m\n"
    for src in "${WT_REPOS[@]}"; do
      local mark=" "; [[ "$here" == "$(cd "$src" && pwd -P)"* ]] && mark="*"
      printf "  %s \033[34m%-12s\033[0m %s\n" "$mark" "${src:t}" "$(_wt_branch "$src")"
    done
    for d in "$base"/$WT_PREFIX*(/N); do
      printf "\033[1m${d:t}\033[0m\n"
      for r in "$d"/*(/N); do
        local mark=" "; [[ "$here" == "$r"* ]] && mark="*"
        printf "  %s \033[34m%-12s\033[0m %s\n" "$mark" "${r:t}" "$(_wt_branch "$r")"
      done
    done
    ;;
  esac
}
