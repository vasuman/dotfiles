#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

link() {
  local src="$SCRIPT_DIR/$1"
  local dest="$2"

  if [[ -L "$dest" ]]; then
    echo "skip: $dest (already linked)"
  elif [[ -e "$dest" ]]; then
    echo "WARN: $dest exists, skipping (back it up first)"
  else
    mkdir -p "$(dirname "$dest")"
    ln -s "$src" "$dest"
    echo "linked: $dest"
  fi
}

source_shell_files() {
  local marker="# dotfiles: source ~/.shell (install.sh)"

  if ! grep -qF "$marker" "${HOME}/.zshrc" 2>/dev/null; then
    cat >>"${HOME}/.zshrc" <<'EOF'

# dotfiles: source ~/.shell (install.sh)
if [[ -d "${HOME}/.shell" ]]; then
  for _dotfiles_shell_file in "${HOME}/.shell"/*.sh(N); do
    source "$_dotfiles_shell_file"
  done
  unset _dotfiles_shell_file
fi
EOF
    echo "updated: ${HOME}/.zshrc"
  fi

  if ! grep -qF "$marker" "${HOME}/.bashrc" 2>/dev/null; then
    cat >>"${HOME}/.bashrc" <<'EOF'

# dotfiles: source ~/.shell (install.sh)
if [[ -d "${HOME}/.shell" ]]; then
  for _dotfiles_shell_file in "${HOME}/.shell"/*.sh; do
    [[ -e "$_dotfiles_shell_file" ]] || continue
    source "$_dotfiles_shell_file"
  done
  unset _dotfiles_shell_file
fi
EOF
    echo "updated: ${HOME}/.bashrc"
  fi
}

link shell ~/.shell
link ghostty ~/.config/ghostty
link gitconfig ~/.gitconfig
link helix ~/.config/helix
link worktrunk ~/.config/worktrunk

source_shell_files
