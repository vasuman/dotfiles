#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

LOADER_BEGIN="# dotfiles: source ~/.shell"
LOADER_END="# dotfiles: end source ~/.shell"

usage() {
  cat <<EOF
Usage: $0 [install|force|uninstall]

  install    Link files and add shell loader blocks if missing (default)
  force      Remove dotfiles-managed links/loaders, then install fresh
  uninstall  Remove dotfiles-managed links/loaders
EOF
}

link() {
  local src="$SCRIPT_DIR/$1"
  local dest="$2"

  if [[ -L "$dest" ]]; then
    if [[ "$(readlink "$dest")" == "$src" ]]; then
      echo "skip: $dest (already linked)"
    elif [[ "${FORCE:-0}" == 1 ]]; then
      local backup="${dest}.bak.$(date +%Y%m%d%H%M%S)"
      mv "$dest" "$backup"
      mkdir -p "$(dirname "$dest")"
      ln -s "$src" "$dest"
      echo "replaced: $dest (backup: $backup)"
    else
      echo "WARN: $dest is a symlink to something else, skipping"
    fi
  elif [[ -e "$dest" ]]; then
    if [[ "${FORCE:-0}" == 1 ]]; then
      local backup="${dest}.bak.$(date +%Y%m%d%H%M%S)"
      mv "$dest" "$backup"
      mkdir -p "$(dirname "$dest")"
      ln -s "$src" "$dest"
      echo "replaced: $dest (backup: $backup)"
    else
      echo "WARN: $dest exists, skipping (run '$0 force' to replace it with a backup)"
    fi
  else
    mkdir -p "$(dirname "$dest")"
    ln -s "$src" "$dest"
    echo "linked: $dest"
  fi
}

unlink_if_managed() {
  local src="$SCRIPT_DIR/$1"
  local dest="$2"

  if [[ -L "$dest" && "$(readlink "$dest")" == "$src" ]]; then
    rm "$dest"
    echo "removed: $dest"
  else
    echo "skip: $dest (not a dotfiles-managed symlink)"
  fi
}

remove_loader_block() {
  local rc_file="$1"
  [[ -f "$rc_file" ]] || return 0

  local tmp
  tmp="$(mktemp)"

  awk \
    -v begin="$LOADER_BEGIN" \
    -v end="$LOADER_END" \
    '
      $0 == begin { skip=1; next }
      $0 == end { skip=0; next }
      !skip { print }
    ' "$rc_file" >"$tmp"

  mv "$tmp" "$rc_file"
}

add_bash_loader() {
  local rc_file="${HOME}/.bashrc"
  grep -qF "$LOADER_BEGIN" "$rc_file" 2>/dev/null && {
    echo "skip: $rc_file (loader already installed)"
    return
  }

  cat >>"$rc_file" <<EOF

$LOADER_BEGIN
if [[ -d "\${HOME}/.shell" ]]; then
  for _dotfiles_shell_file in "\${HOME}/.shell"/*.sh; do
    [[ -e "\$_dotfiles_shell_file" ]] || continue
    source "\$_dotfiles_shell_file"
  done
  unset _dotfiles_shell_file
fi
$LOADER_END
EOF
  echo "updated: $rc_file"
}

add_zsh_loader() {
  local rc_file="${HOME}/.zshrc"
  grep -qF "$LOADER_BEGIN" "$rc_file" 2>/dev/null && {
    echo "skip: $rc_file (loader already installed)"
    return
  }

  cat >>"$rc_file" <<EOF

$LOADER_BEGIN
if [[ -d "\${HOME}/.shell" ]]; then
  for _dotfiles_shell_file in "\${HOME}/.shell"/*.sh(N); do
    source "\$_dotfiles_shell_file"
  done

  for _dotfiles_shell_file in "\${HOME}/.shell"/*.zsh(N); do
    source "\$_dotfiles_shell_file"
  done
  unset _dotfiles_shell_file
fi
$LOADER_END
EOF
  echo "updated: $rc_file"
}

install_links() {
  link shell "${HOME}/.shell"
  link ghostty "${HOME}/.config/ghostty"
  link gitconfig "${HOME}/.gitconfig"
  link helix "${HOME}/.config/helix"
  link worktrunk "${HOME}/.config/worktrunk"
}

uninstall_links() {
  unlink_if_managed shell "${HOME}/.shell"
  unlink_if_managed ghostty "${HOME}/.config/ghostty"
  unlink_if_managed gitconfig "${HOME}/.gitconfig"
  unlink_if_managed helix "${HOME}/.config/helix"
  unlink_if_managed worktrunk "${HOME}/.config/worktrunk"
}

install_shell_loaders() {
  add_zsh_loader
  add_bash_loader
}

uninstall_shell_loaders() {
  remove_loader_block "${HOME}/.zshrc"
  remove_loader_block "${HOME}/.bashrc"
  echo "removed: shell loader blocks"
}

mode="${1:-install}"

case "$mode" in
  install)
    install_links
    install_shell_loaders
    ;;
  force)
    FORCE=1
    uninstall_links
    uninstall_shell_loaders
    install_links
    install_shell_loaders
    ;;
  uninstall)
    uninstall_links
    uninstall_shell_loaders
    ;;
  -h|--help|help)
    usage
    ;;
  *)
    usage >&2
    exit 1
    ;;
esac
