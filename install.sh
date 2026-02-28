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

link shell ~/.shell
link ghostty ~/.config/ghostty
link gitconfig ~/.gitconfig
link helix ~/.config/helix
