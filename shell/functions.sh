# Run command with .env loaded
erun() {
  set -a
  source "${1:-.env}" >/dev/null 2>&1
  set +a
  shift
  "$@"
}
