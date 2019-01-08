#Some useful functions

# npm install and save dev
function ni() {
  npm install --save-dev "$@"
}
# npm install grunt package, and save dev.
function gi() {
  npm install --save-dev grunt-"$@"
}

# npm install grunt contrib package, and save dev.
function gci() {
  npm install --save-dev grunt-contrib-"$@"
}


# Simple calculator
function calc() {
  local result=""
  result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
  #                       └─ default (when `--mathlib` is used) is 20
  #
  if [[ "$result" == *.* ]]; then
    # improve the output for decimal numbers
    printf "$result" |
    sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
        -e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
        -e 's/0*$//;s/\.$//'   # remove trailing zeros
  else
    printf "$result"
  fi
  printf "\n"
}

# Create a new directory and enter it
function md() {
  mkdir -p "$@" && cd "$@"
}

# Determine size of a file or total size of a directory
function fs() {
  if du -b /dev/null > /dev/null 2>&1; then
    local arg=-sbh
  else
    local arg=-sh
  fi
  if [[ -n "$@" ]]; then
    du $arg -- "$@"
  else
    du $arg .[^.]* *
  fi
}

function extract-text {
    FILEPATH=$1
    convert $1 -resize 400% -type Grayscale $1.tif
    tesseract -l eng $1.tif output
}