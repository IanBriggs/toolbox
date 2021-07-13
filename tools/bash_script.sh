#!/bin/bash


# Exit on error
set -e


# Echo commands
#set -x


# Location of bash script
# from https://stackoverflow.com/questions/59895/getting-the-source-directory-of-a-bash-script-from-within
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"


# Catch ctrl-c
# from https://rimuhosting.com/knowledgebase/linux/misc/trapping-ctrl-c-in-bash
trap ctrl_c INT
function ctrl_c() {
    echo "Stopping now"
    # Cleanup seciton
    # Exit
    exit 0
}


# Color printing
# from https://unix.stackexchange.com/questions/9957/how-to-check-if-bash-can-print-colors
# check if stdout is a terminal...
if test -t 1; then

    # see if it supports colors...
    ncolors=$(tput colors)

    if test -n "$ncolors" && test $ncolors -ge 8; then
        bold="$(tput bold)"
        underline="$(tput smul)"
        standout="$(tput smso)"
        normal="$(tput sgr0)"
        black="$(tput setaf 0)"
        red="$(tput setaf 1)"
        green="$(tput setaf 2)"
        yellow="$(tput setaf 3)"
        blue="$(tput setaf 4)"
        magenta="$(tput setaf 5)"
        cyan="$(tput setaf 6)"
        white="$(tput setaf 7)"
    fi
fi
# Usage example: echo "${red}error${normal}"


