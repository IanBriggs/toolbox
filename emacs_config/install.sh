#!/bin/bash


set -e

SCRIPT_LOCATION="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -d "~/.emacs.d" ]; then
    mv "~/.emacs.d" "~/.bak_emacs.d"
fi

mkdir "~/.emacs.d"
cp "${SCRIPT_LOCATION}/*.el" "~/.emacs.d"

echo "To finish installation open ~/.emacs.d/init.el with emacs and install"
echo "listed packages. This may require some restarts to update the package"
echo "public key."
