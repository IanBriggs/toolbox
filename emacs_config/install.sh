#!/bin/bash


set -e

SCRIPT_LOCATION="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [ -d "${HOME}/.emacs.d" ]
then
    mv "${HOME}/.emacs.d" "${HOME}/.bak_emacs.d"
fi

mkdir "${HOME}/.emacs.d"
ln -s  "${SCRIPT_LOCATION}/init.el" "${HOME}/.emacs.d/init.el"
ln -s  "${SCRIPT_LOCATION}/custom.el" "${HOME}/.emacs.d/custom.el"

echo "To finish installation open ${HOME}/.emacs.d/init.el with emacs and"
echo "install listed packages. This may require some restarts to update the"
echo "package public key."
