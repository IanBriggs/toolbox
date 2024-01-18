#!/bin/bash

# Exit on error
set -e

# Absolute path to this directory
SCRIPT_LOCATION="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Backup old emacs, overwite earlier backup
if [ -d "${HOME}/.emacs.d" ]
then
    mv "${HOME}/.emacs.d" "${HOME}/.bak_emacs.d"
fi

# Set our new files
mkdir "${HOME}/.emacs.d"
ln -s  "${SCRIPT_LOCATION}/early-init.el" "${HOME}/.emacs.d/early-init.el"
ln -s  "${SCRIPT_LOCATION}/init.el" "${HOME}/.emacs.d/init.el"

# Warn the user of unusual next start
echo "The first time you launch emacs will take a while to download all new packages."
