#!/usr/bin/env sh

# This script assumes you have installed the following list of prerequisites.
# - GTK4.10 or newer and its dependencies.
# - Roswell Lisp Manager and its dependencies.
# See your distro's package manager to install them. For MacOS, use Homebrew.
# 
# It will run on MacOS and Linux. For Windows, this should also run on WSL2
# or MSYS2, provided you have installed the prerequisites.

export POSIXLY_CORRECT=1
set -ex

root_path="${PWD}"
name="megastrike"

ros install qlot
ros -e "(ql:update-all-dists :prompt nil)"

if [ -f "${root_path}/megastrike.asd" ]
then
    qlot install
    ros run --eval "(ql:quickload :megastrike)" \
        --eval "(asdf:make :megastrike)"
    chmod +x ./megastrike
fi
