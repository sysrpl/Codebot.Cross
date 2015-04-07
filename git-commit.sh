#!/bin/sh

if [ $# -eq 0 ]
  then
    echo "Please provide a commit message"
    exit 0
fi

git add -u
git commit -m "$1"
git push
