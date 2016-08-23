#!/bin/sh -e

USER=$1
if [ "$USER" = "" ]; then
  echo Usage: $0 github-username
  exit 1
fi

git-jar make -s repo $1 private
