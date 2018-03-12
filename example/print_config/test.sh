#!/bin/sh

cat $HOME/.notmuch-config | grep '[a-Z]' | grep -v "#"
