#!/usr/bin/env sh

erl -init_debug -pa ebin -sname testnost@localhost -setcookie abc123 -boot erlfs-alpha -config test