#!/usr/bin/env sh

erl -init_debug -pa ebin -sname testnost@localhost -setcookie abc123 -boot priv/erlfs -config test