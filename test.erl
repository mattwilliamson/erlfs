%% This module runs eunit tests on all modules.

-module(test).

test() ->
    Modules = [],
    test(Modules).

test([]) ->
    ok;
test([H|T]) ->
    test(H);
test(Module) ->
    Module:test().
