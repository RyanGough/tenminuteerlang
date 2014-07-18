-module (demo).

-export ([helloworld/0,supervisor/0]).

supervisor() ->
    process_flag(trap_exit, true),
    register(demo, spawn_link(demo, helloworld, [])),
    receive
        {'EXIT', _From, Reason} ->
            io:format("crashed because: ~p~n", [Reason]),
            supervisor()
    end.

helloworld() ->
    receive
        hello ->
            io:format("hello, world!~n"),
            helloworld();
        {divide, Dividend, Divisor} ->
            io:format("the result is: ~p~n", [Dividend / Divisor]),
            helloworld()
    end.
    