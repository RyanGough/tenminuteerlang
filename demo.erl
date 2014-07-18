-module (demo).

-export ([helloworld/0]).

helloworld() ->
    receive
        hello ->
            io:format("hello, world!~n"),
            helloworld()
    end.
    