# Erlang in Ten Minutes

I recently did a lightning talk at the [XP Manchester](http://xpmanchester.wordpress.com/) user group on the subject of Erlang. I'm pretty new to the language myself but am really loving it and was hoping I could pass on that enthusiasm to others. The talks were recorded and are available [here](http://xpmanchester.wordpress.com/2014/07/11/lightning-talk-videos/) but it's hard to follow the code on screen so I said I would stick it up here.

There are 3 revisions of the demo.erl file in this repo, which correspond to the 3 demos in the talk, although all the demos can be run from the final version of the code. I won't repeat the code in this readme, have a look in the demo.erl file, but I will show some snippets of the code I ran from the shell to exercise the code.

## Demo 1 - Actors

I wanted to demonstrate the actor model in erlang, so created a simple function that would respond to a message consisting of the atom 'hello' by prining "hello, world!" to the console. To try it out, fire up an erlang shell, compile the code, spawn a process to run the function (remember the process identifier in a variable), then send the process a 'hello' message. I also used the process manager application to show the running process.

```
ryan@machine: erl
Eshell V5.9.3.1
1> c(demo).
{ok, demo}
2> Blah = spawn(demo, helloworld, []).
<0.45.0>
3> Blah ! hello.
hello, world!
hello
4> pman:start().
```

## Demo 2 - Supervisors

I then wanted to show how the "Let It Crash" philosophy in Erlang, combined with the concept of Supervisors, can help to create very robust, highly available / fault tolerant systems. I added a new message handling clause to the helloworld function from the previous example, which matched a 3 tuple - {divide, Dividend, Divisor} - and printed the result of dividing the dividend by the divisor. This was mainly so I had an easy way to crash the process! I also added a supervisor function which spawned the helloworld function as a linked process and reigistered it under the name 'demo', and trapped errors in order to restart it in the event of a crash.

I compiled the code and showed I could now spawn a process to run the supervisor and that would start the helloworld worker process. I again used process manager to show the two processes had been created. I could send it a divide message and it would print the result. I then caused a crash and showed how the supervisor handled the EXIT that was thrown from the worker process, and how that the system was still available as the supervisor had created a new process to run the helloworld function.

```
ryan@machine: erl
Eshell V5.9.3.1
1> c(demo).
{ok, demo}
2> spawn(demo, supervisor, []).
<0.51.0>
3> pman:start().
<0.53.0>
4> demo ! {divide, 10, 2}.
the result is: 5.0
{divide, 10, 2}
5> demo ! {divide, 10, 0}.
crashed because: {badarith,[{demo,helloworld,0,[{file,"demo.erl"},{line,19}]}]}
{divide, 10, 0}
6> demo ! {divide, 10, 2}.
the result is: 5.0
```

