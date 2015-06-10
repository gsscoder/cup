# cup
CUP | ConfigUration Package for Erlang with LFE

<sup>The name is a tribute to [LFE](http://lfe.io/) logo.</sup>

## Acknowledgements:
From a Stackoverflow [discussion](https://groups.google.com/forum/#!topic/lisp-flavoured-erlang/S5s6c5DovEE)
followed by a [deepening](https://groups.google.com/forum/#!topic/lisp-flavoured-erlang/S5s6c5DovEE) in the LFE Google group.

Thanks to Duncann McGreggor and Robert Virding.

## Example
This is nothing more than explorative programming for learning. Anyway...
```erlang
%% using a list
LL = cup:lambda_list(["'get-timeout (lambda() (* 3 1000))", "'get-endpoint (lambda() (list '\"localhost\"))"]),
Lambda = cup:lambda_by_atom(LL, 'get-timeout'),
Timeout = Lambda(). % =:= 3000

%% reading from file
{ok, LL} = cup:consult("testconf.lfe"),
Lambda = cup:lambda_by_atom(LL, 'get-endpoints'),
EndPoints = Lambda(). % =:= ["localhost", "127.0.0.1"]
```

There's a lot to do from now... View this repository as the continuation of the discussion.
