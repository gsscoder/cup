%% 2015, Giacomo Stelluti Scala (@gsscoder, gsscoder@gmail)
%% from (1): http://stackoverflow.com/questions/30636588/value-from-binding-in-lfe-interpreter-using-erlang
%%      (2): https://groups.google.com/forum/#!topic/lisp-flavoured-erlang/S5s6c5DovEE

-module(cup).

%% cup: cup library's entry point.

-export([consult/1, lambda_buffer/1, lambda_list/1, lambda_by_atom/2]).
%% Internals Exports for REPL testing
-export([lambdas_body/1, split_lines/1, consult_internal/1]).

-import(lfe_io, [read_string/1]).
-import(lfe_eval, [expr/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API

consult(Filename) ->
	consult_internal(fun() -> file:read_file(Filename) end).

lambda_buffer(Str) ->
    {ok, Expr} = read_string(Str),
	expr(Expr).

lambda_list(Defs) ->
	Body = lambdas_body(Defs),
	lambda_buffer(Body).

lambda_by_atom(List, Atom) ->
	hd([Func || {Name, Func} <- List, Name =:= Atom]).

%% Internals

lambdas_body(Defs) ->
    TupleDefs = [string:join(["(tuple ", X, ")"], "") || X <- Defs], 
    lists:flatten(["(list ", TupleDefs, ")"]).

split_lines(Buf) ->
	lists:filter(fun(X) -> is_blank_line(X) end,
		lists:map(fun(X) -> binary_to_list(X) end, re:split(Buf,"[\r\n]+"))).

is_blank_line(Ln) ->
	length(string:strip(string:strip(Ln,right,32),left,32)) > 0.

consult_internal(ReadFileFunc) ->
	{Res, Buf} = ReadFileFunc(),
	case Res of
		ok -> {Res, lambda_list(split_lines(Buf))};
		_  -> {error, []}
	end.

-ifdef(TEST).

lambdas_body_test() ->
    ?assert("(list (tuple STR1))" =:= lambdas_body(["STR1"])),
    ?assert("(list (tuple STR1)(tuple STR2))" =:= lambdas_body(["STR1", "STR2"])).

lambda_by_atom_test() ->
	L = lambda_by_atom([{'a', fun()->1 end}, {'b', fun()->"two" end}], 'b'),
	?assert(is_function(L)),
	?assert(L() =:= "two").

lambda_list_one_element_test() ->
	[{Name, Lambda}] = lambda_list(["'get-timeout (lambda() (* 3 1000))"]),
	?assert(is_atom(Name)),
	?assert(Name =:= 'get-timeout'),
	?assert(is_function(Lambda)),
	?assert(Lambda() =:= 3000).
	
lambda_list_two_element_test() ->
	LL = lambda_list(["'get-timeout (lambda() (* 3 1000))", "'get-endpoint (lambda() (list '\"localhost\"))"]),
	Lambda = lambda_by_atom(LL, 'get-endpoint'),
	?assert(length(LL) =:= 2),
	?assert(hd(Lambda()) =:= "localhost").
	
consult_internal_test() ->
	{ok, LL} = consult_internal(fun() -> {ok, "'get-timeout (lambda() (* 3 1000))\n'get-endpoint (lambda() (list '\"localhost\"))"} end),
	Lambda = lambda_by_atom(LL, 'get-timeout'),
	?assert(length(LL) =:= 2),
	?assert(is_function(Lambda)),
	?assert(Lambda() =:= 3000).

-endif.

%% End of Module.
