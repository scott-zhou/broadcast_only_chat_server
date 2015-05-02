-module(chatserver).
-export([start/1]).

-define(PARALLEL_NUM, 10).

% send exit to cont_proc to stop server
% send print to cont_proc to print counters
% send reset to cont_proc to reset counters

start(Port) -> 
   io:format("Chat server start on port ~w~n", [Port]),
   case is_started() of
      true ->
         alreadyStarted;
      false ->
         CPid = spawn(fun() -> controller(Port) end),
         register(cont_proc, CPid)
         % io:format("Controller proc created. PID ~p~n", [CPid]),
   end.

rpc(Proc_name, Mail) ->
   Proc_name ! {self(), Mail},
   receive
      {Proc_name, Response} ->
         Response
   end.

%% To avoid duplicate start
is_started() ->
   case whereis(cont_proc) of 
      undefined ->
         false;
      _Pid ->
         true
   end.

