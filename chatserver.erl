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

controller(Port) ->
   linten_on_tcp(Port),
   c_loop([], 0, 0).

linten_on_tcp(Port) ->
   case gen_tcp:listen(Port, [binary, {reuseaddr, true}, {active, once}]) of
      {ok, Listen} ->
         % io:format("tcp listen return ok. Enter server loop to accept connects. ~n"),
         start_server(?PARALLEL_NUM, Listen);
      Other ->
         io:format("Can't listen on socket ~p~n", [Other])
   end.

start_server(0, _) ->
   true;
start_server(Num, LS) ->
   spawn_link(fun() -> server_access_loop(LS) end),
   start_server(Num-1, LS).

server_access_loop(Listen) ->
   case gen_tcp:accept(Listen) of 
      {ok, Socket} ->
         % Create chat agent processor
         % io:format("tcp accept return ok. Start client agent processor. ~n"),
         Pid = spawn(fun() -> client_agent(Socket) end),
         gen_tcp:controlling_process(Socket, Pid),
         server_access_loop(Listen);
      {error, closed} ->
         io:format("tcp accept return error: closed. Exit access loop. ~n");
      _Other ->
         io:format("tcp accept return error ~p~n", [_Other]),
         server_access_loop(Listen)
   end.

c_loop(Members, Rcounter, Scounter) ->
   receive
      {'EXIT', From, Reason} ->
         io:format("Accept proc exiting, got ~p~n", [{'EXIT', From, Reason}]),
         c_loop(Members, Rcounter, Scounter);
      exit ->
         io:format("Controller loop receive normal exit. ~n"),
         send_exit(Members);
      print ->
         io:format("Current number of clients in chatroom: ~w~n", [length(Members)]),
         io:format("Current counter of messages [sent/recv]: [~w/~w]~n", [Scounter, Rcounter]),
         c_loop(Members, Rcounter, Scounter);
      reset ->
         c_loop(Members, 0, 0);
      {Me, join} ->
         % io:format("New client join ~p~n", [Me]),
         Me ! {cont_proc, ok},
         case lists:member(Me, Members) of
            true ->
               c_loop(Members, Rcounter, Scounter);
            false ->
               c_loop([Me | Members], Rcounter, Scounter)
         end;
      {Me, leave} ->
         % io:format("Client leave ~p~n", [Me]),
         Me ! {cont_proc, ok},
         case lists:member(Me, Members) of
            true ->
               c_loop(lists:delete(Me, Members), Rcounter, Scounter);
            false ->
               c_loop(Members, Rcounter, Scounter)
         end;
      {Me, {recv, Msg}} ->
         % io:format("Client send message to all. ~p~n", [Msg]),
         Me ! {cont_proc, ok},
         S = send_msg(Me, Members, Msg, 0),
         c_loop(Members, Rcounter+1, Scounter+S)
   end.

send_msg(From, [H|T], Msg, S) ->
   if 
      From =:= H ->
         send_msg(From, T, Msg, S);
      true ->
         H ! {send, Msg},
         send_msg(From, T, Msg, S+1)
   end;
send_msg(_From, [], _Msg, S) ->
   S.

send_exit([H|T]) ->
   H ! exit,
   send_exit(T);
send_exit([]) ->
   true.

client_agent(Socket) ->
   ok = rpc(cont_proc, join),
   agentloop(Socket).

agentloop(Socket) ->
   %io:format("Enter client agent loop. ~n"),
   receive
      exit ->
         % io:format("Client agent receive normal exit. ~n"),
         gen_tcp:close(Socket);
      {send, Msg} ->
         % io:format("Send msg to client ~p~n", [Msg]),
         gen_tcp:send(Socket, Msg),
         agentloop(Socket);
      {tcp, Socket, Data} ->
         % io:format("Receive msg from client ~p~n", [Data]),
         inet:setopts(Socket, [{active, once}]),
         ok = rpc(cont_proc, {recv, Data}),
         agentloop(Socket);
      {tcp_closed, Socket} ->
         % io:format("Client agent tcp closed. ~n"),
         ok = rpc(cont_proc, leave)
   end.

