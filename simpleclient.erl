-module(simpleclient).
-export([start_counter/0, start_client/3, start_client_send_msg/3]).

start_counter() ->
   Pid = spawn(fun() -> counter(0, 0) end),
   register(counter_proc, Pid).

counter(CNum, MsgNum) ->
   receive
      {add_client} ->
         counter(CNum + 1, MsgNum);
      {del_client} ->
         counter(CNum - 1, MsgNum);
      {recv_msg} ->
         counter(CNum, MsgNum + 1);
      {reset} ->
         counter(CNum, 0);
      {print} ->
         io:format("Current number of client/msg: [~w, ~w]~n",  [CNum, MsgNum]),
         counter(CNum, MsgNum)
   end.


start_client(Ip, Port, Num) ->
   counter_proc ! {print},
   timer:sleep(10),
   start1(Ip, Port, Num),
   timer:sleep(500),
   counter_proc ! {print}.

start1(_, _, 0) ->
   true;
start1(Ip, Port, Num) ->
   timer:sleep(5),
   spawn(fun() -> client(Ip, Port) end),
   start1(Ip, Port, Num-1).

client(Ip, PortNo) ->
       case gen_tcp:connect(Ip,PortNo,[binary, {active,true}]) of
          {ok,Sock} ->
             counter_proc ! {add_client},
             loop(Sock);
          _Other ->
             io:format("gen_tcp connect return error: ~p~n", [_Other])
       end.

loop(Sock) ->
   receive
      {tcp, Sock, _Data} ->
         counter_proc ! {recv_msg},
         loop(Sock);
      {tcp_closed, Sock} ->
         counter_proc ! {del_client}
   end.

start_client_send_msg(Ip, Port, Msg) ->
   counter_proc ! {reset},
   counter_proc ! {print},
   timer:sleep(100),
   case gen_tcp:connect(Ip,Port,[binary, {active,true}]) of
      {ok,Sock} ->
         gen_tcp:send(Sock, Msg),
         counter_proc ! {add_client},
         Pid = spawn(fun() -> loop(Sock) end),
         gen_tcp:controlling_process(Sock, Pid);
      _Other ->
         io:format("gen_tcp connect return error: ~p~n", [_Other])
   end,
   timer:sleep(5000),
   counter_proc ! {print}.

