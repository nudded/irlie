-module(socket_acceptor).

-export([start/0, stop/0]).

-export([quit/1, listen/1,handle/1, handle_line/1]).

start() ->
  {ok, ListenSocket} = gen_tcp:listen(6667, [list, {active, true}]),
  spawn(?MODULE, listen, [ListenSocket]),
  register(stop_server, spawn(?MODULE, quit, [ListenSocket])).

stop() ->
  stop_server ! close.


quit(ListenSocket) ->
  receive
    close -> gen_tcp:close(ListenSocket)
  end.

listen(ListenSocket) ->
  case gen_tcp:accept(ListenSocket) of
    {ok, Socket} -> 
      spawn(?MODULE, listen, [ListenSocket]),
      handle(Socket);
    {error, closed} -> io:format("WARNING: listen socket closed~n")
  end.

handle(Socket) ->
  receive 
    {tcp, _, "QUIT\n"} -> gen_tcp:close(Socket);
    {tcp, _, Message} -> handle_line(Message),
      handle(Socket);
    {tcp_closed, _} -> io:format("socket closed~n")
  end. 


handle_line(Line) ->
  NoEnding = re:replace(Line, "\r\n$", ""),
  Arr = re:split(NoEnding,"\r\n",[{return,list}]),
  lists:map(fun(X) -> 
                   io:format("~p~n", [irc_parse:parse(X)])
            end, Arr).

