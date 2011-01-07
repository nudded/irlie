-module(irc_parse).

-export([parse/1]).

-include_lib("eunit/include/eunit.hrl").

% parse the input string
% returns {ok, Command, [Args]}
% or      {ok, Prefix, Command, [Args]}
parse(String) ->
  [PossiblePrefix | Args] = re:split(String, " ", [{return, list}]),
  case is_prefix(PossiblePrefix) of
    true  -> parse_with_prefix(PossiblePrefix, Args);
    % in this case PossiblePrefix is the Command
    false -> parse_without_prefix(PossiblePrefix, Args)
  end.

parse_with_prefix(_, []) -> {error, no_command};
parse_with_prefix(Prefix, Array) ->
  [Command | Args] = Array,
  {ok, Prefix, Command, parse_args(Args)}.
  
parse_without_prefix(Command, Array) ->
  {ok, Command, parse_args(Array)}.

% test if the given string is valid as a irc prefix
% TODO regex could be better
is_prefix(Prefix) ->
  case re:run(Prefix, "^:[^ ]") of
    nomatch -> false;
    {match, _} -> true
  end.

% parse the arguments so that "channel,channel test" becomes [["channel",
% "channel"], "test"]. If you know what i mean.
parse_args(Args) -> 
  lists:map(fun(Arg) -> parse_arg(Arg) end, Args).

% parse individual argument
parse_arg(Arg) ->
  Array = re:split(Arg,",",[{return, list}]),
  if 
    length(Array) > 1 -> Array;
    true              -> lists:nth(1,Array)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                             TEST FUNCTIONS                                  %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_test() ->
  ?assertEqual({ok, "QUIT", []}, parse("QUIT")),
  ?assertEqual({ok, ":nudded", "QUIT", []}, parse(":nudded QUIT")),
  ?assertEqual({ok, "NICK", ["nudded"]}, parse("NICK nudded")).


