-module(irc_parse).

-export([parse/1]).

% parse the input string
% returns {ok, Command, [Args]}
% or      {ok, Prefix, Command, [Args]}
parse(String) ->
  [PossiblePrefix | [Command | Args]] = re:split(String, " ", [{return, list}]),
  case is_prefix(PossiblePrefix) of
    true -> {ok, PossiblePrefix, Command,
            parse_args(Args)};
    false -> {ok, PossiblePrefix, parse_args([Command | Args])}
  end.

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
  Splitted = lists:map(fun(Arg) -> re:split(Arg,",",[{return, list}]) end, Args),
  Splitted.
