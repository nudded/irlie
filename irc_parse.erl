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
  
parse_without_prefix([], _) -> {error, no_command};
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
parse_args(Array) -> 
  {Args, Trailing} = lists:split(index_of_trailing(Array), Array),
  Parsed = lists:map(fun(Arg) -> parse_arg(Arg) end, Args),
  case join_trailing_with_spaces(Trailing) of
    [] -> Parsed;
    String -> Parsed ++ [String]
  end.

% parse individual argument
parse_arg(Arg) ->
  Array = re:split(Arg,",",[{return, list}]),
  if 
    length(Array) > 1 -> Array;
    true              -> lists:nth(1,Array)
  end.

% return the index of the trailing argument
% if no trailing argument is found it will return the size of the list
index_of_trailing(Args) ->
  index_of_trailing(Args, 0).

index_of_trailing([], Index) -> Index;
index_of_trailing([X | Xs], Index) ->
  case re:run(X, "^:") of
    nomatch -> index_of_trailing(Xs, Index + 1);
    {match, _ } -> Index
  end.

% strips the : from the trailing argument
% makes a new string by joining the seperate words with spaces
join_trailing_with_spaces([]) -> [];
join_trailing_with_spaces([X|Xs]) ->
  lists:nthtail(1,X) ++ lists:foldl(fun(Acc, V) ->
        V ++ " "++ Acc end, "", Xs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                             TEST FUNCTIONS                                  %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_test() ->
  ?assertEqual({ok, "QUIT", []}, parse("QUIT")),
  ?assertEqual({ok, ":nudded", "QUIT", []}, parse(":nudded QUIT")),
  ?assertEqual({error, no_command}, parse("")),
  ?assertEqual({error, no_command}, parse(" ")),
  ?assertEqual({ok, "NICK", ["nudded"]}, parse("NICK nudded")),
  ?assertEqual({ok, "USER", ["*", "*"]}, parse("USER * *")),
  ?assertEqual({ok, "JOIN", ["test", ["#channel", "&otherchannel"]]},
               parse("JOIN test #channel,&otherchannel")),
  ?assertEqual({ok, "QUIT", ["this is a message"]}, 
               parse("QUIT :this is a message")),
  ?assertEqual({ok, "USER", ["*", "*", "Toon 112"]},
               parse("USER * * :Toon 112")).


