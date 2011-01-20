-module(channel_handler).

-export([message/3, join/2]).

% TODO: This should be a gen_server
% init should create a ets set of {channel, [users]}
% any messages passed can then use this table (State)

% Send message from user to all users in a channel.
message(User, Message, Channel) ->
  Users = get_users(Channel),
  send_message(Message, Users).

join(User, Channel) ->
  add_user(User, Channel).
