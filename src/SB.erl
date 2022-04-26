-module(sB@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ subscribeImpl/2
        , raise/2
        ]).

-define(key(Name), {p,l,Name}).
-define(unit, {unit}).

subscribeImpl(BusName, Mapper) ->
  fun() ->
    true = gproc:reg(?key(BusName), {active, Mapper}),
    ?unit
  end.

%%------------------------------------------------------------------------------
%% This code is taken from gproc.erl but slightly modified to extract and use
%% a stored mapping function per recipient and also to enable active / stopped
%% toggling on busses
%%------------------------------------------------------------------------------
raise(BusName, Msg) ->
  fun() ->
    Key = {p, l, BusName},
    ?CATCH_GPROC_ERROR(send1(Key, Msg), [Key, Msg])
  end.



disable(BusName) ->
  fun() ->
      gproc:send({p,l,BusName}, disable),
      ?unit
  end.

enable(BusName) ->
  fun() ->
      gproc:send({p,l,BusName}, enable)
  end.

unsubscribe(Ref) ->
  fun() ->
      Ref ! stop,
      ok
  end.


%%------------------------------------------------------------------------------
%% Internal functions - being slight variants of the functions by the same name
%% in gproc.erl
%%------------------------------------------------------------------------------
send1(Key, Msg) ->
  lists:foreach(fun({Pid, Fn}) ->
                    Pid ! Fn(Msg)
                end, lookup_pids(Key)).

lookup_pids(Key) ->
  ets:select(gproc, [{{{Key,'_'}, '$1', {active, '$2'}},[],[{{'$1','$2'}}]}]).
