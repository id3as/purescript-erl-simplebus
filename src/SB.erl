-module(sB@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ subscribeImpl/2
        , raise/2
        ]).

-define(key(Name), {p,l,Name}).

subscribeImpl(BusName, Mapper) ->
  fun() ->
    io:format(user, "Called with ~p ~n", [?key(BusName)]),
    Res = gproc:reg(?key(BusName), {active, Mapper}),
    io:format(user, "Returned ~p ~n", [Res]),
    {unit}
  end.

unsubscribe(Ref) ->
  fun() ->
      Ref ! stop,
      ok
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
raise(BusName, Msg) ->
  fun() ->
    gproc:send({p,l,BusName}, Msg)
  end.

disable(BusName) ->
  fun() ->
      gproc:send({p,l,BusName}, disable)
  end.

enable(BusName) ->
  fun() ->
      gproc:send({p,l,BusName}, enable)
  end.
