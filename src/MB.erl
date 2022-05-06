-module(mB@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ create/2
        , disable/1
        , enable/1
        , raiseMsg/2
        , subscribeImpl/3
        , updateMetadata/2
        , unsubscribe/1
        ]).

-define(gprocPropertyKey(Name), {p,l,Name}).
-define(gprocNameKey(Name), {n,l,Name}).
-define(enabled(MapperFn), {e, MapperFn}).
-define(disabled(MapperFn), {d, MapperFn}).
-define(unit, {unit}).
-define(just(A), {just, A}).
-define(nothing, {nothing}).

-define(metadataKey, md).
-define(metadataAttribute(Md), {?metadataKey, Md}).

create(BusName, InitalMetadata) ->
  fun() ->
      gproc:reg(?gprocNameKey(BusName), undefined, [?metadataAttribute(InitalMetadata)]),
      BusName
  end.

updateMetadata(BusName, Metadata) ->
  fun() ->
      gproc:set_attributes(?gprocNameKey(BusName), [?metadataAttribute(Metadata)]),
      raiseMsgInt(BusName, {metadataMsg, Metadata}),
      ?unit
  end.



subscribeImpl(enabled, BusName, Mapper) ->
  fun() ->
      try
        io:format(user, "subscribe ~p ~p~n", [BusName, ets:tab2list(gproc)]),
        State = gproc:get_attribute(?gprocNameKey(BusName), ?metadataKey),
        true = gproc:reg(?gprocPropertyKey(BusName), ?enabled(Mapper)),
        ?just(State)
      catch
        error:badarg ->
          ?nothing
      end
  end.
%% subscribeImpl(disabled, BusName, Mapper) ->
%%   fun() ->
%%       true = gproc:reg(?gprocPropertyKey(BusName), ?disabled(Mapper)),
%%       ?unit
%%   end.



%%------------------------------------------------------------------------------
%% This code is taken from gproc.erl but slightly modified to extract and use
%% a stored mapping function per recipient and also to enable active / stopped
%% toggling on busses
%%------------------------------------------------------------------------------
raiseMsg(BusName, Msg) ->
  fun() ->
      raiseMsgInt(BusName, Msg)
  end.

raiseMsgInt(BusName, Msg) ->
  io:format(user, "Raise ~p ~p~n", [BusName, Msg]),
  io:format(user, "Table ~p~n", [ets:tab2list(gproc)]),
  Key = ?gprocPropertyKey(BusName),
  ?CATCH_GPROC_ERROR(send1(Key, Msg), [Key, Msg]).

disable(BusName) ->
  fun() ->
      Key = ?gprocPropertyKey(BusName),
      case gproc:lookup_value(Key) of
        ?enabled(Fn) -> gproc:set_value(Key, ?disabled(Fn));
        ?disabled(_) -> ok
      end,
      ?unit
  end.

enable(BusName) ->
  fun() ->
      Key = ?gprocPropertyKey(BusName),
      case gproc:get_value(Key) of
        ?enabled(_) -> ok;
        ?disabled(Fn) -> gproc:set_value(Key, ?enabled(Fn))
      end,
      ?unit
  end.

unsubscribe(BusName) ->
  fun() ->
      Key = ?gprocPropertyKey(BusName),
      gproc:unreg(Key),
      ?unit
  end.


%%------------------------------------------------------------------------------
%% Internal functions - being slight variants of the functions by the same name
%% in gproc.erl
%%------------------------------------------------------------------------------
send1(Key, Msg) ->
  lists:foreach(fun({Pid, Fn}) ->
                    case Fn(Msg) of
                      ?nothing -> ok;
                      ?just(MappedMsg) ->
                        Pid ! MappedMsg
                    end
                end, lookup_pids(Key)).

lookup_pids(Key) ->
  ets:select(gproc, [{{{Key,'_'}, '$1', ?enabled('$2')},[],[{{'$1','$2'}}]}]).
