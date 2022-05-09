-module(mB@foreign).

-include_lib("gproc/src/gproc_int.hrl").

-export([ create/2
        , disable/1
        , enable/1
        , raiseMsg/2
        , subscribeImpl/4
        , subscribeExistingImpl/3
        , updateMetadata/2
        , unsubscribe/1
        ]).

-define(unit, {unit}).
-define(just(A), {just, A}).
-define(nothing, {nothing}).

-define(gprocPropertyKey(Name), {p,l,Name}).
-define(gprocNameKey(Name), {n,l,Name}).
-define(metadataKey, md).
-define(metadataAttribute(Md), {?metadataKey, Md}).
-define(enabled(MapperFn), {e, MapperFn}).
-define(disabled(MapperFn), {d, MapperFn}).



-define(locked, locked).

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

subscribeImpl(Enabled, BusName, Mapper, MetadataConstructor) ->
  fun() ->
      MaybeMetadata = subscribeLocked(Enabled, BusName, ?just(MetadataConstructor)),
      case MaybeMetadata of
        ?just(ExistingMetadata) ->
          self() ! Mapper(MetadataConstructor(ExistingMetadata));
        _ ->
          ok
      end,
      true = gproc:set_value(?gprocPropertyKey(BusName), ?enabled(Mapper)),
      ?unit
  end.


subscribeExistingImpl(Enabled, BusName, Mapper) ->
  fun() ->
      MaybeMetadata = subscribeLocked(Enabled, BusName, ?nothing),
      case MaybeMetadata of
        ?just(_) ->
          true = gproc:set_value(?gprocPropertyKey(BusName), ?enabled(Mapper));
        ?nothing ->
          catch gproc:unreg(?gprocPropertyKey(BusName))
      end,
      MaybeMetadata
  end.


subscribeLocked(enabled, BusName, MaybeMetadataConstructor) ->
  try
    true = gproc:reg(?gprocPropertyKey(BusName), ?locked),
    Metadata = gproc:get_attribute(?gprocNameKey(BusName), ?metadataKey),
    ?just(Metadata)
  catch
    error:badarg ->
      ?nothing
  end.


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
  Entries = ets:select(gproc, [{{{Key,'_'}, '$1', '$2'},[],[{{'$1','$2'}}]}]),
  case lists:any(fun({_, ?locked}) -> true;
                    (_) -> false
                 end,
                 Entries) of
    true ->
      timer:sleep(0),
      send1(Key, Msg);
    false ->
      lists:foreach(fun({Pid, ?enabled(Fn)}) ->
                        case Fn(Msg) of
                          ?nothing -> ok;
                          ?just(MappedMsg) ->
                            Pid ! MappedMsg
                        end;
                       (_) -> ok
                    end, Entries)
  end.
