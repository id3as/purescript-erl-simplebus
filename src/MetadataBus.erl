-module(metadataBus@foreign).

-export([ create_/2
        , subscribe_/5
        , unsubscribe/1
        , raise_/2
        , read_metadata_/1
        , update_metadata_/2
        ]).

create_(BusName, Metadata) ->
    fun() ->
            _ = gproc:add_local_property({metadata, BusName}, Metadata),
            unit
    end.

subscribe_(Nothing, Just, BusTerminated, BusName, Callback) ->
  Recipient = self(),
  Fun = fun Fun(RecipientMonitorRef, BusOwnerMonitorRef) ->
              receive
                unsubscribe ->
                  gproc:unreg({p, l, BusName}),
                  demonitor(RecipientMonitorRef),
                  demonitor(BusOwnerMonitorRef),
                  exit(normal);
                {'DOWN', RecipientMonitorRef, _, _, _} ->
                  demonitor(BusOwnerMonitorRef),
                  gproc:unreg({p, l, BusName}),
                  exit(normal);
                {'DOWN', BusOwnerMonitorRef, _, _, _} ->
                  demonitor(RecipientMonitorRef),
                  gproc:unreg({p, l, BusName}),
                  (Callback(BusTerminated))(),
                  exit(normal);
                Msg ->
                  (Callback(Msg))(), %% Invoke the effect immediately
                  Fun(RecipientMonitorRef, BusOwnerMonitorRef)
              end
           end,
  fun() ->
          case gproc:lookup_local_properties({metadata, BusName}) of
              [{Pid, Metadata}] ->
                  Worker = spawn_link(fun() ->
                                              gproc:reg({p, l, BusName}),
                                              RecipientMonitorRef = monitor(process, Recipient),
                                              BusOwnerMonitorRef = monitor(process, Pid),
                                              Fun(RecipientMonitorRef, BusOwnerMonitorRef)
                                      end),
                  Just({tuple, Worker, Metadata});
              [] ->
                  Nothing
          end
  end.

unsubscribe(Ref) ->
  fun() ->
      Ref ! unsubscribe,
      ok
  end.

raise_(BusName, Msg) ->
  fun() ->
    gproc:send({p,l,BusName}, Msg)
  end.

read_metadata_(BusName) ->
    fun() ->
            [{_Pid, Metadata}] = gproc:lookup_local_properties({metadata, BusName}),
            Metadata
    end.

update_metadata_(BusName, Metadata) ->
    fun() ->
            _ = gproc:set_value({p, l, {metadata, BusName}}, Metadata),
            unit
    end.
