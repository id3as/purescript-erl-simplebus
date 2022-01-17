-module(simpleBus@foreign).

-export([ subscribeImpl/2
        , unsubscribe/1
        , raiseImpl/2
        ]).


subscribeImpl(BusName, Callback) ->
  Recipient = self(),
  Fun = fun Fun(MonitorRef) ->
              receive
                stop ->
                  gproc:unreg({p, l, BusName}),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', _, _, _, _} ->
                  gproc:unreg({p, l, BusName}),
                  exit(normal);
                Msg ->
                  (Callback(Msg))(), %% Invoke the effect immediately
                  Fun(MonitorRef)
              end
           end,
  fun() ->
    spawn_link(fun() ->
                   gproc:reg({p, l, BusName}),
                   MonitorRef = monitor(process, Recipient),
                   Fun(MonitorRef)
               end)
  end.

unsubscribe(Ref) ->
  fun() ->
      Ref ! stop,
      ok
  end.


raiseImpl(BusName, Msg) ->
  fun() ->
    gproc:send({p,l,BusName}, Msg)
  end.
