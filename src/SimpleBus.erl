-module(simpleBus@foreign).

-export([ subscribeImpl/3
        , unsubscribe/1
        , raiseImpl/2
        ]).


subscribeImpl(BusName, Recipient, Mapper) ->
  Fun = fun Fun(MonitorRef) ->
              receive
                stop ->
                  gproc:unreg({p, l, BusName}),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', MonitorRef, _, _, _} ->
                  gproc:unreg({p, l, BusName}),
                  exit(normal);
                Msg ->
                  Recipient ! Mapper(Msg),
                  Fun(MonitorRef)
              end
           end,
  fun() ->
    Ref = make_ref(),
    Launcher = self(),
    {Pid, MonitorRef} = spawn_monitor(fun() ->
                                       gproc:reg({p, l, BusName}),
                                       MonitorRef = monitor(process, Recipient),
                                       Launcher ! Ref,
                                       Fun(MonitorRef)
                                      end),
    receive
      Ref -> ok;
      {'DOWN', MonitorRef, _, _, Info} ->
        throw({subscription_failed, Info})
    end,
    demonitor(MonitorRef, [flush]),
    Pid
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
