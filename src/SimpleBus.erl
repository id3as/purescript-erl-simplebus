-module(simpleBus@foreign).

-export([ subscribeImpl/3
        , unsubscribe/1
        , raiseImpl/2
        , disable/1
        , enable/1
        ]).


subscribeImpl(BusName, Recipient, Mapper) ->
  Fun = fun Fun(MonitorRef, Enabled) ->
              receive
                stop ->
                  gproc:unreg({p, l, BusName}),
                  demonitor(MonitorRef),
                  exit(normal);
                {'DOWN', MonitorRef, _, _, _} ->
                  gproc:unreg({p, l, BusName}),
                  exit(normal);
                disable ->
                  Fun(MonitorRef, false);
                enable ->
                  Fun(MonitorRef, true);
                {msg, Msg} when Enabled == true ->
                  Recipient ! Mapper(Msg),
                  Fun(MonitorRef, Enabled);
                {msg, _Msg} when Enabled == false ->
                  Fun(MonitorRef, Enabled)
              end
           end,
  fun() ->
    Ref = make_ref(),
    Launcher = self(),
    {Pid, MonitorRef} = spawn_monitor(fun() ->
                                       gproc:reg({p, l, BusName}),
                                       MonitorRef = monitor(process, Recipient),
                                       Launcher ! Ref,
                                       Fun(MonitorRef, true)
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
    gproc:send({p,l,BusName}, {msg, Msg})
  end.

disable(BusName) ->
  fun() ->
      gproc:send({p,l,BusName}, disable)
  end.

enable(BusName) ->
  fun() ->
      gproc:send({p,l,BusName}, enable)
  end.
