-module(bad_02).
-export([wait_rar_for_npli/1]).

-define(RX_EVENT(X), X).
-define(DBG(A,B), ok).
-define(CCPC_SCAM(X), X).
-record(regC,{}).


wait_rar_for_npli(#regC{} = Cnt) ->    WaitTime = regLib:get_wait_npli_time(),
    receive 
        {distr_msg, _, ?RX_EVENT({aaa_handling, undefined, _})} ->
                ?DBG("wait_rar_for_npli, aar failed", []),
                {Cnt, []};

        {distr_msg, _, ?RX_EVENT({aaa_handling, _, _} = AAA_Event)} ->
                Cnt1 = regRx:handle_rx_event(AAA_Event, Cnt),

                receive
                    {distr_msg,
                      DistrId,
                      ?RX_EVENT({rar_handling, {_, AVPs}, _, _, _} = RAR_Event)} ->

                            Npli = regRx:get_npli(AVPs),
                            ?DBG("wait_rar_for_npli, receive rar in timer.", [{npli, Npli}]),
                            self() ! ?CCPC_SCAM({internal_rar_handling, DistrId, RAR_Event}),
                            {Cnt1, Npli}

                after WaitTime -> 
                            ?DBG("wait_rar_for_npli, timer expired after receive aaa.", 
                                  [{timer, WaitTime}]),
                            {Cnt1, []}
                                end
    after WaitTime+50 ->
                {Cnt, []}
    end.
