-module(bad_01).

-export([update_pt_offset/4]).

-define(OAB_FALSE,false).
-define(OAB_PROACT_PT_OFFSET,0).
-record(oabProactStream,{stream_id}).
-record(megaco_sdp_m,{fmt_list,
                      port,
                      media}).
-record(megaco_sdp_a,{attribute,
                      value}).
-record(oabPtOffset,{streamId,
                     ptOffset}).

update_pt_offset(
    [#oabProactStream{stream_id = StreamId}|TProactStreams],
    PtOffsetIaSdp,
    OtherIaSdp,
    PtOs) ->
    {value, {StreamId, PtOffsetMedia}, TPtOffsetIaSdp} = lists:keytake(StreamId, 1, PtOffsetIaSdp),
    {value, {StreamId, OtherMedia}, TOtherIaSdp} = lists:keytake(StreamId, 1, OtherIaSdp),
    NewPtOs =
        case {lists:keyfind(megaco_sdp_m, 1, PtOffsetMedia),
              lists:keyfind(megaco_sdp_m, 1, OtherMedia),
              lists:member(
                  #megaco_sdp_a{attribute = "codec-changed", value = "0"},
                  OtherMedia)} of

            {#megaco_sdp_m{media = audio,
                           port = Port,
                           fmt_list = [ProPt|_] = ProactiveFmt},
             #megaco_sdp_m{fmt_list = [OfferPt|_]},
             ?OAB_FALSE} when Port =/= 0,
                              is_list(ProPt),
                              ProPt =/= "-",
                              OfferPt =/= "-" -> % Proactive codecs were added
                DynProactivePts = [DynPt||DynPt <- [list_to_integer(Pt)||Pt <- ProactiveFmt],
                                          DynPt >= 96,
                                          DynPt =< 127],
                PtOffset = 
                    case lists:keyfind(StreamId, #oabPtOffset.streamId, PtOs) of

                        #oabPtOffset{ptOffset = PtO} ->
                            PtO;

                        _ ->
                            ?OAB_PROACT_PT_OFFSET
                    end,
                NewPtOffset = 
                    case lists:partition(fun(Pt) -> Pt < PtOffset end, DynProactivePts) of

                        {[], []} -> % No dynamic pts
                            PtOffset;

                        {[], GreaterPts} -> % Common case
                            case lists:max(GreaterPts) + 1 of

                                NewPtO when NewPtO =< 127 ->
                                    NewPtO;

                                _ ->
                                    96
                            end; 

                        {LesserPts, _GreaterPts} -> % Pts have restarted from 96
                            lists:max(LesserPts) + 1
                    end,
                lists:keystore(StreamId,
                               #oabPtOffset.streamId,
                               PtOs,
                               #oabPtOffset{streamId = StreamId,
                                            ptOffset =  NewPtOffset});

            _ -> % No proactive codecs were added
                PtOs  
        end,    
    update_pt_offset(
        TProactStreams,
        TPtOffsetIaSdp,
        TOtherIaSdp,
        NewPtOs).
