-module(good_02).
-export([update_qrs/2]).

-record(oabStreamResourceData,{local_ctrl_desc,
                               local_desc,
                               remote_desc}).
-record(oabStreamData,{streamId}).
-record(oabSdpMedia,{m_line}).
-record(oabQueryResult,{streamId,
                        supported_ftm_list}).
-record(megaco_sdp_m,{media,
                      fmt_list}).

update_qrs([SRD|RDTail], QRS) ->
    case is_valid_audio_RD(SRD) of
        true ->
            BgfFmt = get_bgf_fmt_list(SRD),
            NewBgfFmt = handle_dash(BgfFmt),
            StreamId = get_stream_id(SRD),
            NewQrs = store_updated_qrs(NewBgfFmt, StreamId, QRS),
            update_qrs(RDTail, NewQrs);
        false ->
            update_qrs(RDTail, QRS)
    end.

is_valid_audio_RD(#oabStreamResourceData{local_desc = LD, remote_desc = RD}) ->
    #oabSdpMedia{m_line = #megaco_sdp_m{media = Media}} = LD,
    #oabSdpMedia{m_line = #megaco_sdp_m{fmt_list = [OfferPt|_]}} = RD,
    (Media == audio) andalso is_list(OfferPt) andalso OfferPt =/= "-".

get_bgf_fmt_list(#oabStreamResourceData{local_desc = LD}) ->
    #oabSdpMedia{m_line = #megaco_sdp_m{fmt_list = BgfFmt}} = LD,
    BgfFmt.

handle_dash(["-"|_]) ->
    [];
handle_dash(X) ->
    X.

get_stream_id(#oabStreamResourceData{local_ctrl_desc = LD}) ->
    #oabStreamData{streamId = StreamId} = LD,
    StreamId.

store_updated_qrs(BgfFmt, StreamId, QRS) ->   
    NewQRS = #oabQueryResult{streamId = StreamId,
                             supported_ftm_list =  BgfFmt},
    lists:keystore(StreamId, #oabQueryResult.streamId, NewQRS).
