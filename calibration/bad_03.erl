-module(bad_03).
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

%% This is bad because a lot of thins happen in function header.
%% Both how many arguments, and also how deep they match, and that the guard is complex
update_qrs(
    [#oabStreamResourceData{
        local_ctrl_desc = 
            #oabStreamData{streamId = StreamId},
        local_desc = 
            #oabSdpMedia{m_line = #megaco_sdp_m{media = audio,
                                                fmt_list = [BgfPt|_] = BgfFmt}},
        remote_desc = 
            #oabSdpMedia{m_line = #megaco_sdp_m{fmt_list = [OfferPt|_]}}}|RDTail],
    QRs)
    when is_list(BgfPt),
         is_list(OfferPt),
	 OfferPt =/= "-" -> % Codec query result found
    NewBgfFmt =
        case BgfPt of 

            "-" -> % No supported codecs
                [];

            _  ->
                BgfFmt
        end,
    NewQRs = lists:keystore(StreamId,
                            #oabQueryResult.streamId,
                            QRs,
                            #oabQueryResult{streamId = StreamId,
                                            supported_ftm_list =  NewBgfFmt}),
    update_qrs(RDTail, NewQRs).
