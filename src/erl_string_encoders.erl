-module(erl_string_encoders).

-export([
    init/0,
    web_safe_base64_encode/1,
    web_safe_base64_decode/1
]).

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, _} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Dir -> Dir
              end,
    SoName = filename:join(PrivDir, "erl_string_encoders_nif"),
    case catch erlang:load_nif(SoName, 0) of
        ok -> ok;
        LoadError -> error_logger:error_msg("erl_string_encoders: error loading NIF (~p): ~p",
                                            [SoName, LoadError])
    end.

web_safe_base64_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

web_safe_base64_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.
