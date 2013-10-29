-module(erl_string_encoders).

-export([
    init/0,
    base64_encode/1,
    base64_decode/1,
    web_safe_base64_encode/1,
    web_safe_base64_decode/1,
    base85_encode/1,
    base85_decode/1,
    base16_encode/1,
    base16_decode/1,
    base2_encode/1,
    base2_decode/1,
    url_encode/1,
    url_min_encode/1,
    url_decode/1,
    javascript_encode/1,
    to_upper/1,
    to_lower/1,
    to_print/1,
    int_to_string/1,
    uint_to_string/1,
    int64_to_string/1,
    uint64_to_string/1,
    double_to_string_fixed/2,
    double_to_string_variable/2
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

base64_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base64_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

web_safe_base64_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

web_safe_base64_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base85_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base85_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base16_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base16_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base2_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

base2_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

url_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

url_min_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

url_decode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

javascript_encode(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

int_to_string(_Int) ->
    {error, erl_string_encoders_nif_not_loaded}.

int64_to_string(_Int64) ->
    {error, erl_string_encoders_nif_not_loaded}.

uint_to_string(_UInt) ->
    {error, erl_string_encoders_nif_not_loaded}.

uint64_to_string(_UInt64) ->
    {error, erl_string_encoders_nif_not_loaded}.

double_to_string_fixed(_Double, _Precision) ->
    {error, erl_string_encoders_nif_not_loaded}.

double_to_string_variable(_Double, _Precision) ->
    {error, erl_string_encoders_nif_not_loaded}.

to_upper(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

to_lower(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.

to_print(_Binary) ->
    {error, erl_string_encoders_nif_not_loaded}.
