-module(erl_string_encoders).

-compile([no_native]).

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

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

%% public
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
    ?NOT_LOADED.

base64_decode(_Binary) ->
    ?NOT_LOADED.

web_safe_base64_encode(_Binary) ->
    ?NOT_LOADED.

web_safe_base64_decode(_Binary) ->
    ?NOT_LOADED.

base85_encode(_Binary) ->
    ?NOT_LOADED.

base85_decode(_Binary) ->
    ?NOT_LOADED.

base16_encode(_Binary) ->
    ?NOT_LOADED.

base16_decode(_Binary) ->
    ?NOT_LOADED.

base2_encode(_Binary) ->
    ?NOT_LOADED.

base2_decode(_Binary) ->
    ?NOT_LOADED.

url_encode(_Binary) ->
    ?NOT_LOADED.

url_min_encode(_Binary) ->
    ?NOT_LOADED.

url_decode(_Binary) ->
    ?NOT_LOADED.

javascript_encode(_Binary) ->
    ?NOT_LOADED.

int_to_string(_Int) ->
    ?NOT_LOADED.

int64_to_string(_Int64) ->
    ?NOT_LOADED.

uint_to_string(_UInt) ->
    ?NOT_LOADED.

uint64_to_string(_UInt64) ->
    ?NOT_LOADED.

double_to_string_fixed(_Double, _Precision) ->
    ?NOT_LOADED.

double_to_string_variable(_Double, _Precision) ->
    ?NOT_LOADED.

to_upper(_Binary) ->
    ?NOT_LOADED.

to_lower(_Binary) ->
    ?NOT_LOADED.

to_print(_Binary) ->
    ?NOT_LOADED.

%% private
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
