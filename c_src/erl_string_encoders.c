#include <erl_nif.h>
#include <modp_b64.h>
#include <modp_b64w.h>
#include <modp_b85.h>
#include <modp_b16.h>
#include <modp_b2.h>
#include <modp_burl.h>
#include <modp_bjavascript.h>
#include <modp_numtoa.h>
#include <modp_ascii.h>

#include <string.h>

#define STRING_ENCODER_LEN(encoder_name, length_procedure, encode_procedure) \
static ERL_NIF_TERM                                                     \
encoder_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])       \
{                                                                       \
 ErlNifBinary input;                                                    \
 ErlNifBinary result;                                                   \
 int result_len;                                                        \
 if (!enif_inspect_iolist_as_binary(env, argv[0], &input))              \
     return enif_make_badarg(env);                                      \
 enif_alloc_binary(length_procedure(input.size), &result);              \
 if((result_len = encode_procedure((char *)result.data, (const char *)input.data, input.size)) == -1) { \
     enif_release_binary(&result);                                      \
     return make_error(env, "encode_failed");                           \
 }                                                                      \
 if(!enif_realloc_binary(&result, result_len)) {                        \
     enif_release_binary(&result);                                      \
     return make_error(env, "out_of_memory");                           \
 }                                                                      \
 return enif_make_binary(env, &result);                                 \
}

#define STRING_ENCODER(encoder_name, encode_procedure)                  \
static ERL_NIF_TERM                                                     \
encoder_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])       \
{                                                                       \
 ErlNifBinary input;                                                    \
 ErlNifBinary result;                                                   \
 if (!enif_inspect_iolist_as_binary(env, argv[0], &input))              \
     return enif_make_badarg(env);                                      \
 enif_alloc_binary(input.size + 1, &result);                            \
 encode_procedure((char *)result.data, (const char *)input.data, input.size); \
 if(!enif_realloc_binary(&result, input.size)) {                        \
     enif_release_binary(&result);                                      \
     return make_error(env, "out_of_memory");                           \
 }                                                                      \
 return enif_make_binary(env, &result);                                 \
}

#define STRING_CONVERT_NUMBER(converter_name, type, getter, size, convert_procedure) \
static ERL_NIF_TERM                                                     \
converter_name(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])     \
{                                                                       \
 type input;                                                            \
 ErlNifBinary result;                                                   \
 if (!getter(env, argv[0], &input))                                     \
     return enif_make_badarg(env);                                      \
 enif_alloc_binary(size+1, &result);                                    \
 result.data[size] = '\0';                                              \
 convert_procedure(input, (char *)result.data);                         \
 if(!enif_realloc_binary(&result, strlen((char *)result.data))) {               \
     enif_release_binary(&result);                                      \
     return make_error(env, "out_of_memory");                           \
 }                                                                      \
 return  enif_make_binary(env, &result);                                \
}

static ERL_NIF_TERM
make_atom(ErlNifEnv *env, const char *name)
{
    ERL_NIF_TERM ret;

    if (enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

static ERL_NIF_TERM
make_error(ErlNifEnv *env, const char *mesg)
{
    ERL_NIF_TERM error = make_atom(env, "error");
    return enif_make_tuple2(env, error, make_atom(env, mesg));
}

STRING_ENCODER_LEN(base64_encode, modp_b64_encode_len, modp_b64_encode)
STRING_ENCODER_LEN(base64_decode, modp_b64_decode_len, modp_b64_decode)
STRING_ENCODER_LEN(web_safe_base64_encode, modp_b64w_encode_len, modp_b64w_encode)
STRING_ENCODER_LEN(web_safe_base64_decode, modp_b64w_decode_len, modp_b64w_decode)
STRING_ENCODER_LEN(base85_encode, modp_b85_encode_len, modp_b85_encode)
STRING_ENCODER_LEN(base85_decode, modp_b85_decode_len, modp_b85_decode)
STRING_ENCODER_LEN(base16_encode, modp_b16_encode_len, modp_b16_encode)
STRING_ENCODER_LEN(base16_decode, modp_b16_decode_len, modp_b16_decode)
STRING_ENCODER_LEN(base2_encode, modp_b2_encode_len, modp_b2_encode)
STRING_ENCODER_LEN(base2_decode, modp_b2_decode_len, modp_b2_decode)
STRING_ENCODER_LEN(url_encode, modp_burl_encode_len, modp_burl_encode)
STRING_ENCODER_LEN(url_min_encode, modp_burl_encode_len, modp_burl_min_encode)
STRING_ENCODER_LEN(url_decode, modp_burl_decode_len, modp_burl_decode)
STRING_ENCODER_LEN(javascript_encode, modp_bjavascript_encode_len, modp_bjavascript_encode)
STRING_CONVERT_NUMBER(int_to_string, int32_t, enif_get_int, 16, modp_itoa10)
STRING_CONVERT_NUMBER(int64_to_string, int64_t, enif_get_int64, 24, modp_litoa10)
STRING_CONVERT_NUMBER(uint_to_string, uint32_t, enif_get_uint, 16, modp_uitoa10)
STRING_CONVERT_NUMBER(uint64_to_string, uint64_t, enif_get_uint64, 24, modp_ulitoa10)
STRING_ENCODER(to_upper, modp_toupper_copy)
STRING_ENCODER(to_lower, modp_tolower_copy)
STRING_ENCODER(to_print, modp_toprint_copy)

#define DOUBLE_BUFFER_SIZE 32

static ERL_NIF_TERM
double_to_string_fixed(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double input;
    int precision;
    ErlNifBinary result;

    if (!enif_get_double(env, argv[0], &input))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &precision))
        return enif_make_badarg(env);

    enif_alloc_binary(DOUBLE_BUFFER_SIZE + 1, &result);
    result.data[DOUBLE_BUFFER_SIZE] = '\0';
    modp_dtoa(input, (char *)result.data, precision);
    if(!enif_realloc_binary(&result, strlen((char *)result.data))) {
        enif_release_binary(&result);
        return make_error(env, "out_of_memory");
    }
    return  enif_make_binary(env, &result);
}

static ERL_NIF_TERM
double_to_string_variable(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    double input;
    int precision;
    ErlNifBinary result;

    if (!enif_get_double(env, argv[0], &input))
        return enif_make_badarg(env);

    if (!enif_get_int(env, argv[1], &precision))
        return enif_make_badarg(env);

    enif_alloc_binary(DOUBLE_BUFFER_SIZE + 1, &result);
    result.data[DOUBLE_BUFFER_SIZE] = '\0';
    modp_dtoa2(input, (char *)result.data, precision);
    if(!enif_realloc_binary(&result, strlen((char *)result.data))) {
        enif_release_binary(&result);
        return make_error(env, "out_of_memory");
    }
    return  enif_make_binary(env, &result);
}

static ErlNifFunc nif_functions[] = {
    {"base64_encode", 1, base64_encode},
    {"base64_decode", 1, base64_decode},
    {"web_safe_base64_encode", 1, web_safe_base64_encode},
    {"web_safe_base64_decode", 1, web_safe_base64_decode},
    {"base85_encode", 1, base85_encode},
    {"base85_decode", 1, base85_decode},
    {"base16_encode", 1, base16_encode},
    {"base16_decode", 1, base16_decode},
    {"base2_encode", 1, base2_encode},
    {"base2_decode", 1, base2_decode},
    {"url_encode", 1, url_encode},
    {"url_min_encode", 1, url_min_encode},
    {"url_decode", 1, url_decode},
    {"javascript_encode", 1, javascript_encode},
    {"int_to_string", 1, int_to_string},
    {"int64_to_string", 1, int64_to_string},
    {"uint_to_string", 1, uint_to_string},
    {"uint64_to_string", 1, uint64_to_string},
    {"double_to_string_fixed", 2, double_to_string_fixed},
    {"double_to_string_variable", 2, double_to_string_variable},
    {"to_upper", 1, to_upper},
    {"to_lower", 1, to_lower},
    {"to_print", 1, to_print}
};

ERL_NIF_INIT(erl_string_encoders, nif_functions, NULL, NULL, NULL, NULL);
