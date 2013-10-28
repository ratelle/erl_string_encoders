#include <erl_nif.h>
#include <modp_b64w.h>

#include <stdio.h>

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

static ERL_NIF_TERM
web_safe_base64_encode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ErlNifBinary result;
    int result_len;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    enif_alloc_binary(modp_b64w_encode_len(input.size), &result);

    if((result_len = modp_b64w_encode((char *)result.data, (const char *)input.data, input.size)) == -1) {
        enif_release_binary(&result);
        return make_error(env, "encode_failed");
    }

    if(!enif_realloc_binary(&result, result_len)) {
        enif_release_binary(&result);
        return make_error(env, "out_of_memory");
    }

    return enif_make_binary(env, &result);
}

static ERL_NIF_TERM
web_safe_base64_decode(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifBinary input;
    ErlNifBinary result;
    int result_len;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &input)) {
        return enif_make_badarg(env);
    }

    enif_alloc_binary(modp_b64w_decode_len(input.size), &result);

    if((result_len = modp_b64w_decode((char *)result.data, (const char *)input.data, input.size)) == -1) {
        enif_release_binary(&result);
        return make_error(env, "encode_failed");
    }

    if(!enif_realloc_binary(&result, result_len)) {
        enif_release_binary(&result);
        return make_error(env, "out_of_memory");
    }

    return enif_make_binary(env, &result);
}

static ErlNifFunc nif_functions[] = {
    {"web_safe_base64_encode", 1, web_safe_base64_encode},
    {"web_safe_base64_decode", 1, web_safe_base64_decode}
};

ERL_NIF_INIT(erl_string_encoders, nif_functions, NULL, NULL, NULL, NULL);
