#ifndef _PERC_JSON_H_
#define _PERC_JSON_H_

// Filters
struct data {
        ErlNifEnv *env;
        ERL_NIF_TERM undef;
        ERL_NIF_TERM empty_list;
} data;

static inline
int load(ErlNifEnv * env, void** priv_data, ERL_NIF_TERM load_info)
{
        data.env = enif_alloc_env();
        data.undef = enif_make_atom(data.env, "undefined");
        data.empty_list = enif_make_list(data.env, 0);
        return 0;
}

static inline
int no_undef(ErlNifEnv *env, ERL_NIF_TERM term, ERL_NIF_TERM opts)
{
        return !enif_is_identical(data.undef, term);
}

static inline
int no_empty_list(ErlNifEnv *env, ERL_NIF_TERM term, ERL_NIF_TERM opts)
{
        return !enif_is_identical(data.empty_list, term);
}

static inline
int no_empty_binary(ErlNifEnv *env, ERL_NIF_TERM term, ERL_NIF_TERM opts)
{
        ErlNifBinary bin;
        if (!enif_inspect_binary(env, term, &bin))
                return 1;
        return bin.size != 0;
}

#endif //_PERC_JSON_H_
