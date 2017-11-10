#ifndef _PERC_JSON_H_
#define _PERC_JSON_H_

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

#include "nif_utils.h"
#include "perc_encode.h"

#define INT_FMT "%ld"
#define FLOAT_FMT "%.15g"

static inline
int json_enc_undefined(struct encoder *e, ERL_NIF_TERM term)
{
        return ENC_LITERAL(e, "null")? 1 : -1;
}

static inline
int json_enc_integer(struct encoder *e, ERL_NIF_TERM term)
{
        ErlNifSInt64 val;

        if (!enif_get_int64(e->env, term, &val))
                return -1;

        if (!ensure(e, 32))
                return -1;

        size_t cap = capacity(e);
        static_assert( sizeof(long) == 8 );
        int ret = enif_snprintf(get_ptr(e), cap, INT_FMT, val);
        nif_utils_assert(
                e->env,
                ret < cap,
                "Encoded long integer string too long"
                );

        e->index += ret;
        return 1;
}

static inline
int json_enc_float(struct encoder *e, ERL_NIF_TERM term)
{
        double val;

        if (!enif_get_double(e->env, term, &val))
                return -1;

        if (!ensure(e, 32))
                return -1;

        size_t cap = capacity(e);
        int ret = enif_snprintf(get_ptr(e), cap, FLOAT_FMT, val);
        nif_utils_assert(
                e->env,
                ret < cap,
                "Encoded float string too long"
                );

        e->index += ret;
        return 1;
}

static inline
int json_enc_escape_buf(struct encoder *e, const char *buf, size_t len)
{
        size_t index = e->index, last = 0;
        for (size_t i = 0; i < len; ++i) {
                if (buf[i] == '\\' || buf[i] == '\"') {
                        if (!enc_buf(e, &buf[last], i-last))
                                goto fail;
                        if (!ENC_LITERAL(e, "\\"))
                                goto fail;
                        if (!enc_buf(e, &buf[i], 1))
                                goto fail;
                        last = i+1;
                } else if (0x00 <= buf[i] && buf[i] < 0x20) {
                        if (!enc_buf(e, &buf[last], i-last))
                                goto fail;
                        if (!ENC_LITERAL(e, "\\u00"))
                                goto fail;
                        char digits[3];
                        if (enif_snprintf(digits, 3, "%.2X", buf[i]) != 2)
                                goto fail;
                        if (!enc_buf(e, digits, 2))
                                goto fail;
                        last = i+1;
                }
        }

        if (!enc_buf(e, &buf[last], len-last))
                goto fail;
        return 1;
fail:
        e->index = index;
        return -1;
}

static inline
int json_enc_atom(struct encoder *e, ERL_NIF_TERM term)
{
        unsigned int len;
        size_t index = e->index;
        char atom[512];

        if (!enif_get_atom_length(e->env, term, &len, ERL_NIF_LATIN1))
                goto fail;
        if (!enif_get_atom(e->env, term, atom, 512, ERL_NIF_LATIN1))
                goto fail;
        if (!ENC_LITERAL(e, "\""))
                goto fail;
        if (json_enc_escape_buf(e, atom, len) < 0)
                goto fail;
        if (!ENC_LITERAL(e, "\""))
                goto fail;

        return 1;
fail:
        e->index = index;
        return -1;
}

static inline
int json_enc_binary(struct encoder *e, ERL_NIF_TERM term)
{
        ErlNifBinary bin;
        size_t index = e->index;
        if (!enif_inspect_binary(e->env, term, &bin))
                return -1;
        if (!ENC_LITERAL(e, "\""))
                goto fail;
        if (json_enc_escape_buf(e, (char*)bin.data, bin.size) < 0)
                goto fail;
        if (!ENC_LITERAL(e, "\""))
                goto fail;
        return 1;
fail:
        e->index = index;
        return -1;
}

static inline
int json_enc_string(struct encoder *e, ERL_NIF_TERM term)
{
        unsigned int len;
        size_t index = e->index;
        int ret = 0;

        if (!enif_get_list_length(e->env, term, &len))
                goto fail;
        if (!ENC_LITERAL(e, "\""))
                goto fail;
        if (!ensure(e, len+1))
                goto fail;
        ret = enif_get_string(e->env, term, get_ptr(e), len+1,
                              ERL_NIF_LATIN1); // TODO utf8
        if (ret != len+1)
                goto fail;
        e->index += len;
        if (!ENC_LITERAL(e, "\""))
                goto fail;
        return 1;
fail:
        e->index = index;
        return -1;
}

static inline
int json_enc_boolean(struct encoder *e, ERL_NIF_TERM term)
{
        if (enif_is_identical(term, e->true_atom))
                ENC_LITERAL(e, "true");
        else if (enif_is_identical(term, e->false_atom))
                ENC_LITERAL(e, "false");
        else
                return -1;
        return 1;
}

static inline
int json_enc_begin_obj(struct encoder *e)
{
        return ENC_LITERAL(e, "{");
}

static inline
int json_enc_end_obj(struct encoder *e)
{
        return ENC_LITERAL(e, "}");
}

static inline
int json_enc_key(struct encoder *e, bool &is_first,
                 const char *string, size_t len)
{
        int ret;
        if (is_first) {
                is_first = false;
                ret = ENC_LITERAL(e, "\"");
        } else {
                ret = ENC_LITERAL(e, ",\"");
        }
        return ret && enc_buf(e, string, len) && ENC_LITERAL(e, "\":");
}


#define JSON_ENC_KEY(e, is_first, field_string) \
        json_enc_key(e, is_first, field_string, sizeof(field_string)-1)

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

#endif //_PERC_JSON_H_
