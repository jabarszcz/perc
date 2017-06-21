#ifndef _PERC_JSON_H_
#define _PERC_JSON_H_

#include "perc_encode.h"

#define INT_FMT "%ld"
#define FLOAT_FMT "%.15g"

static inline
int json_enc_integer(struct encoder *e, ERL_NIF_TERM term)
{
	ErlNifSInt64 val;

	if (!enif_get_int64(e->env, term, &val))
		return 0;

	unsigned int cap = capacity(e);

	// TODO portability with length of longs: see jiffy
	int ret = enif_snprintf(get_ptr(e), cap, INT_FMT, val);
	if (ret >= cap) {
		if (!ensure(e, ret+1))
			return 0;
		ret = enif_snprintf(get_ptr(e), ret+1, INT_FMT, val);
	}

	e->index += ret;
	return 1;
}

static inline
int json_enc_float(struct encoder *e, ERL_NIF_TERM term)
{
	double val;

	if (!enif_get_double(e->env, term, &val))
		return 0;

	unsigned int cap = capacity(e);

	int ret = enif_snprintf(get_ptr(e), cap, FLOAT_FMT, val);
	if (ret >= cap) {
		if (!ensure(e, ret+1))
			return 0;
		ret = enif_snprintf(get_ptr(e), ret+1, FLOAT_FMT, val);
	}

	e->index += ret;
	return 1;
}

static inline
int json_enc_atom(struct encoder *e, ERL_NIF_TERM term)
{
	unsigned int len, index = e->index;
	int ret = 0;

	if (!enif_get_atom_length(e->env, term, &len, ERL_NIF_LATIN1))
		goto fail;
	if (!ENC_LITERAL(e, "\""))
		goto fail;
	if (!ensure(e, len+1))
		goto fail;
	ret = enif_get_atom(e->env, term, get_ptr(e),
			    len+1, ERL_NIF_LATIN1);
	if (ret != len + 1)
		goto fail;
	e->index += len;
	if (!ENC_LITERAL(e, "\""))
		goto fail;

	return 1;
fail:
	e->index = index;
	return 0;
}

static inline
int json_enc_binary(struct encoder *e, ERL_NIF_TERM term)
{
	ErlNifBinary bin;
	unsigned int index = e->index;
	if (!enif_inspect_binary(e->env, term, &bin))
		return 0;
	if (!ENC_LITERAL(e, "\""))
		goto fail;
	if (!enc_buf(e, (char*)bin.data, bin.size))
		goto fail;
	if (!ENC_LITERAL(e, "\""))
		goto fail;
	return 1;
fail:
	e->index = index;
	return 0;
}

static inline
int json_enc_string(struct encoder *e, ERL_NIF_TERM term)
{
	unsigned int len, index = e->index;
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
	return 0;
}

static inline
int json_enc_boolean(struct encoder *e, ERL_NIF_TERM term)
{
	if (enif_is_identical(term, e->true_atom))
		ENC_LITERAL(e, "true");
	else if (enif_is_identical(term, e->false_atom))
		ENC_LITERAL(e, "false");
	else
		return 0;
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


#define JSON_ENC_KEY(e, first, field_string) do {			\
		if (first) {						\
			ENC_LITERAL(e, "\"" field_string "\":");	\
			first = 0;					\
		} else {						\
			ENC_LITERAL(e, ",\"" field_string "\":");	\
		}							\
	} while (0)


#endif //_PERC_JSON_H_
