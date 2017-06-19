#ifndef _PERC_ENCODE_H_
#define _PERC_ENCODE_H_

#include <string.h>
#include "erl_nif.h"

struct encoder {
	ErlNifBinary bin;
	unsigned int size;
	unsigned int index;
	ErlNifEnv *env;
	ERL_NIF_TERM undef_atom;
	ERL_NIF_TERM true_atom;
	ERL_NIF_TERM false_atom;
};

typedef int (*enc_func)(struct encoder *e, ERL_NIF_TERM term);

#define INT_FMT "%ld"
#define FLOAT_FMT "%.15g"

static inline
int encoder_init(ErlNifEnv *env, struct encoder *e,
			     unsigned int size)
{
	if (!enif_alloc_binary(size, &e->bin))
		return 0;

	e->index = 0;
	e->env = env;

	e->undef_atom = enif_make_atom(env, "undefined");
	e->true_atom = enif_make_atom(env, "true");
	e->false_atom = enif_make_atom(env, "false");

	return 1;
}

static inline
ERL_NIF_TERM encoder_binary(struct encoder *e)
{
	enif_realloc_binary(&e->bin, e->index);
	return enif_make_binary(e->env, &e->bin);
}

static inline
int ensure(struct encoder *e, unsigned int len)
{
	unsigned int size = e->bin.size;

	if (size - e->index >= len)
		return 1;

	while (size - e->index < len)
		size *= 2;

	return enif_realloc_binary(&e->bin, size);
}

static inline
unsigned int capacity(struct encoder *e)
{
	return e->bin.size - e->index;
}

static inline
char *get_ptr(struct encoder *e)
{
	return (char *) &e->bin.data[e->index];
}

static inline
int json_enc_buf(struct encoder *e, const char *buf,
		 unsigned int len)
{
	if (!ensure(e, len))
		return 0;

	memcpy(get_ptr(e), buf, len);

	e->index += len;
	return 1;
}

#define JSON_ENC_LITERAL(e, str) json_enc_buf(e, str, sizeof(str)-1)

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

	if (!enif_get_atom_length(e->env, term, &len, ERL_NIF_LATIN1))
		return 0;
	if (!ensure(e, len+1))
		return 0;
	int ret = enif_get_atom(e->env, term, get_ptr(e),
				len+1, ERL_NIF_LATIN1);
	if (ret != len + 1) {
		e->index = index;
		return 0;
	}

	e->index += len;
	return 1;
}

static inline
int json_enc_binary(struct encoder *e, ERL_NIF_TERM term)
{
	ErlNifBinary bin;
	if (!enif_inspect_binary(e->env, term, &bin))
		return 0;
	if (!ensure(e, bin.size + 2))
		return 0;
	JSON_ENC_LITERAL(e, "\"");
	memcpy(get_ptr(e), bin.data, bin.size);
	e->index += bin.size;
	JSON_ENC_LITERAL(e, "\"");
	return 1;
}

static inline
int json_enc_string(struct encoder *e, ERL_NIF_TERM term)
{
	unsigned int len, index = e->index;

	if (!enif_get_list_length(e->env, term, &len))
		return 0;
	if (!ensure(e, len+1))
		return 0;
	JSON_ENC_LITERAL(e, "\"");
	int ret = enif_get_string(e->env, term, get_ptr(e), len+1,
				  ERL_NIF_LATIN1); // TODO utf8
	e->index += len;
	JSON_ENC_LITERAL(e, "\"");
	if (ret != len+1) {
		e->index = index;
		return 0;
	}
	return 1;
}

static inline
int json_enc_boolean(struct encoder *e, ERL_NIF_TERM term)
{
	if (enif_is_identical(term, e->true_atom))
		JSON_ENC_LITERAL(e, "true");
	else if (enif_is_identical(term, e->false_atom))
		JSON_ENC_LITERAL(e, "false");
	else
		return 1;
	return 0;
}

static inline
int is_undefined(struct encoder *e, ERL_NIF_TERM term)
{
	return enif_is_identical(term, e->undef_atom);
}

template<enc_func encode_function>
inline
int enc_maybe(struct encoder *e, ERL_NIF_TERM term)
{
	if (!is_undefined(e, term))
		return encode_function(e, term);
	return JSON_ENC_LITERAL(e, "null");
}

template<enc_func encode_function>
inline
int json_enc_list(struct encoder *e, ERL_NIF_TERM term)
{
	if (!enif_is_list(e->env, term))
		return 0;

	JSON_ENC_LITERAL(e, "[");

	ERL_NIF_TERM head, tail=term;
	int first = 1;
	while(enif_get_list_cell(e->env, tail, &head, &tail)) {
		if (first)
			first = 0;
		else
			JSON_ENC_LITERAL(e, ",");
		encode_function(e, head);
	}

	JSON_ENC_LITERAL(e, "]");
	return 1;
}

template<int i = 0,
	 enc_func first,
	 enc_func ...rest>
inline
int json_enc_tuple_rec(struct encoder *e,
		       const ERL_NIF_TERM *fields)
{
	if (i) {
		if (!JSON_ENC_LITERAL(e, ","))
			return 0;
	} else {
		if (!JSON_ENC_LITERAL(e, "["))
			return 0;
	}

	if (!first(e, fields[i]))
		return 0;

	return json_enc_tuple_rec<i+1, rest...>(e, fields);
}

template<int i>
inline
int json_enc_tuple_rec(struct encoder *e,
		       const ERL_NIF_TERM *fields)
{
	return JSON_ENC_LITERAL(e, "]");
}

template<enc_func ...encode_functions>
inline
int json_enc_tuple(struct encoder *e, ERL_NIF_TERM term)
{
	int arity;
	const ERL_NIF_TERM *fields;
	unsigned int index = e->index;
	if (!enif_get_tuple(e->env, term, &arity, &fields))
		return 0;
	if (!json_enc_tuple_rec<0, encode_functions...>(e, fields)) {
		e->index = index;
		return 0;
	}
	return 1;
}

template<enc_func encode_function>
inline
int enc_union(struct encoder *e, ERL_NIF_TERM term)
{
	return encode_function(e, term);
}


template<enc_func encode_function, enc_func second, enc_func ...rest>
inline
int enc_union(struct encoder *e, ERL_NIF_TERM term)
{
	if (!encode_function(e, term))
		return enc_union<second, rest...>(e, term);
}

static inline
void json_enc_begin_obj(struct encoder *e)
{
	JSON_ENC_LITERAL(e, "{");
}

static inline
void json_enc_end_obj(struct encoder *e)
{
	JSON_ENC_LITERAL(e, "}");
}


#define JSON_ENC_KEY(e, first, field_string) do {			\
		if (first) {						\
			JSON_ENC_LITERAL(e, "\"" field_string "\":");	\
			first = 0;					\
		} else {						\
			JSON_ENC_LITERAL(e, ",\"" field_string "\":");	\
		}							\
	} while (0)

#endif // _PERC_ENCODE_H_
