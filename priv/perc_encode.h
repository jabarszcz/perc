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
	ERL_NIF_TERM opts;
};

static inline
int encoder_init(ErlNifEnv *env, struct encoder *e,
                 unsigned int size, ERL_NIF_TERM enc_opts)
{
	if (!enif_alloc_binary(size, &e->bin))
		return 0;

	e->index = 0;
	e->env = env;
	e->opts = enc_opts;

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

	while (size - e->index < len && size * 2 > size)
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
int enc_buf(struct encoder *e, const char *buf,
		 unsigned int len)
{
	if (!ensure(e, len))
		return 0;

	memcpy(get_ptr(e), buf, len);

	e->index += len;
	return 1;
}

#define ENC_LITERAL(e, str) enc_buf(e, str, sizeof(str)-1)

static inline
int is_undefined(struct encoder *e, ERL_NIF_TERM term)
{
	return enif_is_identical(term, e->undef_atom);
}

#endif // _PERC_ENCODE_H_
