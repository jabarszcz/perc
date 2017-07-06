#ifndef _PERC_JSON_HPP_
#define _PERC_JSON_HPP_

#include "perc_types.hpp"
#include "perc_json.h"

template <class T>
struct json_encoder {
	static int encode(struct encoder *e, ERL_NIF_TERM term);
};

template<>
int json_encoder<Integer>::encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_enc_integer(e, term);
}

template<>
int json_encoder<Float>::encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_enc_float(e, term);
}

template<>
int json_encoder<Atom>::encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_enc_atom(e, term);
}

template<>
int json_encoder<Binary>::encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_enc_binary(e, term);
}

template<>
int json_encoder<String>::encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_enc_string(e, term);
}

template<>
int json_encoder<Boolean>::encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_enc_boolean(e, term);
}

template <typename T>
struct json_encoder<Maybe<T>> {
	static int encode(struct encoder *e, ERL_NIF_TERM term) {
		if (!is_undefined(e, term))
			return json_encoder<T>::encode(e, term);
		return ENC_LITERAL(e, "null");
	}
};

template <typename T>
struct json_encoder<List<T>> {
	static int encode(struct encoder *e, ERL_NIF_TERM term) {
		if (!enif_is_list(e->env, term))
			return 0;

		if (!ENC_LITERAL(e, "["))
			return 0;

		unsigned int index = e->index;
		ERL_NIF_TERM head, tail=term;
		int first = 1;
		while(enif_get_list_cell(e->env, tail, &head, &tail)) {
			if (first)
				first = 0;
			else
				if (!ENC_LITERAL(e, ","))
					goto fail;
			if (!json_encoder<T>::encode(e, head))
				goto fail;
		}

		if (!ENC_LITERAL(e, "]"))
			goto fail;
		return 1;
	fail:
		e->index = index;
		return 0;
	}
};

template<int i = 0,
	 typename T,
	 typename ... Ts>
static
int json_enc_tuple_rec(struct encoder *e, const ERL_NIF_TERM *fields)
{
	if (i) {
		if (!ENC_LITERAL(e, ","))
			return 0;
	} else {
		if (!ENC_LITERAL(e, "["))
			return 0;
	}

	if (!json_encoder<T>::encode(e, fields[i]))
		return 0;
	return json_enc_tuple_rec<i+1, Ts...>(e, fields);
}

template<int i>
static
int json_enc_tuple_rec(struct encoder *e, const ERL_NIF_TERM *fields)
{
	return ENC_LITERAL(e, "]");
}

template<typename ...Ts>
struct json_encoder<Tuple<Ts...>> {
	static int encode(struct encoder *e, ERL_NIF_TERM term) {
		int arity;
		const ERL_NIF_TERM *fields;
		unsigned int index = e->index;
		if (!enif_get_tuple(e->env, term, &arity, &fields))
			return 0;
		if (!json_enc_tuple_rec<0, Ts...>(e, fields)) {
			e->index = index;
			return 0;
		}
		return 1;
	}
};

template<typename T>
static
int json_enc_union_rec(struct encoder *e, ERL_NIF_TERM term) {
	return json_encoder<T>::encode(e, term);
}

template<typename T1, typename T2, typename ...Ts>
static
int json_enc_union_rec(struct encoder *e, ERL_NIF_TERM term) {
	if (!json_encoder<T1>::encode(e, term))
		return json_enc_union_rec<T2, Ts...>(e, term);
	return 1;
}

template<typename... Ts>
struct json_encoder<Union<Ts...>> {
	static int encode(struct encoder *e, ERL_NIF_TERM term) {
		return json_enc_union_rec<Ts...>(e, term);
	}
};

template<typename T>
static
int json_encode(struct encoder *e, ERL_NIF_TERM term) {
	return json_encoder<T>::encode(e, term);
}

#endif // _PERC_JSON_HPP_
