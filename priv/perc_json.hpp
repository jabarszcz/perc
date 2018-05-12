#ifndef _PERC_JSON_HPP_
#define _PERC_JSON_HPP_

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

#include "perc_types.hpp"
#include "nif_utils.h"

#include "perc_encoder.hpp"
#include "perc_json.h"

#define INT_FMT "%ld"
#define FLOAT_FMT "%.15g"

#define BUF(str) str, (sizeof(str)-1)

struct encode_result {
        enum result_type {SUCCESS=0, MATCH_FAILURE, FATAL} type;
        union {
                struct {
                        int field_count;
                } success;
                struct {
                        ERL_NIF_TERM unmatched_term;
                        ERL_NIF_TERM target_type;
                        ERL_NIF_TERM field_name;
                        ERL_NIF_TERM record_name;
                        bool field_set;
                        int match_index;
                } failure;
        } content;

        encode_result() {
                this->type = SUCCESS;
                this->content.success.field_count = 0;
        }

        bool is_success() {
                return !this->type;
        }

        bool is_match_failure() {
                return this->type == MATCH_FAILURE;
        }

        bool is_not_empy() {
                return this->is_success() && this->content.success.field_count;
        }

        ERL_NIF_TERM get_reason(ErlNifEnv *env) {
                if (!nif_utils_assert(
                            env,
                            !this->is_success(),
                            "Tried to get failure reason of non-failed result"
                            ))
                        return enif_make_atom(env, "error");
                return enif_make_tuple5(
                        env,
                        enif_make_atom(env, "type_mismatch"),
                        this->content.failure.record_name,
                        this->content.failure.field_name,
                        this->content.failure.unmatched_term,
                        this->content.failure.target_type
                        );
        }

        inline
        encode_result merge(encode_result other) {
                if (!this->is_match_failure() || !other.is_match_failure())
                        return other;
                if (this->content.failure.match_index <
                    other.content.failure.match_index)
                        return other;
                return *this;
        }

        static inline
        encode_result succeed(){
                return succeed(1);
        }

        static inline
        encode_result succeed(int field_count){
                encode_result res;
                res.type = SUCCESS;
                res.content.success.field_count = field_count;
                return res;
        }

        static inline
        encode_result fail(ErlNifEnv *env,
                           ERL_NIF_TERM term,
                           ERL_NIF_TERM target,
                           int match_index) {
                encode_result res;
                res.type = MATCH_FAILURE;
                res.content.failure.unmatched_term = term;
                res.content.failure.target_type = target;
                res.content.failure.match_index = match_index;
                res.content.failure.field_set = false;
                return res;
        }

        static inline
        encode_result fatal() {
                encode_result res;
                res.type = FATAL;
                return res;
        }

        static inline
        encode_result make(bool success) {
                if (success)
                        return encode_result::succeed();
                else
                        return encode_result::fatal();
        }
};

template <class T>
struct json_encoder {
        static encode_result encode(Encoder *e, ERL_NIF_TERM term);
};

template<>
encode_result
json_encoder<Undefined>::encode(Encoder *e, ERL_NIF_TERM term) {
        if (!enif_is_identical(term, e->undefined_atom))
                return encode_result::fail(e->env, term, e->undefined_atom,
                                           e->match_index);
        e->copy_in(BUF("null"));
        e->match_index++;
        return encode_result::succeed();
}

template<>
encode_result
json_encoder<Integer>::encode(Encoder *e, ERL_NIF_TERM term) {
        ErlNifSInt64 val;

        if (!enif_get_int64(e->env, term, &val))
                return encode_result::fail(e->env, term, e->int_atom,
                                           e->match_index);

        if (!e->ensure(32))
                return encode_result::fatal();

        size_t cap = e->capacity();
        static_assert( sizeof(long) == 8, "sizeof(long) != 8" );
        int ret = enif_snprintf(e->get_ptr(), cap, INT_FMT, val);
        nif_utils_assert(
                e->env,
                ret < cap,
                "Encoded long integer string too long"
                );

        e->index += ret;
        e->match_index++;
        return encode_result::succeed();
}

template<>
encode_result
json_encoder<Float>::encode(Encoder *e, ERL_NIF_TERM term) {
        double val;

        if (!enif_get_double(e->env, term, &val))
                return encode_result::fail(e->env, term, e->float_atom,
                                           e->match_index);

        if (!e->ensure(32))
                return encode_result::fatal();

        size_t cap = e->capacity();
        int ret = enif_snprintf(e->get_ptr(), cap, FLOAT_FMT, val);
        nif_utils_assert(
                e->env,
                ret < cap,
                "Encoded float string too long"
                );

        e->index += ret;
        e->match_index++;
        return encode_result::succeed();
}

static inline
bool json_enc_escape_buf(Encoder *e, const char *buf, size_t len)
{
        size_t index = e->index, last = 0;
        for (size_t i = 0; i < len; ++i) {
                if (buf[i] == '\\' || buf[i] == '\"') {
                        if (!e->copy_in(&buf[last], i-last) ||
                            !e->copy_in(BUF("\\")) ||
                            !e->copy_in(&buf[i], 1))
                                goto fail;
                        last = i+1;
                } else if (0x00 <= buf[i] && buf[i] < 0x20) {
                        if (!e->copy_in(&buf[last], i-last) ||
                            !e->copy_in(BUF("\\u00")))
                                goto fail;
                        char digits[3];
                        if (enif_snprintf(digits, 3, "%.2X", buf[i]) != 2 ||
                            !e->copy_in(digits, 2))
                                goto fail;
                        last = i+1;
                }
        }

        if (!e->copy_in(&buf[last], len-last))
                goto fail;
        return true;
fail:
        e->index = index;
        return false;
}

#define MAX_ATOM_LENGTH 512

template<>
encode_result
json_encoder<Atom>::encode(Encoder *e, ERL_NIF_TERM term) {
        unsigned int len;
        size_t index = e->index;
        char atom[MAX_ATOM_LENGTH];

        if (!enif_get_atom_length(e->env, term, &len, ERL_NIF_LATIN1))
                return encode_result::fail(e->env, term, e->atom_atom,
                                           e->match_index);
        nif_utils_assert(e->env,
                         enif_get_atom(e->env, term, atom, MAX_ATOM_LENGTH,
                                       ERL_NIF_LATIN1),
                         "Atom string too long to fit in buffer");

        if (!e->copy_in(BUF("\"")) ||
            !json_enc_escape_buf(e, atom, len) ||
            !e->copy_in(BUF("\""))) {
                e->index = index;
                return encode_result::fatal();
        }

        e->match_index++;
        return encode_result::succeed();
}

template<>
encode_result
json_encoder<Binary>::encode(Encoder *e, ERL_NIF_TERM term) {
        ErlNifBinary bin;
        size_t index = e->index;
        if (!enif_inspect_binary(e->env, term, &bin))
                return encode_result::fail(e->env, term, e->binary_atom,
                                           e->match_index);
        if (!e->copy_in(BUF("\"")) ||
            !json_enc_escape_buf(e, (char*)bin.data, bin.size) ||
            !e->copy_in(BUF("\""))) {
                e->index = index;
                return encode_result::fatal();
        }

        e->match_index++;
        return encode_result::succeed();
}

template<>
encode_result
json_encoder<String>::encode(Encoder *e, ERL_NIF_TERM term) {
        unsigned int len;
        size_t index = e->index;
        int ret = 0;

        if (!enif_get_list_length(e->env, term, &len))
                goto badarg;
        if (!e->copy_in(BUF("\"")) ||
            !e->ensure(len+1))
                goto fatal;
        // TODO escape buffer
        ret = enif_get_string(e->env, term, e->get_ptr(), len+1,
                              ERL_NIF_LATIN1); // TODO utf8
        if (ret == 0)
                goto badarg;
        e->index += len;
        if (ret != len+1  ||
            !e->copy_in(BUF("\"")))
                goto fatal;
        e->match_index++;
        return encode_result::succeed();
fatal:
        e->index = index;
        return encode_result::fatal();
badarg:
        e->index = index;
        return encode_result::fail(e->env, term, e->string_atom,
                                   e->match_index);
}

template<>
encode_result
json_encoder<Boolean>::encode(Encoder *e, ERL_NIF_TERM term) {
        if (enif_is_identical(term, e->true_atom)) {
                if (!e->copy_in(BUF("true")))
                        return encode_result::fatal();
        } else if (enif_is_identical(term, e->false_atom)) {
                if (!e->copy_in(BUF("false")))
                        return encode_result::fatal();
        } else
                return encode_result::fail(e->env, term, e->bool_atom,
                                           e->match_index);
        e->match_index++;
        return encode_result::succeed();
}

template <typename T>
struct json_encoder<List<T>> {
        static encode_result encode(Encoder *e, ERL_NIF_TERM term) {
                if (!enif_is_list(e->env, term))
                        return encode_result::fail(e->env, term, e->list_atom,
                                                   e->match_index);

                size_t index = e->index;
                if (!e->copy_in(BUF("[")))
                        return encode_result::fatal();;

                ERL_NIF_TERM head, tail=term;
                bool is_first = true;
                while(enif_get_list_cell(e->env, tail, &head, &tail)) {
                        if (is_first) {
                                is_first = false;
                        } else {
                                if (!e->copy_in(BUF(",")))
                                        goto fail;
                        }
                        encode_result res = json_encoder<T>::encode(e, head);
                        if (!res.is_success()) {
                                e->index = index;
                                return res;
                        }
                }

                if (!e->copy_in(BUF("]")))
                        goto fail;
                e->match_index++;
                return encode_result::succeed();
        fail:
                e->index = index;
                return encode_result::fatal();
        }
};

template<int i = 0,
         typename T,
         typename ... Ts>
static
encode_result json_enc_tuple_rec(Encoder *e, const ERL_NIF_TERM *fields)
{
        if (i) {
                if (!e->copy_in(BUF(",")))
                        return encode_result::fatal();
        } else {
                if (!e->copy_in(BUF("[")))
                        return encode_result::fatal();
        }

        encode_result res = json_encoder<T>::encode(e, fields[i]);
        if (!res.is_success())
                return res;
        return json_enc_tuple_rec<i+1, Ts...>(e, fields);
}

template<int i>
static
encode_result json_enc_tuple_rec(Encoder *e, const ERL_NIF_TERM *fields)
{
        encode_result::make(e->copy_in(BUF("]")));
}

template<typename ...Ts>
struct json_encoder<Tuple<Ts...>> {
        static encode_result encode(Encoder *e, ERL_NIF_TERM term) {
                int arity;
                const ERL_NIF_TERM *fields;
                size_t index = e->index;
                if (!enif_get_tuple(e->env, term, &arity, &fields))
                        return encode_result::fail(e->env, term, e->tuple_atom,
                                                   e->match_index);
                encode_result res = json_enc_tuple_rec<0, Ts...>(e, fields);
                if (!res.is_success())
                        e->index = index;
                return res;
        }
};

template<typename T>
static
encode_result json_enc_union_rec(Encoder *e, ERL_NIF_TERM term, encode_result last) {
        return last.merge(json_encoder<T>::encode(e, term));
}

template<typename T1, typename T2, typename ...Ts>
static
encode_result json_enc_union_rec(Encoder *e, ERL_NIF_TERM term, encode_result last) {
        encode_result res = json_encoder<T1>::encode(e, term);
        encode_result new_last = last.merge(res);
        if (res.is_match_failure())
                return json_enc_union_rec<T2, Ts...>(e, term, new_last);
        return new_last;
}

template<typename... Ts>
struct json_encoder<Union<Ts...>> {
        // TODO keep best error according to e->match_index
        static encode_result encode(Encoder *e, ERL_NIF_TERM term) {
                return json_enc_union_rec<Ts...>(e, term, encode_result());
        }
};

template<trans_func enc, trans_func dec, typename T>
struct json_encoder<Function<enc, dec, T>> {
        static encode_result encode(Encoder *e, ERL_NIF_TERM term) {
                return json_encoder<T>::encode(e, enc(e->env, term, e->opts));
        }
};

template<typename T>
static
encode_result json_encode(Encoder *e, ERL_NIF_TERM term) {
        return json_encoder<T>::encode(e, term);
}

static inline
int json_enc_begin_obj(Encoder *e)
{
        return e->copy_in(BUF("{"));
}

static inline
int json_enc_end_obj(Encoder *e)
{
        return e->copy_in(BUF("}"));
}

static inline
int json_enc_key(Encoder *e, bool &is_first,
                 const char *string, size_t len)
{
        bool ret;
        if (is_first) {
                is_first = false;
                ret = e->copy_in(BUF("\""));
        } else {
                ret = e->copy_in(BUF(",\""));
        }
        return ret && e->copy_in(string, len) && e->copy_in(BUF("\":"));
}

#endif // _PERC_JSON_HPP_
