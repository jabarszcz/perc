#ifndef _PERC_ENCODER_HPP_
#define _PERC_ENCODER_HPP_

#include <stdio.h>

#include <stddef.h>
#include <string.h>
#include "erl_nif.h"

#define AD(name) ERL_NIF_TERM name ## _atom
#define AI(name) this-> name ## _atom = enif_make_atom(env, #name)

struct Encoder {
        ErlNifBinary bin;
        size_t index = 0;
        size_t match_index = 0;
        ErlNifEnv *env;
        ERL_NIF_TERM true_atom;
        ERL_NIF_TERM false_atom;
        AD(undefined);
        AD(int);
        AD(float);
        AD(atom);
        AD(binary);
        AD(string);
        AD(bool);
        AD(list);
        AD(tuple);
        ERL_NIF_TERM opts;

        bool init(ErlNifEnv *env, size_t size, ERL_NIF_TERM enc_opts) {
                this->env = env;
                this->opts = enc_opts;
                this->true_atom = enif_make_atom(env, "true");
                this->false_atom = enif_make_atom(env, "false");
                AI(undefined);
                AI(int);
                AI(float);
                AI(atom);
                AI(binary);
                AI(string);
                AI(bool);
                AI(list);
                AI(tuple);
                return nif_utils_assert(
                        env,
                        enif_alloc_binary(size, &this->bin),
                        "Could not allocate the encoder binary"
                        );
        }

        ERL_NIF_TERM binary() {
                nif_utils_assert(
                        this->env,
                        enif_realloc_binary(&this->bin, this->index),
                        "Could not re-allocate the encoder binary");
                return enif_make_binary(this->env, &this->bin);
        }

        bool ensure(size_t len) {
                size_t size, orig;
                size = orig = this->bin.size;
                if (size - this->index >= len)
                        return true;
                while (size - this->index < len && size * 2 > size)
                        size *= 2;

                return nif_utils_assert(
                        this->env,
                        size > orig && enif_realloc_binary(&this->bin, size),
                        "Could not re-allocate the encoder binary");
        }

        inline
        size_t capacity() {
                return this->bin.size - this->index;
        }

        inline
        char *get_ptr() {
                return (char *) &this->bin.data[this->index];
        }

        inline
        bool copy_in(const char *buf, size_t len) {
                if (!this->ensure(len))
                        return false;
                memcpy(this->get_ptr(), buf, len);
                this->index += len;
                return true;
        }

        #define COPY_IN(str) copy_in(str, sizeof(str)-1)
};

#endif // _PERC_ENCODER_HPP_
