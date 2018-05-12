#ifndef _NIF_UTILS_H_
#define _NIF_UTILS_H_

#include "erl_nif.h"

#ifndef NDEBUG
#define nif_utils_assert(env, test, message) _nif_utils_assert(env, test, message)
#else
#define nif_utils_assert(env, test, message)
#endif


static inline
bool _nif_utils_assert(ErlNifEnv* env, bool test, const char *message)
{
        if (!test) {
                ERL_NIF_TERM reason =
                        enif_make_string(env, message, ERL_NIF_LATIN1);
                enif_raise_exception(env, reason);
        }
        return test;
}

#endif // _NIF_UTILS_H_
