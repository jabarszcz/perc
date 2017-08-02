#ifndef _PERC_TYPES_HPP_
#define _PERC_TYPES_HPP_

#include "erl_nif.h"

enum Id : unsigned int;

class Integer;
class Float;
class Atom;
class Binary;
class String;
class Boolean;

template <typename T>
class Maybe;

template <typename T>
class List;

template <typename... Ts>
class Tuple;

template <typename... Ts>
class Union;

template <Id id>
class Record;

template <Id id>
class UserType;

typedef ERL_NIF_TERM (*trans_func)(ErlNifEnv *, ERL_NIF_TERM);

template <trans_func enc, trans_func dec, typename T>
class Function;

#endif //_PERC_TYPES_HPP_
