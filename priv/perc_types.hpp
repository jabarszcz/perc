#ifndef _PERC_TYPES_HPP_
#define _PERC_TYPES_HPP_

#include "erl_nif.h"

enum Id : unsigned int;

// Nil, Null, None, empty value
class Undefined;

// Basic types
class Integer;
class Float;
class Atom;
class Binary;
class String;
class Boolean;

// Compound types
template <typename T>
class List;

template <typename... Ts>
class Tuple;

template <typename... Ts>
class Union;

// Reference types
template <Id id>
class Record;

template <Id id>
class UserType;

// Function type for presentation logic
typedef ERL_NIF_TERM (*trans_func)(ErlNifEnv *, ERL_NIF_TERM, ERL_NIF_TERM);

template <trans_func enc, trans_func dec, typename T>
class Function;

#endif //_PERC_TYPES_HPP_
