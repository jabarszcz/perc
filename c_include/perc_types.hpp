#ifndef _PERC_TYPES_HPP_
#define _PERC_TYPES_HPP_

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

#endif //_PERC_TYPES_HPP_
