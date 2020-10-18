% erl-jsv

# Introduction
JSV, for JSON Structural Validator, provides a way to validate the structure
and content of JSON data structures.

# Definitions
A definition indicates the data type and attributes of a value, modelled as a
set of constraints. Definitions are represented either as a `{Type,
Constraints}` tuple or simply as a `Type` atom when there is no constraint to
define.

For example, the following definition represents arrays containing at least 3
strictly positive integers:
```erlang
{array, #{element_type => {integer, #{min => 1}},
          min_length => 3}}
```

A value is said to be valid according to a definition if and only if:

- it matches the data type expected by the definition type;
- it satisfies all definition constraints.

# Types
Types are implemented as Erlang modules which implement the following
fundamental operations:

- Verifying constraints name and values.
- Formatting constraint violations.
- Validating value types.
- Validating values against constraints.

## Default types
### null
Represents a `null` JSON constant. This type does not support any constraint.

### boolean
Represents a JSON boolean value.

Constraints:

- `value`: a boolean constant the value must be equal to.

### integer
Represents a JSON number which was parsed as an integer.

Constraints:

- `min`: the minimal value of the integer.
- `max`: the maximal value of the integer.

### number
Represents a JSON number.

Constraints:

- `min`: the minimal value of the number.
- `max`: the maximal value of the number.

### string
Represents a JSON string.

Constraints:

- `min_length`: the minimal length of the string.
- `max_length`: the maximum length of the string.

Note that the length of a string is the number of characters in the string,
and not the number of bytes in its representation. Since erl-jsv relies on the
erl-json library, all JSON strings are represented by UTF-8 binaries: the
length of a string is the number of Unicode codepoints.

### array
Represents a JSON array.

Constraints:

- `element_type`: the definition that must apply to all elements.
- `min_length`: the minimal number of elements in the array.
- `max_length`: the maximum number of elements in the array.

### object
Represents a JSON object.

Constraints:

- `value_type`: the definition that must apply to all member values.
- `min_size`: the minimal number of members in the object.
- `max_size`: the maximum number of members in the object.

## Writing new types
**TODO**

# Errors
Validation errors are reported as maps containing information about the
precise value being incorrect and its location as a [JSON
pointer](https://tools.ietf.org/html/rfc6901). The `jsv:format_value_error/2`
and `jsv:format_value_errors/2` can be used to add textual information to
errors.

# Interface
## Validation
The `jsv:validate/2` and `jsv:validate/3` functions validate a value against a
definition.

Example:
```erlang
Definition = {array, #{element_type => {integer, #{min => 1}}}},
jsv:validate([1, 1, 3, 5], Definition).
```

For `jsv:validate/3`, the following options are available:

- `type_map`: the type map to use (default: `jsv:default_type_map()`).
- `format_value_errors`: if validation fails, add textual information to error
  values.

