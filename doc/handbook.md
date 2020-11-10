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

Types are organized in type maps which associate the atom representing the
type to the module implementing the type.

The default type map, returned by `jsd:default_type_map/0`, contains a set of
default types useful for various kinds of JSON data. Users are free to extend
it or replace it altogether.

## Default types
### any
Represents any JSON value.

Constraints:

- `value`: a JSON value the value must be equal to.

### null
Represents a `null` JSON constant. This type does not support any constraint.

### boolean
Represents a JSON boolean value. This type does not support any constraint.

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
- `prefix`: a prefix the string must start with.
- `suffix`: a suffix the string must end with.
- `values`: a list of possible values of the string.

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
- `required`: a list of member names the object must contain.
- `members`: a map associating member name to definition; member values must
  match the associated definitions if they are present in the value.

### uuid
Represents a [UUID](https://tools.ietf.org/html/rfc4122) string. This type
does not support any constraint.

Example: `"21fba787-4471-422d-bc94-63521e1181da"`.

### uri
Represents a [URI](https://tools.ietf.org/html/rfc3986) string. This type
does not support any constraint.

Example: `"http://example.com/foo/bar?a=1"`.

### time
Represents a partial time (or time of day) as defined in [RFC
3339](https://tools.ietf.org/html/rfc3339), without any fractional seconds.

Example: `"14:45:00"`.

Constraints:

- `min`: the minimal time of day represented by the string.
- `max`: the maximal time of day represented by the string.

### date
Represents a full date as defined in [RFC
3339](https://tools.ietf.org/html/rfc3339).

Example: `"2010-08-01"`.

Constraints:

- `min`: the minimal date represented by the string.
- `max`: the maximal date represented by the string.

### datetime
Represents a full date and time string as defined in [RFC
3339](https://tools.ietf.org/html/rfc3339).

Example: `"2010-08-01T14:45:00+01:00"`.

Note that due to limitations of the Erlang `calendar` module, fractional
seconds in the input string are ignored.

Constraints:

- `min`: the minimal datetime represented by the string.
- `max`: the maximal datetime represented by the string.

## Writing new types
**TODO**

# Errors
Validation errors are reported as maps containing information about the
precise value being incorrect and its location as a [JSON
pointer](https://tools.ietf.org/html/rfc6901). The `jsv:format_value_error/2`
and `jsv:format_value_errors/2` can be used to add textual information to
errors.

# Interface
## Definition verification
The `jsv:verify_definition/2` function verifies a definition, making sure
that:

- its structure is correct;
- referenced types exist in the type map passed to the function;
- constraints exist for the associated type;
- constraint values are valid.

Example:
```erlang
Definition = {array, #{element_type => {integer, #{min => 1}}}},
jsv:verify_definition(Definition, jsv:default_type_map()).
```

## Validation
The `jsv:validate/2` and `jsv:validate/3` functions validate a value against a
definition.

Example:
```erlang
Definition = {array, #{element_type => {integer, #{min => 1}}}},
jsv:validate([1, 1, 3, 5], Definition).
```

Validation functions start by verifying the definition. If verification fails,
a `{invalid_definition, Errors}` error is signaled. Verification can be
disabled with the `disable_verification` option. Note that invalid definitions
can cause obscure errors; definitions should always be verified, either
automatically by validation functions or separately by calling
`jsv:verify_definition/2`.

### Options
For `jsv:validate/3`, the following options are available:

- `type_map`: the type map to use (default: `jsv:default_type_map()`).
- `format_value_errors`: if validation fails, add textual information to error
  values.
- `disable_verification`: do not verify the definition before validation.
