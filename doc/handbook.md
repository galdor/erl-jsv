% erl-jsv

# Introduction
JSV, for JSON Structural Validator, provides a way to validate the structure
and content of JSON data structures.

# Definitions
A definition indicates the data type and attributes of a value, modelled as a
set of constraints. Definitions are represented either as a `{Type,
Constraints}` tuple or simply as a `Type` atom when there is no constraint to
define.

Definitions are represented as one of the following values:
- `Type`: a type assertion without any constraint.
- `{Type, Constraints}`: a type assertion with a set of constraints.
- `{ref, DefinitionName}`: a reference to a definition in the current catalog.
- `{ref, CatalogName, DefinitionName}`: a reference to a definition from a
  catalog.

For example, the following definition represents arrays containing at least 3
strictly positive integers:
```erlang
{array, #{element => {integer, #{min => 1}},
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

# Catalogs
Catalogs are collection of definitions which can be referenced from any other
definitions.

Example:
```erlang
#{user_id => uuid,
  user_name => {string, #{min_length => 2, max_length => 64}}}
```

Catalogs are passed to verification and validation functions.

Example:
```erlang
jsv:validate(42, {ref, api, score},
             #{catalogs => #{api => #{score => integer}}}).
```

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

- `element`: the definition that must apply to all elements.
- `min_length`: the minimal number of elements in the array.
- `max_length`: the maximum number of elements in the array.

### object
Represents a JSON object.

Constraints:

- `value`: the definition that must apply to all member values.
- `min_size`: the minimal number of members in the object.
- `max_size`: the maximum number of members in the object.
- `required`: a list of member names the object must contain.
- `members`: a map associating member name to definition; member values must
  match the associated definitions if they are present in the value.

### uuid
Represents a [UUID](https://tools.ietf.org/html/rfc4122) string. This type
does not support any constraint.

Example: `"21fba787-4471-422d-bc94-63521e1181da"`.

### ksuid
Represents a [KSUID](https://github.com/segmentio/ksuid) string. This type
does not support any constraint.

Example: `"1l0UE6izCgIw533MOupkAowglGJ"`.

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

# Canonical values
Validation functions return a canonical version of the input value. Canonical
versions are easier to work in Erlang with, and having them remove the need
for lots of the usual data massaging steps.

Values are canonicalized as follows:

- Date values: the canonical form is an Erlang value of type
  `calendar:date()`. Example: `"2010-08-01"` is canonicalized to `{2010, 8,
  1}`.
- Time values: the canonical form is an Erlang value of type
  `calendar:time()`. Example: `"20:10:30"` is canonicalized to `{20, 10, 30}`.
- Datetime values: the canonical form is an Erlang value of type
  `calendar:datetime()`. Example: `"2010-08-01T14:45:00+01:00"` is
  canonicalized to `{{2010, 8, 1}, {13, 45, 0}}`.
- UUID and KSUID values: the canonical form is the binary representation.
- Arrays: the canonical form is an Erlang list containing the canonical form
  of each element of the array.
- Objects: the canonical form is an Erlang map. For each member:
  - If the key is part of the set of keys defined by a `members` constraint,
    it is converted to an atom; if not, it is converted to a binary.
  - If the definition contains a `value` constraint or if the key is part of
    the set of keys defined by a `members` constraint, the value is converted
    to its canonical version; if not, it is unaffected by canonicalization.
- Strings: the canonical form is either a binary, or an atom if the value has
  a `values` constraint.

Other values are unaffected by canonicalization.

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
Definition = {array, #{element => {integer, #{min => 1}}}},
jsv:verify_definition(Definition, jsv:default_type_map()).
```

## Validation
The `jsv:validate/2` and `jsv:validate/3` functions validate a value against a
definition.

Example:
```erlang
Definition = {array, #{element => {integer, #{min => 1}}}},
jsv:validate([1, 1, 3, 5], Definition).
```

Validation functions start by verifying the definition. If verification fails,
a `{invalid_definition, Errors}` error is signaled. Verification can be
disabled with the `disable_verification` option. Note that invalid definitions
can cause obscure errors; definitions should always be verified, either
automatically by validation functions or separately by calling
`jsv:verify_definition/2`.

If validation succeeds, these functions return `{ok, CanonicalValue}` where
`CanonicalValue` is the canonized version of the value passed in input.

### Options
For `jsv:validate/3`, the following options are available:

- `type_map`: the type map to use (default: `jsv:default_type_map()`).
- `format_value_errors`: if validation fails, add textual information to error
  values.
- `disable_verification`: do not verify the definition before validation.
- `catalogs`: a set of catalogs to use during verification and validation.

## Generation
The `jsv:generate/2` and `jsv:generate/3` functions generate JSON values from
Erlang terms using a definition. This makes it easier to convert canonical
representations to valid JSON strings.

For example:
```erlang
Definition = {object, #{members => #{a => integer,
                                     b => {string, #{values => [foo, bar]}},
                                     c => ksuid,
                                     d => date}}},
jsv:generate(#{a => 2,
               b => foo,
               c => <<12,87,174,185,56,8,84,101,32,110,233,137,39,77,248,128,10,113,46,87>>,
               d => {2020, 12, 4}}, Definition).
```
Returns:
```erlang
{ok, #{<<"a">> => 2,
       <<"b">> => <<"foo">>,
       <<"c">> => <<"1lBaURQi3YcGvvNkAD6vVrp6mGN">>,
       <<"d">> => <<"2020-12-04">>}}
```

During generation, value are handled according to their definition type:
- For type `any`, values are returned without any validation or conversion.
- For types `boolean`, `null`, `number` and `integer`, values are type checked
  and returned without any conversion.
- For type `string`, values are type checked and converted to binaries. Atoms,
  strings and binaries are accepted.
- For type `array`, values are type checked and returned with each array
  element generated using the `value` constraint if it exists, or the `any`
  type if it does not.
- For type `object`, values are type checked and returned with each key
  converted to a binary (with atoms, strings and binaries accepted as input)
  and each value generated using the corresponding `members` or `values`
  constraint if they exist, or the `any` type if they do not.
- For types `uuid` and `ksuid`, values are type checked, formatted and
  converted to binaries.
- For type `uri`, values are type checked and returned without any
  conversion. Only binaries are accepted.
- For types `time`, `date` and `datetime`, values are type checked, formatted,
  and converted to binaries.

Note that generation function only check types, not constraints, for two
reasons:
- It would be quite complex due to the multiple necessary type conversions.
- The impact on performances would be significant. Paying this price to ensure
  data are validated in input makes sense, but doing it for the output is not
  as beneficial.
