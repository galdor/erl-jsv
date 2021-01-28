% erl-jsv changelog

# Next Version
## Features
- Add `jsv:verify_catalog/1` and `jsv:verify_catalog/2`.
- Add experimental support for an extra set of data in type definitions. Use
  it for generation and validation functions.
- Add a `unique_elements` constraint for arrays.
- Add an `invalid_member_handling` option to treat object with a `members`
  constraint as records and define how to handle members which are not part of
  it.
- Add `jsv:verify_definition/1`.
- Introduce the `{any, Definitions}` definition combinator.
## Bugs
- Fix the type signature of `jsv:validate/2`.
- Delete catalog registry ETS tables on shutdown.
- Fix circular reference handling in definition verification.
## Misc
- Rename the `jsv:definition_error/0` type to `jsv:definition_error_reason/0`.
- Remove the entire notion of keywords; object member constraints and string
  value constraints must use atoms. This makes the code significantly simpler.

# 1.2.0
## Features
- Introduce keywords, which are values of type binary, string or atom.
- Use keywords for member names in object constraints.
- Add a `values` constraint for strings.
- Introduce definition catalogs.
- Introduce the definition catalog registry.
- Introduce the idea of canonical version. `jsv:validate/2` and
  `jsv:validate/3` now return `{ok, CanonicalValue}` on success.
- Introduce value generation, which creates valid JSON values from Erlang
  terms according to a definition.
- Add a type module for KSUID strings.
- Add a minimal `email_address` type.
## Bugs
- Handle values which are not atoms/strings/binaries in `jsv:is_keyword/1`.
- Fix handling of unknown `gen_server` calls.
## Misc
- Rename the `element_type` constraint to `element`.
- Rename the `value_type` constraint to `value`.
- Use the string representation for ksuids (erl-ksuid 1.1.x).

# 1.1.0
## Features
- Add the possibility for the `jsv_type:validate_type/1` callback to return
  `{ok, Value}` where `Value` is a term representing the value interpreted or
  parsed from the original JSON value. This value will be passed directly to
  the `jsv_type:validate_constraint/3` instead of the original JSON value.
- Add a type module for UUID strings.
- Add a type modules for URI strings.
- Add type modules for dates, times and datetimes.

# 1.0.0
First public version.
