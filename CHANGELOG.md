% erl-jsv changelog

# Next Version
## Features
- Introduce keywords, which are values of type binary, string or atom.
- Use keywords for member names in object constraints.
- Add a `values` constraint for strings.
## Misc
- Rename the `element_type` constraint to `element`.

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
