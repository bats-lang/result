# result

Result type for error handling in the [Bats](https://github.com/bats-lang) programming language.

## Features

- `result(a, e)`: linear sum type for success (`ok(a)`) or error (`err(e)`)
- `option(a)`: optional value (`some(a)` or `none()`)
- Unwrap helpers: `unwrap_or`, `option_unwrap_or`
- Discard helpers for consuming unused results

## Usage

```bats
#use result as R

val r: $R.result(int, int) = $R.ok(42)
case+ r of
| ~$R.ok(v) => println! ("got: ", v)
| ~$R.err(e) => println! ("error: ", e)
```

## API

See [docs/lib.md](docs/lib.md) for the full API reference.

## Safety

Safe library — no `$UNSAFE`, no `$extfcall`.
