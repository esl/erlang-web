# Bundled jQuery

- Version: 4.0.0, full uncompressed browser/UMD build (includes AJAX).
- Source: https://code.jquery.com/jquery-4.0.0.js
- SHA-256: `f5fb077959ca06faa1dc50761d8bbb836c6c78067932537a2b3fea9e401257c5`
- License: MIT, https://jquery.com/license/ (copyright and license notice
  retained in `jquery.js`).

The upstream file is vendored unchanged as `jquery.js`. Verify it from the
repository root with:

```sh
shasum -a 256 lib/wparts-2.0.0/priv/jquery.js
```

`jquery.autocomplete.js` is the separately maintained Erlang Web plugin.
Deploy both files together; see `doc/NEXT_RELEASE` for migration instructions.
