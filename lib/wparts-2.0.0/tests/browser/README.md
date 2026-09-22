# Browser regression tests

From the repository root, serve the actual bundled files over HTTP:

```sh
python3 -m http.server 8000 --bind 127.0.0.1
```

Open http://127.0.0.1:8000/lib/wparts-2.0.0/tests/browser/ in Chromium,
Firefox and WebKit (Safari). Keep the page focused while tests run. All rows
must say PASS and the console must contain no unexpected exceptions. No npm
dependencies, external CDN, legacy jQuery or Migrate are required by the fixture.

The suite exercises both local data and HTTP text responses. It uses the
production autocomplete template and assets directly. Automated browser runners
can wait for `window.autocompleteTestResults.done` and inspect its `results`
array; a failed assertion has `passed: false` and an `error` message.

Also smoke-test real typing, arrow keys, Enter/Tab, and pointer selection in an
application page after replacing its deployed jQuery and autocomplete assets.
