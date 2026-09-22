# Erlang Web 2.0.0 validation

Validated on 2026-09-23 against baseline commit `2e5c2c9`.

| Check                              | Result                                                                                                                                                      |
| ---------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Bundled jQuery                     | Official full 4.0.0 browser build; SHA-256 matches `lib/wparts-2.0.0/priv/JQUERY.md`                                                                        |
| Browser regression fixture         | 10/10 passed in Chromium 153.0.8010.12, Firefox 155.0, and WebKit 26.6; no uncaught page errors                                                             |
| Real browser interactions          | Typing, arrows, Enter, Tab, hover and click passed in all three engines                                                                                     |
| Fresh setup                        | `bin/start.erl` succeeded; both copied JavaScript assets match the bundle byte for byte                                                                     |
| Existing deployment                | Setup preserved old assets; explicit paired replacement installed the new assets byte for byte                                                              |
| Application metadata               | eptic, wpart and wparts load as 2.0.0 and resolve their renamed directories                                                                                 |
| File/path checks                   | Other tracked file contents preserved except the intended changes; no obsolete active version paths; JavaScript syntax checks and `git diff --check` passed |
| Original GitHub vulnerability scan | **Pending; not run on this local revision**                                                                                                                 |

Browser tests load the production files without Migrate. Reproduction instructions
are in `lib/wparts-2.0.0/tests/browser/README.md`. Browser tooling and downloads
were kept outside the repository.

## Existing Erlang runtime failures

The baseline and updated source were tested in separate disposable copies, both
using Erlang/OTP 28.1.1. Neither is a successful full build on this runtime:

- `bin/compile.erl` rejects the legacy `-spec` syntax in `eptic.erl`. Build logs
  are identical after normalizing the renamed version paths. The script exits
  zero despite reporting compilation errors, so its exit status is insufficient.
- The wparts runner reaches the same build failure. Explicitly running EUnit
  reports six `undef` failures in `datetime_format_tests`, on both copies,
  because the application modules were not built.
- After generating startup files, `bin/test.erl` fails during application startup
  and subsequently in `port_close/1` on both copies. The test compilation command
  succeeds, but excludes the framework applications and is not evidence that
  framework tests passed.

These pre-existing failures were not changed as part of the jQuery upgrade.
A successful full Erlang regression run still requires a compatible legacy
runtime or a separate OTP compatibility update.

## Outstanding release gate

The public GitHub Actions workflow list for `esl/erlang-web` returned zero
workflows. No authenticated scanner execution was available, and the changed
revision has not been pushed. The local checksum, version and security regression
checks do **not** substitute for the original scanner.

Before publishing 2.0.0, run the original GitHub scanner on the changed revision,
confirm the jQuery alert is resolved, and attach its result to the release review.
No alert suppression, release tag or publication was performed.
