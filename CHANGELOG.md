# CHANGELOG

## 1.5.2 - 2016-04-04

### Notable changes

* fix crash with HEAD requests #47

[Full list of changes since 1.5.1](https://github.com/for-GET/katt/compare/for-GET:1.5.1...1.5.2)


## 1.5.1 - 2016-04-03

### Notable changes

* improvements to BARE_MODE #45

[Full list of changes since 1.5.0](https://github.com/for-GET/katt/compare/for-GET:1.5.0...1.5.1)


## 1.5.0 - 2016-03-18

### Breaking changes

* KATT CLI defaults now to showing only the last transaction, but understands `--all` for previous behaviour #31
* new Erlang pattern that includes error details #25

### Notable changes

* a BARE_MODE has been introduced, in order to build KATT with as few dependencies as possible #34 #38
* textual diff for strings #20 (new Erlang pattern that includes error details)
* added `runtime_value` and `runtime_validation` #29
* KATT CLI can now convert [an HTTP Archive](http://www.softwareishard.com/blog/har-12-spec/) to an APIB file, making it easier to create scenarios
* allow `katt:run` to take in a `#katt_blueprint` record, not just a path to an APIB file
* switched lhttpc for hackney #17
* switched mochijson3 for jsx

[Full list of changes since 1.4.0](https://github.com/for-GET/katt/compare/for-GET:1.4.0...1.5.0-rc)


## [1.4.0] - 2016-01-19

### Breaking changes

* allow chained scenarios in CLI KATT #19
* normalize set errors 40c2613e163aede46069d6a9fb2122d551adff35

### Notable changes

* handle duplicate headers #14
* handle request timeout #13
* fix katt compilation as rebar dependencies #24

[Full list of changes since 1.3.0](https://github.com/for-GET/katt/compare/for-GET:1.3.0...1.4.0)
[Full list of changes since 1.3.1-rc](https://github.com/for-GET/katt/compare/for-GET:1.3.1-rc...1.4.0)


## [1.3.0] - 2015-03-15

### Notable changes

* **standalone** katt executable (with Erlang/JSON output)
  * progress output to stderr
* complex/multiple store (match and store a substring in a param)
  * storing is optional as one can use {{_}} to match any substring
* support complex validations via a special `{ "{{type}}": "....", "type_specific_arg1": ..., ... }` pattern
  * built-in `set` comparison - JSON array with no specific order
* extensions (XML example in README)
* progress callback
* request timeout and transaction description can be set via HTTP request headers
* request can be delayed (put to sleep) via HTTP request header
* optimized, refactored, moved shit around, fixed bugs

[Full list of changes since 1.2.6](https://github.com/for-GET/katt/compare/for-GET:1.2.6...1.3.0)
[Full list of changes since 1.3.0-rc](https://github.com/for-GET/katt/compare/for-GET:1.3.0-rc...1.3.0)


## [1.2.6] - 2014-07-31
