# KATT [![Build Status](https://secure.travis-ci.org/klarna/katt.png)](http://travis-ci.org/klarna/katt)

KATT (Klarna API Testing Tool) is an HTTP-based API testing tool for Erlang.


## Quick start

Use for shooting HTTP requests in a sequential order and verifying the response.
Any relevant difference between expected and actual responses will cause a
failure.

The builtin validator supports basic text validation and more advanced validation of HTTP headers and JSON structures.

The validator makes use of a few tags with special meaning:
<dl>
  <dt>"{{_}}"</dt>
  <dd>
    Match anything (i.e. no real validation, only check existence).
  </dd>
  <dt>"{{unexpected}}"</dt>
  <dd>
    Match nothing (i.e. no real validation, only check lack of existence)
  </dd>
  <dt>
    "{{&gt;key}}"</dt>
  <dd>
    Store value of the whole string (key must be unique within testcase)
  </dd>
  <dt>"{{&lt;key}}"</dt>
  <dd>
    Recall stored value.
  </dd>
</dl>

The "{{_}}" tag can also be used as a JSON object's property in order to
validate any other additional properties.

By default, the builtin validator will allow additional fields in an object
structure. To counteract that default, one can add `"{{_}}": "{{unexpected}}"`
inside the object, effectively making a rule no other properties beyond the
ones defined are expected.


## Examples

A simple example that will make requests to a third party server:

```bash
ERL_LIBS=deps erl -pa ebin -noshell -eval '
  ok = ssl:start(),
  ok = lhttpc:start(),
  BlueprintFile = "./doc/example-httpbin.apib",
  Params = [{hostname, "httpbin.org"}, {my_name, "Joe"}, {your_name, "Mike"}],
  io:format("~p~n", [katt:run(BlueprintFile, Params)]).
' -s init stop
```
... or run the code passed to -eval from the Erlang shell (assuming that you
have started the Erlang shell from the repo's root directory with `ERL_LIBS=deps
erl -pa ebin`).


## Interface

* `katt:run` to be called with
  * `filename`
  * `params` (optional)
    * `protocol`
    * `hostname`
    * `port`
    * `request_timeout`
    * `scenario_timeout`
  * `callbacks` (optional)
    * `parse` to be called with `headers`, `body`, `params`, `callbacks`
    * `request` to be called with `request`, `params`, `callbacks`
    * `validate` to be called with `expected`, `actual`, `params`, `callbacks`


## Contributing

A pull-request is most welcome. Please make sure that the following criteria are
fulfilled before making your pull-request:

* Include a description regarding what has been changed and why.
* Make sure that the changed or added functionality (if you modify code) is
  covered by unit tests.
* Make sure that all unit tests pass.


## License

[Apache 2.0](LICENSE)
