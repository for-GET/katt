# KATT [![Build Status][2]][1]

KATT (Klarna API Testing Tool) is an HTTP-based API testing tool for Erlang.


## Quick start

Use for shooting HTTP requests in a sequential order and verifying the response.
Any relevant difference between expected and actual responses will cause a
failure.

The builtin validator supports basic text validation and more advanced validation of HTTP headers and JSON structures.

The validator makes use of a few tags with special meaning:

`"{{_}}"`  
Match anything (i.e. no real validation, only check existence).

`"{{unexpected}}"`  
Match nothing (i.e. no real validation, only check lack of existence)

`"{{>key}}"`  
Store value of the whole string (key must be unique within testcase)

`"{{<key}}"`  
Recall stored value.

The `"{{_}}"` tag can also be used as a JSON object's property in order to
validate any other additional properties.

By default, the builtin validator will allow additional properties in an object
structure, or additional items in an array structure. To counteract that
default, one can do `{..., "{{_}}": "{{unexpected}}"}` or
`[..., "{{unexpected}}"]`, effectively making a rule that no properties/items
are expected beyond the ones defined.


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


## CLI

You can also fire up `katt` from the CLI, with
```bash
katt ./doc/example-httpbin.apib hostname=httpbin.org my_name=Joe your_name=Mike
```

If you want non-string params, use `:=` as a separator e.g. `my_int:=123`.

You can also output the result in JSON format, with
```bash
katt --json ./doc/example-httpbin.apib hostname=httpbin.org my_name=Joe your_name=Mike
```

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
    * `recall` to be called with `syntax`, `text`, `params`, `callbacks`
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


  [1]: https://travis-ci.org/klarna/katt
  [2]: https://travis-ci.org/klarna/katt.png
