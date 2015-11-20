# KATT [![Build Status][2]][1]

KATT (Klarna* API Testing Tool) is an HTTP-based API testing tool for Erlang.

\* Albeit the "Klarna" mention, this repository is not affiliated with Klarna AB.
KATT was born at Klarna, and Klarna maintains copyright for parts of the code,
but it is now being maintained outside the company, by its authors and new contributors.


## Quick start

[An example is worth a 1000 words.](doc/example-httpbin.apib)

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
  application:ensure_all_started(katt),
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
bin/katt hostname=httpbin.org my_name=Joe your_name=Mike -- ./doc/example-httpbin.apib
```

If you want non-string params, use `:=` as a separator e.g. `my_int:=123`.

You can also output the result in JSON format, with `--json`, and beautify it e.g. with python
```bash
bin/katt --json hostname=httpbin.org my_name=Joe your_name=Mike -- ./doc/example-httpbin.apib | python -m json.tool
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
    * `ext` to be called with `scope` (recall_body, parse, validate_body, validate_type)
    * `recall` to be called with `syntax`, `text`, `params`, `callbacks`
    * `parse` to be called with `headers`, `body`, `params`, `callbacks`
    * `request` to be called with `request`, `params`, `callbacks`
    * `validate` to be called with `expected`, `actual`, `params`, `callbacks`
    * `progress` to be called with `transaction_result`

A request can also be configured via HTTP request headers:

* `x-katt-description` would take precedence over the transaction's description
* `x-katt-request-timeout` would take precedence over the `request_timeout` param
* `x-katt-request-sleep` would delay the request for a specific amount of milliseconds

### If you would like to disable JSON support

```erlang
OnlyText = fun(_Scope) -> [] end,
katt:run("text_only_scenario.apib", [], [{ext, OnlyText}]).
```

### If you would like to add XML support

```erlang
XmlJson =
  fun(recall_body) ->
    [ fun katt_callbacks_json:recall_body/4
    , fun custom_callbacks_xml:recall_body/4
    ];
  fun(parse) ->
    [ fun katt_callbacks_json:parse/5
    , fun custom_callbacks_xml:parse/5
    ];
  fun(validate_body) ->
    [ fun katt_callbacks_json:validate_body/3
    , fun custom_callbacks_xml:validate_body/3
    ],
  fun(validate_type) ->
    [ fun katt_callbacks_json:validate_type/7
    , fun custom_callbacks_xml:validate_type/7
    ],
katt:run("xml_and_json_scenario.apib", [], [{ext, XmlJson}]).
```

See [src/katt_callbacks_json.erl](src/katt_callbacks_json.erl) to see how your
`custom_callbacks_xml` module should be implemented.

## Contributing

A pull-request is most welcome. Please make sure that the following criteria are
fulfilled before making your pull-request:

* Include a description regarding what has been changed and why.
* Make sure that the changed or added functionality (if you modify code) is
  covered by unit tests.
* Make sure that all unit tests pass.


## License

[Apache 2.0](LICENSE)


  [1]: https://travis-ci.org/for-GET/katt
  [2]: https://travis-ci.org/for-GET/katt.png
