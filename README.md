# KATT [![Build Status][2]][1] [Docs][3]

KATT (Klarna* API Testing Tool) is an HTTP-based API testing tool for Erlang.

\* Albeit the "Klarna" mention, this repository is not affiliated with Klarna AB.
KATT was indeed born at Klarna, and Klarna AB holds copyright for parts of the code,
but it is now being maintained outside the company, by its original authors and new contributors.


## Quick start

[An example is worth a 1000 words.](doc/example-httpbin.apib)

Use for shooting HTTP requests in a sequential order and verifying the response.
Any relevant difference between expected and actual responses will cause a
failure.

The builtin validator supports basic text validation and more advanced validation of HTTP headers,
and media-types (`application/json`, `application/*+json`, `application/x-www-form-urlencoded`).

The validator makes use of a few tags with special meaning:

`"{{_}}"`
Match anything (i.e. no real validation, only check existence).

`"{{unexpected}}"`
Match nothing (i.e. no real validation, only check lack of existence)

`"{{>key}}"`
Store value of the whole string (key must be unique within testcase)

`"{{<key}}"`
Recall stored value.

`"{{{module:function(arg1, arg2,.....)}}}"`
Use custom function for validation and/or value extraction. The module must be available in the code path at runtime. 

This annotation invokes the function with  the supplied argument(s) and the entire Json node as as argument. 

If no args is specified  the function is called with only one argument, the Json node.

The Json node will always be the first argument supplied to the function 

The function should return either

`{pass,[{"Key","Value"}]}` returns key value pairs that can be referenced through recall
further down in the blueprint file . The list can contain one or more keyvalue pairs. 

`{not_equal,{"ErrorName","Description"}}` if validation is done and errors are found, they can be passed back to katt with this payload. 

`{pass,[]}`, in case of successfull validation when no values needs to be stored this can be passed back   

The `"{{_}}"` tag can also be used as a JSON object's property in order to
validate any other additional properties.

By default, the builtin validator will allow additional properties in an object
structure, or additional items in an array structure. To counteract that
default, one can do `{..., "{{_}}": "{{unexpected}}"}` or
`[..., "{{unexpected}}"]`, effectively making a rule that no properties/items
are expected beyond the ones defined.

For more complex validations, KATT supports extensible validation types.
Built-in validation types: `set`, `runtime_value`, `runtime_validation`.

`set` will ignore the order of an array's items, and just check for existence:

```
{
  "some_array": {
    "{{type}}": "set",
    "value": [1, 2, 3]
  }
}
```

So the above would validate against JSON instances such as
`{"some_array": [1, 3, 2]}`, or `{"some_array": [3, 2, 1]}`,
or even `{"some_array": [4, 3, 2, 1]}` unless we add `{{unexpected}}`.

`runtime_value` would just run code (only `erlang` and `shell` supported for now),
while having access to `ParentKey`, `Actual`, `Unexpected` and `Callbacks`,
and return the expected value and matched against the actual one.

```
{
  "rfc1123": {
    "{{type}}": "runtime_validation",
    "erlang": "list_to_binary(httpd_util:rfc1123_date(calendar:now_to_datetime(erlang:now())))"
  }
}
```

or in array format

```
{
  "rfc1123": {
    "{{type}}": "runtime_validation",
    "erlang": ["list_to_binary(",
               "  httpd_util:rfc1123_date(",
               "    calendar:now_to_datetime(",
               "      erlang:now()",
               ")))"
              ]
  }
}
```

`runtime_validation` would just run code (only `erlang` and `shell` supported for now),
while having access to `ParentKey`, `Actual`, `Unexpected` and `Callbacks`,
and return

* `{pass, [{"Key", "Value"}]}` i.e. validation passed, store new param "Key" with value "Value"
* `{not_equal, {Key, Expected, Actual}}`
* `{not_equal, {Key, Expected, Actual, [{"more", "info"}]}}`

```
{
  "rfc1123": {
    "{{type}}": "runtime_validation",
    "erlang": "Expected = httpd_util:rfc1123_date(calendar:now_to_datetime(erlang:now())), case Actual =:= Expected of true -> {pass, []}; false -> {not_equal, {ParentKey, Expected, Actual}} end"
  }
}
```

or in array format

```
{
  "rfc1123": {
    "{{type}}": "runtime_validation",
    "erlang": ["Expected = httpd_util:rfc1123_date(calendar:now_to_datetime(erlang:now())),",
               "case Actual =:= Expected of",
               "  true ->",
               "    {pass, []};",
               "  false ->",
               "    {not_equal, {ParentKey, Expected, Actual}}",
               "end"
              ]
  }
}
```


## Examples

A simple example that will make requests to a third party server:

```bash
ERL_LIBS=deps erl -pa ebin -noshell -eval '
  application:ensure_all_started(katt),
  BlueprintFile = "doc/example-httpbin.apib",
  Params = [{base_url, "http://httpbin.org"}, {my_name, "Joe"}, {your_name, "Mike"}],
  io:format("~p~n", [katt:run(BlueprintFile, Params)]).
' -s init stop
```
... or run the code passed to -eval from the Erlang shell (assuming that you
have started the Erlang shell from the repo's root directory with `ERL_LIBS=deps
erl -pa ebin`).


## CLI

You can also fire up `katt` from the CLI, with
```bash
bin/katt base_url=http://httpbin.org my_name=Joe your_name=Mike -- doc/example-httpbin.apib
```

If you want non-string params, use `:=` as a separator e.g. `my_int:=123`.

You can also output the result in JSON format, with `--json`, and beautify it e.g. with python
```bash
bin/katt --json base_url=http://httpbin.org my_name=Joe your_name=Mike -- doc/example-httpbin.apib | python -m json.tool
```


## Interface

* `katt:run` to be called with
  * `filename`
  * `params` (optional)
    * `base_url`, alternatively you can use the legacy
      * `protocol`
      * `hostname`
      * `port`
      * `base_path`
    * `request_timeout`
    * `scenario_timeout`
  * `callbacks` (optional)
    * `ext` to be called with `scope` (recall_body, parse, validate_body, validate_type)
    * `recall` to be called with `syntax`, `text`, `params`, `callbacks`
    * `parse` to be called with `headers`, `body`, `params`, `callbacks`
    * `request` to be called with `request`, `params`, `callbacks`
    * `validate` to be called with `expected`, `actual`, `params`, `callbacks`
    * `progress` to be called with `transaction_result`
    * `text_diff` to be called with `text`, `text`

A request can also be configured via HTTP request headers:

* `x-katt-description` would take precedence over the transaction's description
* `x-katt-request-timeout` would take precedence over the `request_timeout` param
* `x-katt-request-sleep` would delay the request for a specific amount of milliseconds

### If you would like to convert a HAR file to an APIB file

The HTTP Archive format or HAR, is a JSON-formatted archive file format
for logging of a web browser's interaction with a site, [standardized by
the Web Performance Working Group of the World Wide Web Consortium (W3C)](https://dvcs.w3.org/hg/webperf/raw-file/tip/specs/HAR/Overview.html).

For example, to convert [doc/example-teapot.har](doc/example-teapot.har)
into [doc/example-teapot.apib](doc/example-teapot.apib), run:

``` bash
bin/katt from-har --apib -- doc/example-teapot.har > doc/example-teapot.apib
```

### If you would like to disable JSON support

```erlang
OnlyText = fun(_Scope) -> [] end,
katt:run("text_only_scenario.apib", [], [{ext, OnlyText}]).
```

### If you would like to add XML support

```erlang
PlusXml =
  fun(recall_body) ->
    [ fun custom_callbacks_xml:recall_body/4
    ] ++ katt_callbacks:ext(recall_body);
  fun(parse) ->
    [ fun custom_callbacks_xml:parse/5
    ] ++ katt_callbacks:ext(parse);
  fun(validate_body) ->
    [ fun custom_callbacks_xml:validate_body/3
    ] ++ katt_callbacks:ext(validate_body),
  fun(validate_type) ->
    [ fun custom_callbacks_xml:validate_type/7
    ] ++ katt_callbacks:ext(validate_type),
katt:run("xml_scenario.apib", [], [{ext, PlusXml}]).
```

See [src/katt_callbacks_json.erl](src/katt_callbacks_json.erl) to see how your
`custom_callbacks_xml` module should be implemented.

### If you would like to build KATT with almost no dependencies

``` bash
export KATT_BARE_MODE=true
# or
touch .rebar/BARE_MODE
```


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
  [3]: https://dev.erldocs.com/github.com/for-get/katt/
