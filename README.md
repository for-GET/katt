# KATT [![Build Status](https://secure.travis-ci.org/klarna/katt.png)](http://travis-ci.org/klarna/katt)


KATT (Klarna API Testing Tool) is an HTTP-based API testing tool for Erlang.

## Quick start

Use for shooting HTTP requests in a sequential order and verifying the response.
Any relevant difference between expected and actual responses will cause a
failure.

Tags with special meaning:
<dl>
  <dt>"{{_}}"</dt>
  <dd>
    Match anything (i.e. no real validation, only check existence)</dd>
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


## Examples

From the repo's root directory, and having started the erlang shell with
```ERL_LIBS=deps erl -pa ebin```:

Simple example

```erlang

1> ok = ssl:start(), ok = lhttpc:start().
ok
2> katt:run("doc/example.apib").

...

```

Note: The simple example assumes that you have a server running that behaves
according to the specification, which you probably don't have.


`katt:run(X)` is the same as `katt:run(X, [])`, which is the same as
`katt:run(X, [], [])`.


A more advanced example, which will make requests to a third party server and
that also has some more esoteric functionality in its blueprint:

```erlang

1> ok = ssl:start(), ok = lhttpc:start().
ok
2> TemplateVariables = [{"my_name", "Joe"}, {"your_name", "Mike"}],
2> Params = [{host, "httpbin.org"}],
2> katt:run("doc/example-httpbin.apib", Params, TemplateVariables).
[{{katt_request,"GET",
                "http://httpbin.org:80/get?origin=x&whoarewe=Mike_and_Joe",
                [{"Accept","application/json"},{"User-Agent","KATT"}],
                null},
  pass},
 {{katt_request,"POST","http://httpbin.org:80/post",
                [{"Accept","application/json"},
                 {"Content-Type","application/json"},
                 {"User-Agent","KATT"}],
                <<"{\"origin\":\"x\",\"whoarewe\":\"Mike_and_Joe\",\"date\":\"x"...>>},
  pass},
 {{katt_request,"GET","http://httpbin.org:80/cache",
                [{"Accept","application/json"},
                 {"User-Agent","KATT"},
                 {"If-Modified-Since","x"}],
                null},
  pass},
 {{katt_request,"GET","http://httpbin.org:80/cache",
                [{"Accept","application/json"},{"User-Agent","KATT"}],
                null},
  pass},
 {{katt_request,"GET","http://httpbin.org:80/status/401",
                [{"Accept","*"},{"User-Agent","KATT"}],
                null},
  pass},
 {{katt_request,"GET","http://httpbin.org:80/status/418",
                [{"Accept","*"},{"User-Agent","KATT"}],
                null},
  pass}]

```


## Contributing

A pull-request is most welcome. Please make sure that the following criteria are
fulfilled before making your pull-request:

* Include a description regarding what has been changed and why.
* Make sure that the changed or added functionality (if you modify code) is
  covered by unit tests.
* Make sure that all unit tests pass.


## License

Copyright 2013 Klarna AB.

Licensed under the [Apache License, Version 2.0](LICENSE).
