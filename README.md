
splunge
=======

A tool for searching JSON formatted logs with a query language which is inspired
by Splunk (well, a very small subset of it, for now). Once it's more complete,
it could be useful for times when you are running your service locally and you
want to figure out what's going on but you don't know `grep` and `jq` well
enough.

The idea is that it should be easy to hack on and extend when you realise you
need a specific feature.

## Design

The query is parsed and "compiled" into a
[transducer](https://clojure.org/reference/transducers), which is essentially a
stateful closure that processes a stream of events. The incoming events
(line-separated JSON) are processed as a stream by this transducer, so memory
usage should be constant. The exception is of course the `join` command, which
needs to keep everything in memory.

## But isn't JVM Clojure a bad fit for a CLI tool because of its horrendous startup times?

GraalVM native image solves this. You can grab a compiled binary
[here](https://github.com/dlesl/splunge/releases).

## Example

``` sh
$ for x in {1..1000}; do echo '{"amount": '$((x % 10))'}'; done | splunge 'stats count by amount | sort - amount'

| amount | count |
|--------+-------|
|      9 |   100 |
|      8 |   100 |
|      7 |   100 |
|      6 |   100 |
|      5 |   100 |
|      4 |   100 |
|      3 |   100 |
|      2 |   100 |
|      1 |   100 |
|      0 |   100 |
```

