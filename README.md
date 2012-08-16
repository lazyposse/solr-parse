# solr-parse

Turns Solr queries into Clojure predicates.

## What's this?

It's probably nothing interesting for you, really!

And it's also half-bake.

Take a look only if you want to see an usage of Christophe Grand's
parser: [parsley](https://github.com/cgrand/parsley).

It takes a Solr query, and output a Clojure predicate that takes an
object and return true if it matches the query.

## dependency

In your `project.clj`, in the dependency key, add this:

``` clj
[solr-parse 0.1.0-SNAPSHOT]
```

Then

``` clj
lein deps
```

Then you're good

## Usage

The idea is to be able to compute, given a function and a solr query, a clojure function able to query a clj data store.

For example, given a function 'default-transco' which takes 2 parameters and returns a list of data:

``` clj
(fact
  (default-transco :a "b")
    => '(= (m :a) "b"))
```

And a query:

``` clj
(fact
  (compile-query default-transco "(a:b AND c:\"d\") OR (e:f) OR g:h")
    => '(or (and (= (m :a) :b) (= (m :c) "d")) (= (m :e) :f) (= (m :g) :h)))
```

Here is another function `reverse-transco`:

``` clj
(fact
  (reverse-transco :a "b")
    => '(= (m "b") :a))
```

And the same query:

``` clj
(fact
  (compile-query reverse-transco "(a:b AND c:\"d\") OR (e:f) OR g:h")
    => '(or (and (= (m :b) :a) (= (m "d") :c)) (= (m :f) :e) (= (m :h) :g)))
```

In this example:
- `a:b`, `c:"d"`, `e:f`, and `g:h` are some key value pair which hold meaning in your context.
- m represents something in your `default-transco` or `reverse-transco`

## License

Copyright © 2012 commiters

Distributed under the Eclipse Public License, the same as Clojure.
