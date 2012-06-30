# solr-parse

I'm just a function really (compile-solr).

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

The idea behind the scene is to be able to feed some solr query and get a function that will be able to query a clj data store.

For example:

Given a function `default-transco`

``` clj

This function takes 2 parameters and returns a list of data.

```
(fact (default-transco :a "b") => '(= (m :a) "b"))
```

When compiling the query with this function:

``` clj
(fact
  (compile-query default-transco "(a:b AND c:\"d\") OR (e:f) OR g:h") => '(or (and (= (m :a) :b) (= (m :c) "d")) (= (m :e) :f) (= (m :g) :h)))
```

Here is another function and the result:

``` clj
(fact (reverse-transco :a "b") => '(= (m "b) :a))
```

Here is the result of the same query with another function

``` clj
(fact
  (compile-query reverse-transco "(a:b AND c:\"d\") OR (e:f) OR g:h") => '(or (and (= (m :b) :a) (= (m "d") :c)) (= (m :f) :e) (= (m :h) :g)))
```

In this example:
- `a:b`, `c:d`, `e:f`, and `g:h` are some key value pair which hold meaning in your context.
- m represents a map which also hold meaning for the overall data structure to be viewed as a function.

## License

Copyright © 2012 commiters

Distributed under the Eclipse Public License, the same as Clojure.
