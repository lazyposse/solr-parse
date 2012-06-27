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

The idea behind the scene is to be able to feed some solr query and retrieve a list of data representing the same query.

For example:

``` clj
solr-parse.eval> (compile-query "(a:b AND c:d) OR (e:f) OR g:h")
(or (and (= (m :a) :b) (= (m :c) :d)) (= (m :e) :f) (= (m :g) :h))
```

## License

Copyright © 2012 commiters

Distributed under the Eclipse Public License, the same as Clojure.
