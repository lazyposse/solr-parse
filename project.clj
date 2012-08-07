(defproject solr-parse "0.1.0-SNAPSHOT"
  :min-lein-version "2.0.0"
  :profiles {:dev
             {:dependencies
              [[midje "1.4.0"]
               [com.stuartsierra/lazytest "1.2.3"]]}}
  :dependencies [[org.clojure/clojure                   "1.4.0"]
                 [com.google.code.javaparser/javaparser "1.0.8"]
                 [org.clojure/tools.trace               "0.7.3"]
                 [net.cgrand/parsley                    "0.9.1"]]
  :description "Compile a solr query to an equivalent clojure function."
  :repositories {"stuart" "http://stuartsierra.com/maven2"})
