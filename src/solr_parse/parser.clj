(ns solr-parse.parser
  (:use     [midje.sweet])
  (:use     [clojure.pprint     :only [pprint pp print-table]])
  (:use     [clojure.string     :only [split join split-lines replace-first] :as s])
  (:use     [clojure.repl       :only [doc find-doc]])
  (:use     [clojure.java.javadoc       :only [javadoc]])
  (:use     [clojure.tools.trace :only [trace deftrace trace-forms trace-ns untrace-ns trace-vars]])
  (:use     [clojure.walk       :as w])
  (:use     [net.cgrand.parsley       :as p])
  (:require [clojure.xml        :as xml])
  (:require [clojure.set        :as set])
  (:require [clj-http.client    :as client])
  (:require [clojure.java.shell :as sh])
  (:require [clojure.java.io    :as io])
  (:require [clojure.reflect    :as ref])
  (:require [clojure.inspector  :as ins])
  (:import  [java.io     File StreamTokenizer]))

(defn esc
  [s] (s/escape s {\' \"}))

(def example-solr-query "(-w:b AND ((w:\"P\" AND w:\"M\" AND a:\"a\"))) OR (w:b AND -((w:\"\nP\" AND w:\"M\" AND a:\"a\")))")

(def example-solr-query
  (esc "(-w:b AND ((w:'P' AND w:'M' AND a:'a'))) OR (w:b AND -((w:'\nP' AND w:'M' AND a:'a')))"))

(def example-solr-query-no-line-break
  (esc "(-w:b AND ((w:'P' AND w:'M' AND a:'a'))) OR (w:b AND -((w:'P' AND w:'M' AND a:'a')))"))

(def p (p/parser {:main :str*}
                 :str      ["\"" :str-word "\""]
                 :str-word #{#"([a-z]|\\\")+"}))

(def p-key (p/parser {:main :symbol*}
                        :symbol #"[^-\"\s\(\)][^\"\s\(\):]*"))

(fact "p-symbol"
 (get-in (p "a-b") [:content 0 :content]) => ["a-b"])

(def p-string (p/parser {:main :string*}
                        :string ["\"" :str-word "\""]
                        :str-word #"(\\\"|[^\"])*"))

(fact "p-string"
      (get-in (p-string "\"a-b\"") [:content 0 :content 1 :content]) => ["a-b"])

(fact "p-string: with quote"
      (get-in (p-string "\"a\\\"b\"") [:content 0 :content 1 :content]) => ["a\\\"b"])

(def p-key-value (p/parser {:main :key-value*}
                             :key-value [:prefix-op* :symbol ":" :word]
                             :prefix-op   "-"
                             :word-        #{:string :symbol}
                             :symbol      #"[^-\"\s\(\)][^\"\s\(\):]*"
                             :string      ["\"" :str-word "\""]
                             :str-word    #"(\\\"|[^\"])*"))

(fact "p-key-value"
      (get-in (p-key-value "a:b") [:content 0 :content 0 :content 0]) => "a"
      (get-in (p-key-value "a:b") [:content 0 :content 2 :content 0]) => "b")

(defn parse-ok?
  [p]
  (not (re-find #":net.cgrand.parsley/(unexpected|unfinished)"
                (with-out-str (println p)))))

(def p-expr (p/parser {:main :expr
                       :space :ws?}
                      :ws-         #"\s+"
                      :expr-       #{:key-value ["(" :prefix-op* :expr :right-hand* ")"]}
                      :right-hand- [:binary-op :prefix-op* :expr]
                      :key-value   (p/unspaced :symbol ":" :word)
                      :prefix-op   "-"
                      :binary-op   #{"AND" "OR"}
                      :word-       #{:string :symbol}
                      :symbol      #"[^-\"\s\(\)][^\"\s\(\):]*"
                      :string      (p/unspaced ["\"" :str-word "\""])
                      :str-word-    #"(\\\"|[^\"])*"))

(fact "p-expr: good case with strings"
      (parse-ok? (p-expr (esc "(a:'b')")))                         => truthy
      (parse-ok? (p-expr (esc "(a:' b ')")))                       => truthy
      (parse-ok? (p-expr (esc "(a:'\na\nb\n')")))                  => truthy)

(fact "p-expr: good case"
      (parse-ok? (p-expr "(a:b)"))                                 => truthy
      (parse-ok? (p-expr "((a:b))"))                               => truthy
      (parse-ok? (p-expr "(((a:b)))"))                             => truthy
      (parse-ok? (p-expr "(a:b AND c:d)"))                         => truthy
      (parse-ok? (p-expr "(a:b AND c:d OR d:e)"))                  => truthy
      (parse-ok? (p-expr "(a:b AND (c:d OR d:e))"))                => truthy
      (parse-ok? (p-expr "((a:b OR a:b) AND (c:d OR d:e))"))       => truthy)

(fact "p-expr: good case with '-'"
      (parse-ok? (p-expr "(-a:b)"))                                => truthy
      (parse-ok? (p-expr "(--a:b)"))                               => truthy
      (parse-ok? (p-expr "(---a:b)"))                              => truthy
      (parse-ok? (p-expr "(-(-a:b))"))                             => truthy
      (parse-ok? (p-expr "(-(-(-a:b)))"))                          => truthy
      (parse-ok? (p-expr "(-a:b AND  c:d)"))                       => truthy
      (parse-ok? (p-expr "( a:b AND -c:d)"))                       => truthy
      (parse-ok? (p-expr "(-a:b AND -c:d OR -d:e)"))               => truthy
      (parse-ok? (p-expr "(-a:b AND -(-c:d OR -d:e))"))            => truthy
      (parse-ok? (p-expr "(-(-a:b OR -a:b) AND -(-c:d OR -d:e))")) => truthy)

(fact "p-expr: bad cases"
      (parse-ok? (p-expr "(AND)"))                                 => falsey
      (parse-ok? (p-expr "((a:b)"))                                => falsey
      (parse-ok? (p-expr "(a : b)"))                               => falsey
      (parse-ok? (p-expr "(a: b)"))                                => falsey
      (parse-ok? (p-expr "(a :b)"))                                => falsey
      (parse-ok? (p-expr "(a:b AND)"))                             => falsey
      (parse-ok? (p-expr "(AND a:b)"))                             => falsey
      (parse-ok? (p-expr "(a:b -AND a:b)"))                        => falsey)

(defn parse-solr
  [s] (p-expr (str "(" s ")")))

(def input-tree {:tag :net.cgrand.parsley/root,
                 :content
                 ["("
                  {:tag :key-value,
                   :content
                   [{:tag :symbol, :content ["a"]} ":" {:tag :symbol, :content ["b"]}]}
                  ")"]})

(defn my-pred
  ""
  [s] (= (s "a") "b"))

(defn tree-to-pred
  [t] (fn [s] (let [[{[a] :content} _ {[b] :content}] (get-in t [:content 1 :content])]
               (= (s a) b))))

(defn tree-to-pred
  [t] (fn [s] ))

(future-fact "tree-to-pred: ok"
 ((tree-to-pred input-tree) {"a" "b", "c" "d"}) => truthy )

(fact "tree-to-pred: ko1"
 ((tree-to-pred input-tree) {"a" "x", "c" "d"}) => falsey )

(fact "tree-to-pred: ko2"
 ((tree-to-pred input-tree) {"x" "b", "c" "d"}) => falsey )

(future-fact ["("
              {:tag :key-value,
               :content
               [{:tag :symbol, :content ["1"]} ":" {:tag :symbol, :content ["1"]}]}
              " "
              {:tag :binary-op, :content ["AND"]}
              " "
              {:tag :key-value,
               :content
               [{:tag :symbol, :content ["2"]} ":" {:tag :symbol, :content ["2"]}]}
              ")"]) => true
