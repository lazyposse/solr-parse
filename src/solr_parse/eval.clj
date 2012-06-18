(ns solr-parse.eval
  (:use     [midje.sweet])
  (:use     [clojure-station.core :as c])
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
  (:require [clojure-station.graphviz :as viz])
  (:import  [java.io     File StreamTokenizer]))

;; ala 4 clj ;;;
;;; A expression evaluator

(defmulti to-query2 "Dispatch on the :tag flag" (fn [x] (cond (map? x)       (:tag x)
                                                            (#{"(" ")"} x) :par
                                                            (s/blank? x)   :blank)))

(defmethod to-query2 :symbol
  [{[x] :content}] x)

(fact (to-query2 {:tag :symbol :content ["a"]}) => "a")

(defmethod to-query2 :key-value
  [{[{[x] :content} _ y] :content}] (list '= (list 'm x) (to-query2 y)))

(fact "to-query2"
      (to-query2 {:tag :key-value,
                 :content
                 [ {:tag :symbol, :content ["b"]} ":" {:tag :symbol, :content ["2"]}]})
      => '(= (m "b") "2"))


(defmethod to-query2 :binary-op
  [{[o] :content}]
  (if-let [r ({"AND" 'and
               "OR"  'or} o)]
    r
    (throw (Exception.))))

(fact (to-query2 {:tag :binary-op, :content ["AND"]}) => 'and)

(fact (to-query2 {:tag :binary-op, :content ["OR"]}) => 'or)

(fact (to-query2 {:tag :binary-op, :content ["X"]}) => (throws Exception))

(def q
{:tag :net.cgrand.parsley/root,
 :content
 ["("
  {:tag :key-value,
   :content
   [{:tag :symbol, :content ["a"]} ":" {:tag :symbol, :content ["1"]}]}
  " "
  {:tag :binary-op, :content ["AND"]}
  " "
  {:tag :key-value,
   :content
   [{:tag :symbol, :content ["b"]} ":" {:tag :symbol, :content ["2"]}]}
  ")"]})

(defmethod to-query2 :par
  [c] c)

(defmethod to-query2 :blank
  [_] nil)

(defmethod to-query2 :net.cgrand.parsley/root
  [{c :content}] (mapcat (fn [x] (if-let [r (to-query2 x)]
                                  [r])) c))

(fact (to-query2 q) => '(and (= (m "a") "1")
                            (= (m "b") "2")))


(prn "-------------------")
