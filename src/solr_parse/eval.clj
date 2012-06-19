(ns solr-parse.eval
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

;; ala 4 clj ;;;
;;; A expression evaluator

(defmulti to-query3 "Dispatch on the :tag flag" (fn [x] (cond (map? x)       (:tag x)
                                                            (#{"(" ")"} x) :par
                                                            (s/blank? x)   :blank)))

(defmethod to-query3 :symbol
  [{[x] :content}] x)

(fact (to-query3 {:tag :symbol :content ["a"]}) => "a")

(defmethod to-query3 :key-value
  [{[{[x] :content} _ y] :content}] (list '= (list 'm x) (to-query3 y)))

(fact "to-query3"
      (to-query3 {:tag :key-value,
                 :content
                 [ {:tag :symbol, :content ["b"]} ":" {:tag :symbol, :content ["2"]}]})
      => '(= (m "b") "2"))


(defmethod to-query3 :binary-op
  [{[o] :content}]
  (if-let [r ({"AND" 'and
               "OR"  'or} o)]
    r
    (throw (Exception.))))

(fact (to-query3 {:tag :binary-op, :content ["AND"]}) => 'and)

(fact (to-query3 {:tag :binary-op, :content ["OR"]}) => 'or)

(fact (to-query3 {:tag :binary-op, :content ["X"]}) => (throws Exception))

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

(defmethod to-query3 :par
  [c] c)

(defmethod to-query3 :blank
  [_] nil)

(defmethod to-query3 :net.cgrand.parsley/root
  [{c :content}] (mapcat (fn [x] (if-let [r (to-query3 x)]
                                  [r])) c))

(future-fact "to-query :net.cgrand.parsley/root"
  (to-query3 q) => '(and (= (m "a") "1")
                         (= (m "b") "2")))

(defn transform "Transform the expression into a pol one."
  [v]
  (loop [[f & r :as all]  v
         p                []
         a                []]
    (if all
      (cond (= "(" f) (recur r (conj p [])                      a)
            (= ")" f) (recur r (pop p)                          (conj a (peek p)))
            :else     (recur r (conj (pop p) (conj (peek p) f)) a))
      a)))

(defn split-at-last-par
  [s] (reduce (fn [[h t :as a] i] (cond (seq h)   (update-in a [0] conj i)
                                       (= ")" i) (update-in a [0] conj i)
                                       :else     (update-in a [1] conj i)))
              [() ()]
              (reverse s)))

(fact "split-at-last-par"
      (split-at-last-par
          ["(" :a :b ")"])
      => [["(" :a :b ")"] []])

(fact "split-at-last-par"
      (split-at-last-par
          [:x "(" "(" :a :b ")" ")"   :y])
      => [[:x "(" "(" :a :b ")" ")"] [:y]])

(fact "split-at-last-par"
      (split-at-last-par
           [1 2 "(" 3 ")"   4 5])
      =>  [[1 2 "(" 3 ")"] [4 5]])

(def par? #{"(" ")"})

(defn transform
  [[f & r]] (cond (not (par? f)) (cons f (transform r))
                  (= "(" f)      [(transform r)]))

(fact "ok"
      (transform ["(" :x :y :z ")"]) => [[:x :y :z]])

(fact
 (transform ["(" :x :y :w "(" :z  ")" ")"]) => [[:x :y :w [:z]]])

(future-fact
 (transform ["(" :x "(" :y  ")" :z ")"]) => [[:x [:y] :z]])

(def example-ng
  {:tag :net.cgrand.parsley/root,
 :content
 [{:tag :expr-par,
   :content
   ["("
    {:tag :key-value,
     :content
     [{:tag :symbol, :content ["a"]}
      ":"
      {:tag :symbol, :content ["b"]}]}
    " "
    {:tag :right-hand,
     :content
     [{:tag :binary-op, :content ["AND"]}
      " "
      {:tag :key-value,
       :content
       [{:tag :symbol, :content ["c"]}
        ":"
        {:tag :symbol, :content ["d"]}]}]}
    ")"]}]})

(defmethod to-query3 :expr-par
  [{q :content}] (map to-query3 (remove #{"(" ")" " "} q)))
