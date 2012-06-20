(ns solr-parse.eval
  (:use     [midje.sweet])
  (:use     [clojure.pprint     :only [pprint pp print-table]])
  (:use     [clojure.string     :only [split join split-lines replace-first] :as s])
  (:use     [clojure.repl       :only [doc find-doc dir]])
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

(defmulti to-query "Dispatch on the :tag flag"
  (fn [x] (cond (map? x)       (:tag x)
               (#{"(" ")"} x) :par
               (s/blank? x)   :blank)))

(defmethod to-query :symbol
  [{[x] :content}] x)

(fact "to-query :symbol"
  (to-query {:tag :symbol :content ["a"]}) => "a")

(defmethod to-query :key-value
  [{[{[x] :content} _ y] :content}] (list '= (list 'm x) (to-query y)))

(fact "to-query :key-value"
      (to-query {:tag :key-value,
                 :content
                 [ {:tag :symbol, :content ["b"]} ":" {:tag :symbol, :content ["2"]}]})
      => '(= (m "b") "2"))

(defmethod to-query :binary-op
  [{[o] :content}]
  (if-let [r ({"AND" 'and
               "OR"  'or} o)]
    r
    (throw (Exception.))))

(fact "to-query :binary-op"
  (to-query {:tag :binary-op, :content ["AND"]}) => 'and)

(fact "to-query :binary-op"
  (to-query {:tag :binary-op, :content ["OR"]}) => 'or)

(fact "to-query :binary-op"
  (to-query {:tag :binary-op, :content ["X"]}) => (throws Exception))

(defmethod to-query :par
  [c] c)

(defmethod to-query :blank
  [_] nil)

(defmethod to-query :root
  [{c :content}] (map to-query c))

(future-fact "to-query :net.cgrand.parsley/root"
  (to-query q) => '(and (= (m "a") "1")
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
 (transform ["(" :x :y :w "(" :z ")" ")"]) => [[:x :y :w [:z]]])

(future-fact
 (transform ["(" :x "(" :y  ")" :z ")"]) => [[:x [:y] :z]])

(def example-ng
{:tag :root
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
    {:tag :binary-op, :content ["OR"]}
    " "
    {:tag :key-value,
     :content
     [{:tag :symbol, :content ["c"]}
      ":"
      {:tag :symbol, :content ["d"]}]}
    " "
    {:tag :binary-op, :content ["AND"]}
    " "
    {:tag :key-value,
     :content
     [{:tag :symbol, :content ["e"]}
      ":"
      {:tag :symbol, :content ["f"]}]}
    ")"]}]})

(defn and-or
  [s] (cons 'OR (map (fn [ands] (cons 'AND (remove #{'AND} ands)))
                     (take-nth 2 (partition-by #{'OR} s)))))

(defn binary-op?
  [x] (= (:tag x) :binary-op))

(defn and?
  [x] (and (binary-op? x) (= (:content x ) ["AND"])))

(defn or?
  [x] (and (binary-op? x) (= (:content x ) ["OR"])))

(defmethod to-query :expr-par
  [{q :content}] (cons 'or
                       (map (fn [ands] (cons 'and (map to-query (remove and? ands))))
                            (take-nth 2 (partition-by or?
                                                      (remove #{"(" ")" " "} q))))))

(fact "and-or"
  (and-or '(0 AND 1 OR 2 OR 3 AND 4 AND 5))
  => '(OR (AND 0 1) (AND 2) (AND 3 4 5)))

(def example2-src "a:b AND b:c OR e:f AND g:d")

(def example2 {:tag :root,
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
                  {:tag :binary-op, :content ["AND"]}
                  " "
                  {:tag :key-value,
                   :content
                   [{:tag :symbol, :content ["b"]}
                    ":"
                    {:tag :symbol, :content ["c"]}]}
                  " "
                  {:tag :binary-op, :content ["OR"]}
                  " "
                  {:tag :key-value,
                   :content
                   [{:tag :symbol, :content ["e"]}
                    ":"
                    {:tag :symbol, :content ["f"]}]}
                  " "
                  {:tag :binary-op, :content ["AND"]}
                  " "
                  {:tag :key-value,
                   :content
                   [{:tag :symbol, :content ["g"]}
                    ":"
                    {:tag :symbol, :content ["d"]}]}
                  ")"]}]})

(defmethod to-query :expr-par-simple
  [{q :content}] (list q))

(def example-not {:tag :root,
                  :content
                  [{:tag :expr-par-simple,
                    :content
                    ["("
                     {:tag :prefix-op, :content ["-"]}
                     {:tag :key-value,
                      :content
                      [{:tag :symbol, :content ["a"]}
                       ":"
                       {:tag :symbol, :content ["b"]}]}
                     ")"]}]})

