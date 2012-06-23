(ns solr-parse.eval
  (:use     [midje.sweet])
  (:use     [clojure.pprint     :only [pprint pp print-table]])
  (:use     [clojure.string     :only [split join split-lines replace-first] :as s])
  (:use     [clojure.repl       :only [doc find-doc dir]])
  (:use     [clojure.java.javadoc       :only [javadoc]])
  (:use     [clojure.tools.trace :only [trace deftrace trace-forms trace-ns untrace-ns trace-vars]])
  (:use     [clojure.walk       :as w])
  (:use     [solr-parse.parser  :only [parse-solr example-solr-query]])
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

(def to-query +) ;; hack to be able to redifine a multimethod dispatch

(defmulti to-query "Dispatch on the :tag flag"
  (fn [x] (cond (map? x)       (:tag x)
               (#{"(" ")"} x) :par
               (s/blank? x)   :blank)))

(defmethod to-query :symbol
  [{[x] :content}] (keyword x))

(fact "to-query :symbol"
  (to-query {:tag :symbol :content ["a"]}) => :a)

(defmethod to-query :key-value
  [{[x _ y] :content}] (list '= (list 'm (to-query x)) (to-query y)))

(fact "to-query :key-value"
      (to-query {:tag :key-value,
                 :content
                 [ {:tag :symbol, :content ["b"]} ":" {:tag :symbol, :content ["2"]}]})
      => '(= (m :b) :2))

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
  [c] nil)

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
                                                      (remove #{"(" ")" " "} (trace q)))))))

(defmethod to-query :expr-par
  [{q :content}] q)



(defn and-or
  [s] (cons 'OR (map (fn [ands] (cons 'AND (remove #{'AND} ands)))
                     (take-nth 2 (partition-by #{'OR} s)))))

(future-fact "and-or: only and"
  (and-or '(0 AND 1))
  => '(AND 0 1))

(future-fact "and-or: only or"
  (and-or '(0 AND 1))
  => '(AND 0 1))

(fact "and-or"
  (and-or '(0 AND 1 OR 2 OR 3 AND 4 AND 5))
  => '(OR (AND 0 1) (AND 2) (AND 3 4 5)))

(fact "and-or"
  (and-or '(0 AND 1 OR 2 OR 3 AND 4 AND 5))
  => '(OR (AND 0 1) (AND 2) (AND 3 4 5)))

(defn and-ify
  [s]
  (if (and (sequential? s) (some #{'AND} s))
    (cons 'and (remove #{'AND} s))
    s))

(fact "and-ify"
  (and-ify '(0)) => '(0))

(fact "and-ify"
  (and-ify 0) => 0)

(fact "and-ify"
  (and-ify '(0 AND 1)) => '(and 0 1))

(fact "and-ify"
  (and-ify '(0 AND 1 AND 2)) => '(and 0 1 2))

(defn or-ify
  [s]
  (if (some #{'OR} s)
    (cons 'or (map (fn [x] (if (and (sequential? x) (second x))
                            x
                            (first x)))
                   (take-nth 2 (partition-by #{'OR} s))))
    s))

(fact "or-ify: no or"
  (or-ify '(a AND b))
  => '(a AND b))

(fact "or-ify"
  (or-ify '(a OR b AND c OR d))
  => '(or a (b AND c) d))

(def example2-src "a:b AND b:c OR e:f AND g:d")

(def example2
{:tag :root,
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
  [{q :content}] (map to-query q))

(defmethod to-query :prefix-op
  [{[q] :content}] (if-let [o ({"-" 'not} q)]
                     o
                     (throw (RuntimeException. (str "Unknown prefix operator: " q)))))

(defmethod to-query :expr-prefixed
  [{q :content}]
  {:pre [(= 2 (count q))]}
  (list (to-query (first  q))
        (to-query (second q))))

(def example-not-src "-a:b")

(def example-not
  {:tag :root,
   :content
   [{:tag :expr-par-simple,
     :content
     ["("
      {:tag :expr-prefixed,
       :content
       [{:tag :prefix-op, :content ["-"]}
        {:tag :key-value,
         :content
         [{:tag :symbol, :content ["a"]}
          ":"
          {:tag :symbol, :content ["b"]}]}]}
      ")"]}]})

(defmethod to-query :string
  [{[_ q _] :content}] q)

(def example-str
  {:tag :root,
   :content
   [{:tag :expr-par-simple,
     :content
     ["("
      {:tag :key-value,
       :content
       [{:tag :symbol, :content ["a"]}
        ":"
        {:tag :string, :content ["\"" "b" "\""]}]}
      ")"]}]})

(def example-big
  {:tag :root,
   :content
   [{:tag :expr-par,
     :content
     ["("
      {:tag :expr-par,
       :content
       ["("
        {:tag :expr-prefixed,
         :content
         [{:tag :prefix-op, :content ["-"]}
          {:tag :key-value,
           :content
           [{:tag :symbol, :content ["w"]}
            ":"
            {:tag :symbol, :content ["b"]}]}]}
        " "
        {:tag :binary-op, :content ["AND"]}
        " "
        {:tag :expr-par-simple,
         :content
         ["("
          {:tag :expr-par,
           :content
           ["("
            {:tag :key-value,
             :content
             [{:tag :symbol, :content ["w"]}
              ":"
              {:tag :string, :content ["\"" "P" "\""]}]}
            " "
            {:tag :binary-op, :content ["AND"]}
            " "
            {:tag :key-value,
             :content
             [{:tag :symbol, :content ["w"]}
              ":"
              {:tag :string, :content ["\"" "M" "\""]}]}
            " "
            {:tag :binary-op, :content ["AND"]}
            " "
            {:tag :key-value,
             :content
             [{:tag :symbol, :content ["a"]}
              ":"
              {:tag :string, :content ["\"" "a" "\""]}]}
            ")"]}
          ")"]}
        ")"]}
      " "
      {:tag :binary-op, :content ["OR"]}
      " "
      {:tag :expr-par,
       :content
       ["("
        {:tag :key-value,
         :content
         [{:tag :symbol, :content ["w"]}
          ":"
          {:tag :symbol, :content ["b"]}]}
        " "
        {:tag :binary-op, :content ["AND"]}
        " "
        {:tag :expr-prefixed,
         :content
         [{:tag :prefix-op, :content ["-"]}
          {:tag :expr-par-simple,
           :content
           ["("
            {:tag :expr-par,
             :content
             ["("
              {:tag :key-value,
               :content
               [{:tag :symbol, :content ["w"]}
                ":"
                {:tag :string, :content ["\"" "\nP" "\""]}]}
              " "
              {:tag :binary-op, :content ["AND"]}
              " "
              {:tag :key-value,
               :content
               [{:tag :symbol, :content ["w"]}
                ":"
                {:tag :string, :content ["\"" "M" "\""]}]}
              " "
              {:tag :binary-op, :content ["AND"]}
              " "
              {:tag :key-value,
               :content
               [{:tag :symbol, :content ["a"]}
                ":"
                {:tag :string, :content ["\"" "a" "\""]}]}
              ")"]}
            ")"]}]}
        ")"]}
      ")"]}]})

