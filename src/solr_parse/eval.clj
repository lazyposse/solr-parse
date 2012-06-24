(ns solr-parse.eval
  (:use     [midje.sweet])
  (:use     [clojure.pprint       :only [pprint pp print-table]])
  (:use     [clojure.repl         :only [doc find-doc dir]])
  (:use     [clojure.java.javadoc :only [javadoc]])
  (:use     [clojure.tools.trace  :only [trace deftrace trace-forms trace-ns untrace-ns trace-vars]])
  (:use     [clojure.walk         :as w])
  (:use     [solr-parse.parser    :only [parse-solr example-solr-query]])
  (:require [clojure.string       :only [split join split-lines replace-first blank?] :as s])
  (:require [clojure.xml          :as xml])
  (:require [clojure.set          :as set])
  (:require [clj-http.client      :as client])
  (:require [clojure.java.shell   :as sh])
  (:require [clojure.java.io      :as io])
  (:require [clojure.reflect      :as ref])
  (:require [clojure.inspector    :as ins])
  (:import  [java.io              File StreamTokenizer]))

;; ala 4 clj ;;;
;;; A expression evaluator

(def to-query +) ;; hack to be able to redefine a multimethod dispatch

(defmulti to-query "Dispatch on the :tag flag" :tag)

(defmethod to-query :symbol
  [{[x] :content}] (keyword x))

(fact "to-query :symbol"
  (to-query {:tag :symbol :content ["a"]}) => :a)

(defmethod to-query :default
  [x] x)

(fact "to-query unidentified symbol return the symbol as is."
  (to-query 'or) => 'or
  (to-query 'and) => 'and)

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

(defmethod to-query :root
  [{c :content}] (map to-query c))

(future-fact "to-query :net.cgrand.parsley/root"
  (to-query q) => '(and (= (m "a") "1")
                         (= (m "b") "2")))

(defn binary-op?
  [x] (= (:tag x) :binary-op))

(defn and-ify
  [s]
  (if (and (sequential? s) (some #{'and} s))
    (cons 'and (remove #{'and} s))
    s))

(fact "and-ify"
  (and-ify '(0)) => '(0)
  (and-ify 0) => 0
  (and-ify '(0 and 1)) => '(and 0 1)
  (and-ify '(0 and 1 and 2)) => '(and 0 1 2))

(defn or-ify
  [s]
  (if (some #{'or} s)
    (cons 'or (map (fn [x] (if (and (sequential? x) (second x))
                            x
                            (first x)))
                   (take-nth 2 (partition-by #{'or} s))))
    s))

(fact "or-ify: no or"
  (or-ify '(a and b)) => '(a and b)
  (or-ify '(a or b and c or d)) => '(or a (b and c) d))

(defn binary-ify
  [s]
  (map and-ify (or-ify s)))

(fact "binary-ify"
  (binary-ify '(a or b and c or d)) => '(or a (and b c) d))

(defmethod to-query :expr-par
  [{q :content}]
  (binary-ify (map to-query (remove #{"(" ")" " "} q))))

(fact "a:b OR c:d AND e:f"
  (let [example-ng {:tag :root
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
                       ")"]}]}]
    (to-query example-ng) => '((or (= (m :a) :b) (and (= (m :c) :d) (= (m :e) :f))))))

(fact "a:b AND b:c OR e:f AND g:d"
  (let [example2 {:tag :root,
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
                     ")"]}]}]
    (to-query example2) => '((or (and (= (m :a) :b) (= (m :b) :c)) (and (= (m :e) :f) (= (m :g) :d))))))

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

(future-fact "-a:b"
  (let [example-not {:tag :root,
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
                        ")"]}]}]
    (to-query example-not) => '((not (= (m :a) :b)))))

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

