(ns solr-parse.eval
  (:use     [midje.sweet])
  (:use     [clojure.pprint       :only [pprint pp print-table]])
  (:use     [clojure.repl         :only [doc find-doc dir]])
  (:use     [clojure.java.javadoc :only [javadoc]])
  (:use     [solr-parse.parser    :only [parse-solr example-solr-query]])
  (:require [clojure.string :as s]))

;; ala 4 clj ;;;
;;; A expression evaluator

(def to-query +) ;; hack to be able to redefine a multimethod dispatch

(defn default-transco
  [left right] (list '= (list 'm left) right))

(fact "default-transco"
  (default-transco :a "b") => '(= (m :a) "b"))

(defmulti to-query "Dispatch on the :tag flag"
  (fn [f x] (cond (map? x)       (:tag x)
                 (#{"(" ")"} x) :par
                 (s/blank? x)   :blank)))

(defmethod to-query :par
  [f c] nil)

(fact "to-query :par"
  (to-query default-transco "(") => nil
  (to-query default-transco ")") => nil)

(defmethod to-query :blank
  [f _] nil)

(fact "to-query :blank"
  (to-query default-transco " ") => nil)

(defmethod to-query :symbol
  [f {[x] :content}] (keyword x))

(fact "to-query :symbol"
  (to-query default-transco {:tag :symbol :content ["a"]}) => :a)

(defmethod to-query :string
  [f {[_ q _] :content}] q)

(fact "to-query :string"
  (to-query default-transco {:tag :string, :content ["\"" "a" "\""]}) => "a")

(defmethod to-query :key-value
  [f {[x _ y] :content}] (f (to-query f x) (to-query f y)))

(fact "to-query :key-value"
  (to-query default-transco {:tag :key-value,
                             :content
                             [ {:tag :symbol, :content ["b"]} ":" {:tag :symbol, :content ["2"]}]})
  => '(= (m :b) :2))

(defmethod to-query :binary-op
  [f {[o] :content}]
  (if-let [r ({"AND" 'and
               "OR"  'or} o)]
    r
    (throw (Exception.))))

(fact "to-query :binary-op"
  (to-query default-transco {:tag :binary-op, :content ["AND"]}) => 'and
  (to-query default-transco {:tag :binary-op, :content ["OR"]}) => 'or
  (to-query default-transco {:tag :binary-op, :content ["X"]}) => (throws Exception))

(defmethod to-query :root
  [f {c :content}]
  (map (partial to-query f) c))

(fact "to-query :root"
  (let [q {:tag :root,
           :content
           [{:tag :key-value,
             :content
             [{:tag :symbol, :content ["a"]}
              ":"
              {:tag :string, :content ["\"" "b" "\""]}]}]}]
    (to-query default-transco q) => '((= (m :a) "b"))))


(defn- rm-nil "Given a nested datastructure, returns the same but without nil"
  [s] (if (sequential? s)
        (map rm-nil (remove nil? s))
        s))

(fact "rm-nil"
  (rm-nil '(nil)) => ()
  (rm-nil '((nil))) => '(())
  (rm-nil 'a) => 'a
  (rm-nil '(a nil a nil)) => '(a a))

(defn- dup-par?
  [s] (and (sequential? s)
           (sequential? (first s))
           (not (second s))))

(fact "dup-par?"
  (dup-par? :a) => false
  (dup-par? '(:a)) => false
  (dup-par? '((:a))) => true)

(defn- rm-dup-par-1 "given a form 'peel' its parens til it only one level"
  [s] (if (dup-par? s)
        (rm-dup-par-1 (first s))
        s))

(fact "rm-dup-par-1"
  (rm-dup-par-1 'a) => 'a
  (rm-dup-par-1 '(a)) => '(a)
  (rm-dup-par-1 '((a))) => '(a)
  (rm-dup-par-1 '(((a)))) => '(a))

(defn- rm-dup-par "Remove duplicate parens from the given form (recursively)"
  [s] (if (sequential? s)
        (map rm-dup-par
             (rm-dup-par-1 s))
        s))

(fact "rm-dup-par"
  (rm-dup-par '((a (b) ((c))))) => '(a (b) (c)))

(defn compile-solr-query
  [f q]
  (-> (to-query f q)
      rm-nil
      rm-dup-par))

(defn binary-op?
  [x] (= (:tag x) :binary-op))

(defn and?
  [x] (and (binary-op? x) (= (:content x ) ["AND"])))

(defn or?
  [x] (and (binary-op? x) (= (:content x ) ["OR"])))

(defmethod to-query :expr-par
  [f {s :content}]
  (map (fn [x] (if (sequential? x)
                (map (partial to-query f) x)
                (to-query f x)))
       (map (fn [x] (if (and (sequential? x) (some and? x))
                     (cons (first (filter and? x))
                           (remove and? x))
                     x))
            (if (some or? s)
              (let [o (first (filter or? s))]
                (cons o (take-nth 2 (partition-by or? s))))
              [s]))))

(fact "a:b OR c:d AND e:f"
  (let [example-ng {:tag :root
                    :content
                    [{:tag :expr-par
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
    (compile-solr-query default-transco example-ng) => '(or (= (m :a) :b) (and (= (m :c) :d) (= (m :e) :f)))))

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
    (compile-solr-query default-transco example2) => '(or (and (= (m :a) :b) (= (m :b) :c))
                                                          (and (= (m :e) :f) (= (m :g) :d)))))

(defmethod to-query :expr-par-simple
  [f {q :content}]
  (map (partial to-query f) q))

(fact "to-query :expr-par-simple"
  (let [q {:tag :expr-par-simple,
           :content
           ["("
            {:tag :key-value,
             :content
             [{:tag :symbol, :content ["a"]}
              ":"
              {:tag :symbol, :content ["b"]}]}
            ")"]}]
    (compile-solr-query default-transco q)) => '(= (m :a) :b))

(defmethod to-query :prefix-op
  [f {[q] :content}] (if-let [o ({"-" 'not} q)]
                     o
                     (throw (RuntimeException. (str "Unknown prefix operator: " q)))))

(fact "to-query :prefix-op"
  (to-query default-transco {:tag :prefix-op, :content ["-"]}) => 'not
  (to-query default-transco {:tag :prefix-op, :content [:a]}) => (throws RuntimeException))

(defmethod to-query :expr-prefixed
  [f {q :content}]
  {:pre [(= 2 (count q))]}
  (list (to-query f (first  q))
        (to-query f (second q))))

(fact "to-query - expr-prefixed"
  (let [q {:tag :expr-prefixed,
           :content
           [{:tag :prefix-op, :content ["-"]}
            {:tag :key-value,
             :content
             [{:tag :symbol, :content ["a"]}
              ":"
              {:tag :symbol, :content ["b"]}]}]}]
    (to-query default-transco q) => '(not (= (m :a) :b))))

(fact "-a:b"
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
    (compile-solr-query default-transco example-not) => '(not (= (m :a) :b))))

(fact "IT - ((-w:b AND ((w:\"P\" AND w:\"M\" a:\"a\"))) OR (w:b AND -((w:\"\nP\" AND w:\"M\" AND a:\"a\"))))"
  (let [q {:tag :root,
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
              ")"]}]}]
    ;; ((-w:b AND ((w:\"P\" AND w:\"M\" a:\"a\"))) OR (w:b AND -((w:\"\nP\" AND w:\"M\" AND a:\"a\"))))
    (compile-solr-query default-transco q) => '(or (and (not (= (m :w) :b))
                                                        (and (= (m :w) "P")
                                                             (= (m :w) "M")
                                                             (= (m :a) "a")))
                                                   (and (= (m :w) :b)
                                                        (not (and (= (m :w) "\nP")
                                                                  (= (m :w) "M")
                                                                  (= (m :a) "a")))))))

(defn compile-query
  "Compile the query into a data structure representing the function"
  ([f q]
     (compile-solr-query f (parse-solr q))))

(fact "compile-query - without parenthesis"
  (compile-query default-transco "a:b AND c:d")               => '(and (= (m :a) :b) (= (m :c) :d))
  (compile-query default-transco "a:b AND c:d AND e:f")       => '(and (= (m :a) :b) (= (m :c) :d) (= (m :e) :f))
  (compile-query default-transco "a:b AND c:d OR e:f")        => '(or (and (= (m :a) :b) (= (m :c) :d))
                                                                      (= (m :e) :f))
  (compile-query default-transco "a:b AND c:d OR e:f OR g:h") => '(or (and (= (m :a) :b) (= (m :c) :d))
                                                                      (= (m :e) :f)
                                                                      (= (m :g) :h)))

(fact "compile-query - with parenthesis"
  (compile-query default-transco "(a:b AND c:d) OR (e:f)")        => '(or (and (= (m :a) :b) (= (m :c) :d))
                                                                          (= (m :e) :f))
  (compile-query default-transco "(a:b AND c:d) OR (e:f) OR g:h") => '(or (and (= (m :a) :b) (= (m :c) :d))
                                                                          (= (m :e) :f)
                                                                          (= (m :g) :h)))
(defn default-transco-reversed
  [left right] (list '= (list 'm right) left))

(fact "default-transco-reversed"
  (default-transco-reversed :a "b") => '(= (m "b") :a))

(fact "compile-query - with parenthesis"
  (compile-query default-transco-reversed "(a:b AND c:d) OR (e:f)")        => '(or (and (= (m :b) :a) (= (m :d) :c))
                                                                                   (= (m :f) :e))
  (compile-query default-transco-reversed "(a:b AND c:d) OR (e:f) OR g:h") => '(or (and (= (m :b) :a) (= (m :d) :c))
                                                                                   (= (m :f) :e)
                                                                                   (= (m :h) :g)))
