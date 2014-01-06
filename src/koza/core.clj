(ns koza.core
  (:gen-class)
  (:require clojure.zip)
  (:use [clojure.string] [clojure.test]))

(defn generate-tree
    "Generates a genetic-programming individual using the 'grow' method"
    [terms funcs depth max-depth]
    (def select-from
        (if (== depth 0)
            funcs
            (if (== depth max-depth)
                terms
                (concat terms funcs))))
    (def root-choice (rand-nth select-from))
    (def root (first root-choice))
    (def arg-count (last root-choice))
    (if (== arg-count 0)
        root
        (concat [root] 
            (for
                [i (range arg-count)]
                (generate-tree terms funcs (+ 1 depth) max-depth)
        ))))


(defn recursive-count
    [coll]
    (apply + (for [el coll]
         (if (not (seq? el))
             1
             (recursive-count el)
    ))))


(defn replace-location
    [m w]
    (clojure.zip/root
        (clojure.zip/replace
            m
            (clojure.zip/node w))))

(defn get-random-subtree
    [z]
    (def walk-len (rand-int (recursive-count z)))
    (def st (nth
        (iterate clojure.zip/next z) 
        walk-len))
    (def st-node (clojure.zip/node st))
    (if (and (complement (seq? st-node)) (function? st-node))
            (clojure.zip/up st)
            st))

(defn crossover
    [man woman]
    (def m-zip (clojure.zip/seq-zip man))
    (def w-zip (clojure.zip/seq-zip woman))
    (let [mx (get-random-subtree m-zip)
          wx (get-random-subtree w-zip)]
        (println (clojure.zip/node mx) (clojure.zip/node wx))
        [(replace-location mx wx) (replace-location wx mx)])
)

(defn generate-individual 
    [terms funcs max-depth]
    (generate-tree terms funcs 0 max-depth))


(defn -main
  [& args]
  (def terms '(1 2 3))
  (def tterms (map vector terms (take (count terms) (repeat 0))))
  (def funcs '([+ 2] [- 2] [* 2]))
  (def ret (generate-individual tterms funcs 4))
  (println ret)
  (println (first (crossover ret ret)))
)
