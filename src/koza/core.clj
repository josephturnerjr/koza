; I am sure I am doing something wrong here
(ns koza.core
  (:gen-class)
  (:require clojure.zip)
  (:use [clojure.string] [clojure.test]))

(defn generate-tree
    "Generates a genetic-programming individual using the 'grow' method"
    [terms funcs depth max-depth]
    (def select-from
        (if (== depth 0)
            ; Only select from functions for the first level
            funcs
            (if (== depth max-depth)
                ; Only select from terms at max depth
                terms
                ; Otherwise select from everything
                (concat terms funcs))))
    ; Make a choice for the node itself
    (def root-choice (rand-nth select-from))
    (def root (first root-choice))
    (def arg-count (last root-choice))
    (if (== arg-count 0)
        ; If we've selected a terminal, done
        root
        ; Otherwise recursively generate subtrees for each argument
        (concat [root] 
            (for
                [i (range arg-count)]
                (generate-tree terms funcs (+ 1 depth) max-depth)
        ))))


(defn recursive-count
    "Counts the elements in a form (or nested data structure)"
    [coll]
    (apply + (for [el coll]
         (if (not (seq? el))
             1
             (recursive-count el)
    ))))


(defn replace-location
    "Replace a subtree"
    [m w]
    (clojure.zip/root
        (clojure.zip/replace
            m
            (clojure.zip/node w))))

(defn get-random-subtree
    "Select a random subtree for a given tree"
    [z]
    ; Zipper uses a depth-first traversal for next
    (def walk-len (rand-int (recursive-count z)))
    ; Step through a random number of steps
    (def st (nth
        (iterate clojure.zip/next z) 
        walk-len))
    (def st-node (clojure.zip/node st))
    ; Unfortunately zipper doesn't understand prefix notation, so if we wind
    ;   up with a function symbol, go up a level to get the subtree
    (if (and (complement (seq? st-node)) (function? st-node))
            (clojure.zip/up st)
            st))

(defn crossover
    "Genetic crossover for two individuals"
    [man woman]
    (def m-zip (clojure.zip/seq-zip man))
    (def w-zip (clojure.zip/seq-zip woman))
    ; Pick a random subtree of each and swap them
    (let [mx (get-random-subtree m-zip)
          wx (get-random-subtree w-zip)]
        [(replace-location mx wx) (replace-location wx mx)])
)

(defn generate-individual 
    "Generate an individual based on the terminals and functions"
    [terms funcs max-depth]
    (generate-tree terms funcs 0 max-depth))

(defn generate-population
    "Generate a population of individuals"
    [terms funcs max-depth size]
    (take size (repeatedly #(generate-individual terms funcs max-depth))))

(defn evaluate-fitness
    "Evaluates individuals in a population based on their fitness"
    [fitness-func pop]
    (for [ind pop]
         [(fitness-func ind) ind])
)

(defn eval-bound
    "Evals a form with a passed-in binding. Probably a better way to do this"
    [ind terms vals]
    (def bound (seq (conj ['let [terms vals]] ind)))
    (eval bound))

(defn primary-partial
    "Partial, but you pass in the first argument"
    [f & args]
    (fn [primary] (apply f primary args)))

(defn iterate-population
    "Takes a population from generation n to generation n+1"
    [pop crossover-prob]
    (def nr-crossover (* crossover-prob (count pop)))
    (def nr-reproduce (- (count pop) nr-crossover))
)

(defn -main
  [& args]
  (def population-size 500)
  (def generations 51)
  (def max-depth 6)
  (def terms '[a])
  (def tterms (map vector terms (take (count terms) (repeat 0))))
  (def funcs '([+ 2] [- 2] [* 2]))
  (def ret (generate-population tterms funcs max-depth population-size))
  (println (evaluate-fitness (primary-partial eval-bound terms '[1]) ret))
)
