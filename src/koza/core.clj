(ns koza.core
  (:gen-class)
  (:use clojure.string))

(defn generate-tree
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


(defn generate-individual 
    [terms funcs max-depth]
    (generate-tree terms funcs 0 max-depth))


(defn -main
  [& args]
  (def terms '(1 2 3))
  (def tterms (map vector terms (take (count terms) (repeat 0))))
  (def funcs '((+, 2) (-, 2) (*, 2)))
  (def ret (generate-individual tterms funcs 4))
  (println ret)
  (println (eval ret))
)
