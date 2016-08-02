(ns erdos.core
  (:gen-class))

(defn uni-insert
  [graph [a b]]
  (assoc graph a (conj (graph a '()) b)))

(defn insert
  "adds path to graph"
  [graph [a b]]
  (let [gprime (uni-insert graph [a b])]
    (uni-insert gprime [b a])))

(defn build-graph
  "build the graph structure"
  [edges]
  (reduce insert {} edges))

(defn walk
  "walk graph depth first, add nodes to visited set"
  [graph node visited]
  (if (visited node)
    visited
    (reduce into #{}
      (map #(walk graph % (conj visited node))
        (graph node '())))))

(defn count-components
  "count the number of seperate components"
  [graph n]
  (count
    (set
      (map #(walk graph % #{}) (range n)))))

(defn -main
    "I don't do a whole lot ... yet."
    [& args]
    (let [edges '([0 1]
                  [1 2]
                  [0 1]
                  [3 4]
                  [4 6]
                  [4 5]
                  [6 3])
           g (build-graph edges)]
      (println g)
      (println (count-components g 8))))
