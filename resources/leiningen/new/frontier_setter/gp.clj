(ns {{sanitized}}.util.gp
  (:gen-class))

(defn random-tree
      [leafs inners depth]
      (letfn [(f
                [depth']
                (if (or (zero? depth')
                        (zero? (rand-int 2)))
                  (rand-nth leafs)
                  (list (rand-nth inners) (f (dec depth')) (f (dec depth')))))]
             (f depth)))
