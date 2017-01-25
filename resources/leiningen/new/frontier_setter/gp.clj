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

(defn get-node-indexes
      [tree]
      (let [res-list (atom [])]
           (letfn [(f
                     [tree' index]
                     (do (if (list? (first tree'))
                           (f (first tree') (conj index 'first))
                           (swap! res-list conj (conj index 'first)))
                         (if-not (empty? (rest tree'))
                           (f (rest tree') (conj index 'rest)))))]
                  (do (f tree [])
                      @res-list)))
      )

(defn random-node-index
      [tree]
      (rand-nth (get-node-indexes tree)))

(defn mutate
      [leafs inners tree]
      (let [rand-index (random-node-index tree)]
           (letfn [(f
                     [tree' index]
                     (cond
                       (= index rand-index) (random-tree leafs inners 10)
                       (or (not (list? tree'))
                           (empty? tree')) tree'
                       :else (cons (f (first tree') (conj index 'first)) (f (rest tree') (conj index 'rest)))))] (f tree []) )))

