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

(defn get-node
      [tree index]
      (if (empty? index)
        tree
        (get-node ((resolve (first index)) tree) (rest index))))


(defn get-node-indexes
      [tree]
      (let [res-list (atom [['identity]])]
           (letfn [(get-indexes
                     [child-num index]
                     (loop [child-num' child-num conjs []]
                               (if (<= child-num' 1)
                                 (conj conjs (apply vector (concat index (conj (apply vector (take child-num' (repeat 'rest))) 'first))))
                                 (recur (dec child-num') (conj conjs (apply vector (concat index (conj (apply vector (take child-num' (repeat 'rest))) 'first))))))))
                   (f
                     [index]
                     (if (list? (get-node tree index))
                     (let [child-num (count (rest (get-node tree index)))]
                          (let [indexes (get-indexes child-num index)]
                               (do (swap! res-list concat indexes)
                                   (doall (map f indexes)))))))]
                  (do (f ['identity])
                      @res-list))))

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

(defn cross
      [tree1 tree2]
      (let [rand-index1 (random-node-index tree1) rand-index2 (random-node-index tree2)]
           (letfn [(f
                     [tree subtree index rand-index]
                     (cond
                       (= index rand-index) subtree
                       (or (not (list? tree))
                           (empty? tree)) tree
                       :else (cons (f (first tree) subtree (conj index 'first) rand-index) (f (rest tree) subtree (conj index 'rest) rand-index))))]
                  [(f tree1 '(nil) [] rand-index1 ) (f tree2 '(nil) [] rand-index2 )])))

