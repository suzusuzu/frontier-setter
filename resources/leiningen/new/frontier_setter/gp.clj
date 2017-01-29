(ns {{sanitized}}.util.gp
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class))

(defn random-tree
      [leafs inners depth]
      (let [leafs-num (count leafs)
            inners-num (count inners)
            sum (+ (count leafs) (count inners))]
           (letfn [(f
                     [depth']
                     (if (zero? (dec depth'))
                       (rand-nth leafs)
                       (if (< (rand-int sum) leafs-num)
                         (rand-nth leafs)
                         (list (rand-nth inners) (f (dec depth')) (f (dec depth'))))))] (f depth)))
      )


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


(defn get-inner-node-indexes
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
                      (let [indexes (apply vector (filter #(and (list? (get-node tree %)) (> (count (rest (get-node tree %))) 0))  (get-indexes child-num index)))]
                           (do (swap! res-list concat indexes)
                               (doall (map f indexes)))))
                     ))]
           (do (f ['identity])
                  @res-list))))

(defn random-node-index
      [tree]
      (rand-nth (get-node-indexes tree)))

(defn random-inner-node-index
      [tree]
      (rand-nth (get-inner-node-indexes tree)))

(defn mutate
      [leafs inners tree depth]
      (let [rand-index (random-node-index tree)]
           (letfn [(f
                     [tree' index]
                     (cond
                       (= index rand-index) (random-tree leafs inners depth)
                       (or (not (list? tree'))
                           (empty? tree')) tree'
                       :else (cons (f (first tree') (conj index 'first)) (f (rest tree') (conj index 'rest)))))] (f tree ['identity]) )))

(defn cross
      [tree1 tree2]
      (let [rand-index1 (random-node-index tree1)
            rand-index2 (random-node-index tree2)]
           (letfn [(f
                     [tree subtree index rand-index]
                     (cond
                       (= index rand-index) subtree
                       (or (not (list? tree))
                           (empty? tree)) tree
                       :else (cons (f (first tree) subtree (conj index 'first) rand-index) (f (rest tree) subtree (conj index 'rest) rand-index))))]
                  [(f tree1 (get-node tree2 rand-index2) ['identity] rand-index1 ) (f tree2 (get-node tree1 rand-index1) ['identity] rand-index2 )])))

(defn not-same-shuffle
      [list]
      (loop [list' (shuffle list)]
            (if (= list list')
              (recur (shuffle list))
              list'))
      )


(defn inverse
      [tree]
      (let [rand-index (random-inner-node-index tree)]
           (letfn [(f
                     [tree' index]
                     (cond
                       (= index rand-index) (cons (first tree') (apply list (not-same-shuffle (rest tree'))))
                       (or (not (list? tree'))
                           (empty? tree')) tree'
                       :else (cons (f (first tree') (conj index 'first)) (f (rest tree') (conj index 'rest)))))] (f tree ['identity])))
      )

(defn generate-island
  [num leafs inners depth]
  (apply vector (take num (repeatedly #(random-tree leafs inners depth)))))


(defn tree-roulette
  [island evaluation-func]
  (apply vector (reduce concat [] (map #(take (evaluation-func %) (repeat %)) island)))
  )

(defn evolution
  [island cross-p mutation-p inversion-p evaluation-func leafs inners depth]
  (let [island-num (count island)
        evolution-roulette (concat
                             (take (* cross-p 10) (repeat :cross))
                             (take (* mutation-p 10) (repeat :mutation))
                             (take (* inversion-p 10) (repeat :inversion))
                             (take (- 1000 (* 10 (+ cross-p mutation-p inversion-p))) (repeat :id)))
        tree-roulette (tree-roulette island evaluation-func)]
    (loop [island' []]
      (if (>= (count island') island-num)
        island'
        (let [rand-f (rand-nth evolution-roulette)]
          (cond
            (= rand-f :cross) (if (> (- island-num (count island')) 1)
                                (recur (apply vector (concat island' (cross (rand-nth tree-roulette) (rand-nth tree-roulette)))))
                                (recur (conj island' (rand-nth (cross (rand-nth tree-roulette) (rand-nth tree-roulette))))))
            (= rand-f :mutation) (recur (conj island' (mutate leafs inners (rand-nth tree-roulette) depth)))
            (= rand-f :inversion) (recur (conj island (inverse (rand-nth tree-roulette))))
            :else (recur (conj island (rand-nth tree-roulette)))))))))


