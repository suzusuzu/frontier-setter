(defconf
  :leaf-node ['x 'natulal-number]                           ;葉ノードの要素
  :inner-node ['+ '- '*]                                       ;内部ノードの要素
  :population 100                                           ;個体数
  :limit-generation 'INF                                    ;最大世代
  :island-num 10                                            ;島の数
  :cross-probability 0.8                                    ;交配確率
  :inversion-probability 0.1                                ;逆位確率
  :mutation-probability 0.1                                 ;突然変異確率
  :evaluation-func #(%)                                     ;評価関数
  )
