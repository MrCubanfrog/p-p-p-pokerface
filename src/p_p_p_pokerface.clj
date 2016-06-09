(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rnk _] card]
    (if (Character/isDigit rnk)
      (Integer/valueOf (str rnk))
      (get {\T 10, \J 11, \Q 12, \K 13, \A 14} rnk))))

 (defn suit [card]
  (let [[_ st] card]
    (str st)))

(defn pair? [hand]
  (== 2 (apply max (vals (frequencies (map rank hand))))))

(defn three-of-a-kind? [hand]
  (== 3 (apply max (vals (frequencies (map rank hand))))))

(defn four-of-a-kind? [hand]
  (== 4 (apply max (vals (frequencies (map rank hand))))))

(defn flush? [hand]
  (== 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3](sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (or
    (= [1 4] (sort (vals (frequencies (map rank hand)))))
    (= [1 2 2] (sort (vals (frequencies (map rank hand)))))))

(defn straight? [hand]
  (or
    (= (sort (map rank hand)) (range (apply min (map rank hand)) (+ (apply max (map rank hand)) 1)))
    (and (= 14 (apply max (map rank hand))) (= [1 2 3 4 5] (range 1 (+ (nth (sort (map rank hand)) 3) 1))))
  ))

(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)
    ))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]       [pair? 1]           [two-pairs? 2]
                   [three-of-a-kind? 3] [straight? 4]       [flush? 5]
                   [full-house? 6]      [four-of-a-kind? 7] [straight-flush? 8]
                    }
        hands (filter #((first %) hand) checkers)
        values (map #(second %) hands)]

          (apply max values)
    ))
