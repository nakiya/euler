(letfn [(divisor-sum [n]
          (->> (for [i (range 2 n)]
                 (if (zero? (mod n i))
                   (quot n i)
                   0))
               (reduce +)
               (inc)))]
  (->> (range 1 10001)
       (filter #(and (= % (divisor-sum (divisor-sum %)))
                     (not= % (divisor-sum %))))
       (reduce +)))
