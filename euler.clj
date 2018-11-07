(ns eul
  (:require [clojure.string :as str]))

; https://projecteuler.net/problem=1
(reduce + (filter #(or (= (mod % 3) 0) (= (mod % 5) 0)) (range 1000)))

; https://projecteuler.net/problem=2
(defn fib
  ([]
   (fib 1 1))
  ([a b]
   (lazy-seq (cons a (fib b (+ a b))))))

(reduce + (filter #(even? %) (take-while #(< % 4000000) (fib))))

; https://projecteuler.net/problem=16

(defn sum-digits [num]
  (let [str-num (str num)
        digits (map #(- (int %) 48) str-num)]
    (reduce + digits)))

(sum-digits (.toBigInteger (BigDecimal. (Math/pow 2 1000))))

;https://projecteuler.net/problem=8
(def p8-input "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(let [parts (partition 13 1 p8-input)]
  (->>
   parts
   (map (fn [part]
          (->> part
               (map #(- (int %) 48))
               (reduce *))))
   (reduce max)))

; https://projecteuler.net/problem=17
(defn num-to-string [num]
  (cond 
    (< num 20)
    (case num
      0 ""
      1 "one"
      2 "two"
      3 "three"
      4 "four"
      5 "five"
      6 "six"
      7 "seven"
      8 "eight"
      9 "nine"
      10 "ten"
      11 "eleven"
      12 "twelve"
      13 "thirteen"
      14 "fourteen"
      15 "fifteen"
      16 "sixteen"
      17 "seventeen"
      18 "eighteen"
      19 "nineteen")
    (< num 100)
    (str
     (case (quot num 10)
       2 "twenty"
       3 "thirty"
       4 "forty"
       5 "fifty"
       6 "sixty"
       7 "seventy"
       8 "eighty"
       9 "ninety")
     (num-to-string (mod num 10)))
    (< num 1000)
    (str
     (num-to-string (quot num 100))
     "hundred"
     (let [rest (num-to-string (mod num 100))]
       (if (= rest "")
         ""
         (str "and" rest))))
    (= num 1000)
    "onethousand"))

(count (num-to-string 342))
(num-to-string 115)

(->> (range 1 1001)
     (map num-to-string)
     (map count)
     (reduce +))

; https://projecteuler.net/problem=18

(def p18-data
  [
                                [75]
                              [95  64]
                            [17  47  82]
                          [18  35  87  10]
                        [20  4   82  47  65]
                      [19  1   23  75  3   34]
                    [88  2   77  73   7  63  67]
                  [99  65   4  28   6  16  70  92]
                [41  41  26  56  83  40  80  70  33]
              [41  48  72  33  47  32  37  16  94  29]
            [53  71  44  65  25  43  91  52  97  51  14]
          [70  11  33  28  77  73  17  78  39  68  17  57]
        [91  71  52  38  17  14  91  43  58  50  27  29  48]
      [63  66   4  68  89  53  67  30  73  16  69  87  40  31]
    [4   62  98  27  23   9  70  98  73  93  38  53  60   4  23]
  ])

;; Build tree
; (defn make-tree [triangle row col]
;   (let [val (get-in triangle [row col])]
;     (if (< (+ 1 row) (count triangle))
;       [val (make-tree triangle (+ 1 row) col) (make-tree triangle (+ 1 row) (+ 1 col))]
;       [val nil nil])))

; (make-tree p18-data 0 0)

(defn dfs [triangle row col max-rows]
 (if (< row max-rows)
   (let [val (get-in triangle [row col])]
     (max (+ val (dfs triangle (+ 1 row) col max-rows))
          (+ val (dfs triangle (+ 1 row) (+ 1 col) max-rows))))
     0))

(dfs p18-data 0 0 (count p18-data))

; https://projecteuler.net/problem=19


; A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
(defn is-leap-year [year]
  (not (or (and (= 0 (mod year 100))
                (< 0 (mod year 400)))
           (< 0 (mod year 4)))))

(def leap-year-months [31 29 31 30 31 30 31 31 30 31 30 31])
(def non-leap-year-months [31 28 31 30 31 30 31 31 30 31 30 31])

(defn day-sums [days]
  (reduce
   (fn [coll e]
     (let [last-sum (last coll)
           last-sum (if (nil? last-sum) 0 last-sum)]
       (conj coll (+ last-sum e))))
   [] days))

(def leap-year-month-sums (day-sums leap-year-months))
(def non-leap-year-month-sums (day-sums non-leap-year-months))

(def year-month-days
  (->> (range 1900 2001)
       (map is-leap-year)
       (map #(if % leap-year-months non-leap-year-months))
       flatten))

(def year-month-sums 
  (day-sums year-month-days))

; 1 Jan 1900 was a Monday
(defn is-sunday [sum]
  (= 6 (mod sum 7)))

(def count-sundays
  (->> year-month-sums
       (drop 11)
       (filter is-sunday)
       count))

;https://projecteuler.net/problem=22

(defn p22 []
  (-> "p022_names.txt"
      slurp
      (str/split #",")
      (->> (map (fn [s]
                  (apply str (remove #(= \" %) s)))))
      sort
      (->> (map (fn [name]
                  (reduce (fn [p e]
                            (+ p (- (int e) 64)))
                          0 name))))
      (->> (map-indexed (fn [idx sum] (* (+ 1 idx) sum))))
      (->> (reduce +))
    ; (->> (take 10)))
      ))

; https://projecteuler.net/problem=23
; Also, see https://codereview.stackexchange.com/questions/206966/improving-performance-of-this-code

(defn sum-div [num]
  (->> (range 2 (+ 1 (int (Math/ceil (Math/sqrt num)))))
       (filter #(= (mod num %) 0))
       (map (fn [x]
              (let [d (/ num x)]
                (if (= x d)
                  x
                  [x d]))))
       (flatten)
       (distinct)
       (apply +)
       (inc)))

(def abundants
  (->> (range 12 28123)
       (map #(vector % (sum-div %)))
       (filter #(> (second %) (first %)))
       (map first)
       (set)))

(defn is-sum-of-two-abundants?
  [num]
    (some (fn [ab]
            (abundants (- num ab)))
          abundants))

(time
 (->> (range 28124)
      (remove is-sum-of-two-abundants?)
      (apply +)))

; https://projecteuler.net/problem=24

(def elements [0 1 2])

(defn powers-of-two
  ([]
   (powers-of-two 1))
  ([a]
   (lazy-seq
    (cons a (powers-of-two (* a 2))))))

;; I have no idea how this works.
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

; (apply str (nth (permutations "0123456789") 999999))

; https://projecteuler.net/problem=26

(defn divide 
  ([num divisor prev len]
   (if (prev [num divisor])
     (- len (prev [num divisor]))
     (let [r (rem (* 10 num) divisor)]
       (if (zero? r)
         len
         (recur r divisor (assoc prev [num divisor] len) (inc len))))))
  ([divisor]
   (divide 1 divisor {} 0)))

(->> (map #(vector (divide %) %) (range 2 1000))
     (into (sorted-map))
     last
     second)

; https://projecteuler.net/problem=27
