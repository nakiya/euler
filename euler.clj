(ns eul
  (:require [clojure.string :as str]
            [clojure.set :as cs]
            [clojure.math.combinatorics :as combo]))

; https://projecteuler.net/problem=1
(defn problem-1 []
  (reduce + (filter #(or (= (mod % 3) 0) (= (mod % 5) 0)) (range 1000))))

; https://projecteuler.net/problem=2
(defn fibonacci
  ([]
   (fibonacci 1 1))
  ([a b]
   (lazy-seq (cons a (fibonacci b (+ a b))))))

(defn problem-2 []
  (reduce + (filter #(even? %) (take-while #(< % 4000000) (fibonacci)))))

;; https://projecteuler.net/problem=3

(defn problem-3 []
  (let [num 600851475143
        sr (inc (int (Math/sqrt num)))
        get-factors (fn ([n div factors]
                         (cond (<= n 1) factors
                               (= 0 (mod n div)) (recur (quot n div) div (conj factors div))
                               :else (recur n (inc div) factors))))]
    (last (get-factors num 2 []))))

;; https://projecteuler.net/problem=4
(defn- is-palindrome? [num]
  (let [snum (str num)]
    (= snum (str/reverse snum))))

(defn problem-4 []
  (->> (combo/combinations (range 100 1000) 2)
       (map #(apply * %))
       (filter is-palindrome?)
       (sort >)
       (first)))

;; https://projecteuler.net/problem=5

(defn problem-5 []
  (* 16 9 5 7 11 13 17 19))

;; https://projecteuler.net/problem=6

(defn- square [n]
  (* n n))

(defn- problem-6 []
  (- (->> (range 1 101)
          (reduce +)
          (square))
     (->> (range 1 101)
          (map square)
          (reduce +))))

;; https://projecteuler.net/problem=7
;; Cheating. See gen-primes someway down below.
(defn- problem-7 []
  (->> (gen-primes)
       (drop 10000)
       (first)))

;https://projecteuler.net/problem=8
(defn problem-8 [] (let [p8-input "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
                         parts (partition 13 1 p8-input)]
                     (->>
                      parts
                      (map (fn [part]
                             (->> part
                                  (map #(- (int %) 48))
                                  (reduce *))))
                      (reduce max))))

;; https://projecteuler.net/problem=9
(defn- problem-9 []
  (let [[a b]
        (->> (combo/combinations (range 1000) 2)
             (map (fn [[a b]] [(+ (square a) (square b)) a b]))
             (filter (fn [[c2 _ _]]
                       (let [sr (int (Math/sqrt c2))]
                         (= c2 (square sr)))))
             (filter (fn [[_ a b]]
                       (= (+ (square a) (square b)) (square (- 1000 a b)))))
             (filter (fn [[_ a b]]
                       (and (> a 0) (> b 0))))
             (first)
             (rest))]
    (* a b (- 1000 a b))))

;; https://projecteuler.net/problem=10
(defn problem-10 []
  (->> (gen-primes)
       (take-while #(< % 2000000))
       (reduce +)))

;; https://projecteuler.net/problem=11
(defn- problem-11 []
  (let [data
        (->> "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
            49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
            81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
            52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
            22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
            24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
            32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
            67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
            24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
            21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
            78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
            16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
            86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
            19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
            04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
            88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
            04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
            20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
            20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
            01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
             (.split #"\n")
             (map #(.split #"\s+" %))
             (map (partial drop-while empty?))
             (mapcat (partial map #(Integer/parseInt %)))
             (partition 20)
             (map vec)
             (vec))
        transform-fn (fn [x y]
                       (vector
                        (map #(map (fn [[xdelta ydelta] [x y]] [(+ x xdelta) (+ y ydelta)])
                                   %
                                   (repeat [x y]))
                             [[[0 0] [1 0] [2 0] [3 0]]
                              [[0 0] [0 1] [0 2] [0 3]]
                              [[0 0] [1 1] [2 2] [3 3]]
                              [[0 0] [-1 1] [-2 2] [-3 3]]])))
        prod-fn (fn [data coords]
                  (->> (filter (fn [[x y]] (and (>= x 0) (< x 20) (>= y 0) (< y 20)))
                               coords)
                       (map #(get-in data %))
                       (reduce *)))]
    (->>
     (for [i (range 20) j (range 20)]
       [i j])
     (map #(apply transform-fn %))
     (mapcat identity)
     (mapcat identity)
     (map (partial prod-fn data))
     (sort >)
     (first))))

;; https://projecteuler.net/problem=14
(defn problem-14 []
  (let [collatz-fn
        (memoize
         (fn [n]
           (cond (= 1 n) 1
                 (even? n) (inc (collatz-fn (/ n 2)))
                 :else (inc (collatz-fn (inc (* 3 n)))))))])
  (->> (range 1 1000001)
       (map #(vector % (collatz-fn %)))
       (sort-by second >)
       (ffirst)))

;; https://projecteuler.net/problem=15
(defn problem-15 []
  (let
   [routes
    (memoize
     (fn [rows cols]
       (cond (zero? rows) 1
             (zero? cols) 1
             :else (+ (routes (dec rows) cols) (routes rows (dec cols))))))]
    (routes 20 20)))

;; https://projecteuler.net/problem=16

(defn sum-digits [num]
  (let [str-num (str num)
        digits (map #(- (int %) 48) str-num)]
    (reduce + digits)))

(defn problem-16 []
  (sum-digits (.toBigInteger (BigDecimal. (Math/pow 2 1000)))))

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

;; (count (num-to-string 342))
;; (num-to-string 115)

(defn problem-17 [] (->> (range 1 1001)
                         (map num-to-string)
                         (map count)
                         (reduce +)))

; https://projecteuler.net/problem=18

(defn dfs [triangle row col max-rows]
  (if (< row max-rows)
    (let [val (get-in triangle [row col])]
      (max (+ val (dfs triangle (+ 1 row) col max-rows))
           (+ val (dfs triangle (+ 1 row) (+ 1 col) max-rows))))
    0))

(defn problem-18 []
  (let [p18-data
        [[75]
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
         [4   62  98  27  23   9  70  98  73  93  38  53  60   4  23]]]
    (dfs p18-data 0 0 (count p18-data))))

; https://projecteuler.net/problem=19

; A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
(defn is-leap-year [year]
  (not (or (and (= 0 (mod year 100))
                (< 0 (mod year 400)))
           (< 0 (mod year 4)))))

(defn day-sums [days]
  (reduce
   (fn [coll e]
     (let [last-sum (last coll)
           last-sum (if (nil? last-sum) 0 last-sum)]
       (conj coll (+ last-sum e))))
   [] days))

; 1 Jan 1900 was a Monday
(defn is-sunday [sum]
  (= 6 (mod sum 7)))

(defn problem-19 []
  (let [leap-year-months [31 29 31 30 31 30 31 31 30 31 30 31]
        non-leap-year-months [31 28 31 30 31 30 31 31 30 31 30 31]
        leap-year-month-sums (day-sums leap-year-months)
        non-leap-year-month-sums (day-sums non-leap-year-months)
        year-month-days (->> (range 1900 2001)
                             (map is-leap-year)
                             (map #(if % leap-year-months non-leap-year-months))
                             flatten)
        year-month-sums (day-sums year-month-days)]
    (->> year-month-sums
         (drop 11)
         (filter is-sunday)
         count)))

;https://projecteuler.net/problem=20
(defn problem-20 []
  (letfn [(big-factorial [n]
            (cond (zero? n) 1N
                  :else (* n (big-factorial (dec n)))))]
    (->> (big-factorial 100)
         (str)
         (map #(- (int %) 48))
         (reduce +))))

;https://projecteuler.net/problem=21

(defn problem-21 []
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
         (reduce +))))

;https://projecteuler.net/problem=22

(defn problem-22 []
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
      (->> (reduce +))))

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

(defn problem-23 []
  (->> (range 28124)
       (remove is-sum-of-two-abundants?)
       (apply +)))

; https://projecteuler.net/problem=24

;; I have no idea how this works.
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn problem-24 []
  (apply str (nth (permutations "0123456789") 999999)))

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

(defn problem-26 []
  (->> (map #(vector (divide %) %) (range 2 1000))
       (into (sorted-map))
       last
       second))

; https://projecteuler.net/problem=27

(defn divides? [n divisor] (= 0 (mod n divisor)))

(def find-divisor
  (memoize
   (fn [n test-divisor]
     (cond (> (* test-divisor test-divisor) n) n
           (= 0 (mod n test-divisor)) test-divisor
           :else (recur n (+ 1 test-divisor))))))

(def prime?
  (memoize
   (fn [n]
     (if (neg? n)
       false
       (= n (find-divisor n 2))))))

(defn quadratic-vals
  ([a b n]
   (lazy-seq (cons (+ (* n n) (* a n) b)
                   (quadratic-vals a b (+ 1 n)))))
  ([a b]
   (quadratic-vals a b 0)))

(defn problem-27 []
  (->> (for [i (range -1000 1001)
             j (range -1000 1001)]
         (->> (quadratic-vals i j)
              (take-while prime?)
              (count)
              (conj [[i j]])))
       (sort-by second >)
       (ffirst)
       (apply *)))

; https://projecteuler.net/problem=28

;; Each rectangle has sides that are +2 in length than previous.
(defn diagonal-vals
  ([dim last-val]
   (lazy-seq (cons (map + (repeat last-val) (map * (repeat dim) [1 2 3 4]))
                   (diagonal-vals (+ 2 dim) (+ last-val (* 4 dim))))))
  ([]
   (diagonal-vals 2 1)))

;; n-th term in seq has square length (2n + 1)
;; 1001 length rect = 500th elem.
;; Finally add + 1 for the first one.
(defn problem-28 []
  (->> (diagonal-vals)
       (take 500)
       (flatten)
       (apply +)
       (inc)))

; https://projecteuler.net/problem=29

(defn expt
  ([a b p]
   (if (zero? b)
     p
     (recur a (dec b) (* a p))))
  ([a b]
   (expt a b 1)))

(defn problem-29 []
  (->>
   (for [i (range 2 101)
         j (range 2 101)]
     (expt (biginteger i) (biginteger j)))
   (distinct)
   (count)))

; https://projecteuler.net/problem=30

(defn get-digits [num]
  (->> num
       (str)
       (mapv #(- (int %) 48))))


(defn is-sum-of-powers-of-digits? [num power]
  (->> num
       (get-digits)
       ;; Can increase perf by memoizing expt?
       (mapv #(expt % power))
       (apply +)
       (= num)))

;; Anything beyond 6 * (9^5) cannot be expressed as a sum of powers of 5 of digits.
(defn problem-30 []
  (let [max-sum-of-pw-of-five (* 6 (expt 9 5))]
    (->> (range 2 max-sum-of-pw-of-five)
         (filter #(is-sum-of-powers-of-digits? % 5))
         (apply +))))

; https://projecteuler.net/problem=31

(defn count-change
  ([amount]
   (count-change amount 8))
  ([amount kinds-of-coins]
   (let [coins [1, 2, 5, 10, 20, 50, 100, 200]]
     (cond (= amount 0) 1
           (< amount 0) 0
           (= kinds-of-coins 0) 0
           :else (+ (count-change amount (- kinds-of-coins 1))
                    (count-change (- amount (nth coins (dec kinds-of-coins))) kinds-of-coins))))))

(defn problem-31 []
  (count-change 200))

; https://projecteuler.net/problem=32

(defn is-pan-digital? [[[n1 n2] p]]
  (->> (str n1 n2 p)
       (remove #{\0})
       (distinct)
       (.length)
       (= 9)))

(defn problem-32 []
  (->> (for [i (range 1 10000)
             j (range 1 10000)]
         [i j])
       (filter #(and (< (apply * %) 10000) (>= (apply * %) 1000)))
       (reduce #(conj %1 %2 (apply * %2)) [])
       (apply hash-map)
       (filter is-pan-digital?)
       (map second)
       (distinct)
       (apply +)))

; https://projecteuler.net/problem=33

(defn skip-zero-range [start end]
  (->> (range start end)
       (remove #(zero? (rem % 10)))))


(defn is-curious-fraction? [num denom]
  (let [val (/ num denom)]
    (or (= val (and (= (mod num 10) (mod denom 10))
                    (/ (quot num 10) (quot denom 10))))
        (= val (and (= (mod num 10) (quot denom 10))
                    (/ (quot num 10) (mod denom 10))))
        (= val (and (= (quot num 10) (mod denom 10))
                    (/ (mod num 10) (quot denom 10))))
        (= val (and (= (quot num 10) (quot denom 10))
                    (/ (mod num 10) (mod denom 10)))))))

(defn problem-33 []
  (->> (for [numerator (skip-zero-range 10 100)
             denominator (skip-zero-range 10 100)]
         [numerator denominator])
       (filter #(apply is-curious-fraction? %))
       (filter #(not= (first %) (second %)))
       (filter #(< (first %) (second %)))
       (map #(apply / %))
       (apply *)))


; https://projecteuler.net/problem=34
; Don't care about int overflow as we are not gonna hit it.
(defn factorial
  ([x prod]
   (if (= x 0)
     prod
     (recur (dec x) (* prod x))))
  ([x]
   (factorial x (biginteger 1))))

(def factorial (memoize factorial))

(defn sum-fact-digits [num]
  (->> num
       (str)
       (map #(- (int %) 48))
       (map factorial)
       (apply +)))

;; Need only look until (* 7 (factorial 9)) (9! cannot catch 8 digit numbers.)
(defn problem-34 []
  (->> (range 3 (* 7 (factorial 9)))
       (filter #(= % (sum-fact-digits %)))
       (apply +)))

; https://projecteuler.net/problem=35

(defn rotate-str [s]
  (str (subs s 1) (subs s 0 1)))

(defn rotations [num]
  (let [sn (str num)
        l (.length sn)]
    (->> (iterate (fn [x] (rotate-str x)) sn)
         (take l)
         (map #(Integer. %)))))

(defn is-circular-prime? [num]
  (every? prime? (rotations num)))

(defn problem-35 []
  (->> (range 2 1000000)
       (filter is-circular-prime?)
       (count)))

;; https://projecteuler.net/problem=36

(defn is-palindrome? [s length]
  (if (< length 2)
    true
    (if (not= (first s) (nth s (dec length)))
      false
      (is-palindrome? (subs s 1 (dec length)) (- length 2)))))

(defn is-palindrome-number? [num base]
  (let [num-str (Integer/parseInt num base)]
    (is-palindrome? num-str (.length num-str))))

(defn is-palindrome-number? [num]
  (let [num-str (str num)]
    (is-palindrome? num-str (.length num-str))))

(defn problem-36 []
  (->> (range 1 1000000)
       (filter #(and (is-palindrome-number? % 2)
                     (is-palindrome-number? % 10)))
       (apply +)))

;; https://projecteuler.net/problem=37

;; Primes lazy-seq implementation taken from https://gist.github.com/nbrandaleone/5acf93235be254c52595
;; (def primes
;;   (concat
;;    [2 3 5 7]
;;    (lazy-seq
;;     (let [primes-from
;;           (fn primes-from [n [f & r]]
;;             (if (some #(zero? (rem n %))
;;                       (take-while #(<= (* % %) n) primes))
;;               (recur (+ n f) r)
;;               (lazy-seq (cons n (primes-from (+ n f) r)))))
;;           wheel (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6  4  2
;;                         6 4 6 8 4 2 4 2 4 8 6 4 6 2  4  6
;;                         2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10])]
;;       (primes-from 11 wheel)))))


(defn is-truncatable-prime? [num]
  (let [s (str num)
        length (.length s)
        sub-l (map #(subs s %) (range length))
        sub-r (map #(subs s 0 %) (range 1 length))
        all (map #(Integer. %) (concat sub-l sub-r))]
    (every? prime? all)))

(defn problem-37 []
  (->> (range)
       (drop 10)
       (filter is-truncatable-prime?)
       (take 11)
       (apply +)))

;; https://projecteuler.net/problem=38

(defn problem-38 []
  (->>
   (for [i (range 1 10000)]
     (->> (map #(range 1 %) (range 2 11))
          (map #(map (partial * i) %))
          (map #(apply str %))
          (filter #(= (.length %) 9))))
   (remove empty?)
   (flatten)
   (map distinct)
   (map #(apply str %))
   (filter #(= (.length %) 9))
   (remove #(some #{\0} %))
   (map #(Integer. %))
   (sort >)
   (first)))

;; https://projecteuler.net/problem=39

;; c = p - a - b
;; c^2 = a^2 + b^2
;; (p - a - b)^2 = a^2 + b^2
;; (p - (a + b))^2 = a^2 + b^2
;; p^2 + (a + b)^2 - 2*p*(a+b) = a^2 + b^2
;; p^2 + a^2 + b^2 + 2*a*b - 2*p*(a+b) = a^2 + b^2
;; p^2 + 2(ab - ap - bp) = 0
;; p^2 + 2b(a - p) - 2ap = 0
;; b = (p^2 - 2ap)/2(p-a)

(defn problem-39 []
  (->> (for [p (range 1 1000) a (range 1 p)] [p a])
       (map (fn [[p a :as entry]]
              (let [b (/ (- (* p p) (* 2 a p))
                         (* 2 (- p a)))]
                (if (pos-int? b)
                  (conj entry b)
                  nil))))
       (map #(remove nil? %))
       (remove empty?)
       (map set)
       (distinct)
       (map #(apply vector %))
       (map #(apply max %))
       (frequencies)
       (apply max-key val)
       (key)))

;; https://projecteuler.net/problem=40

(defn char->int [ch]
  (- (int ch) 48))

(defn champernowne
  ([num pos]
   (lazy-seq
    (let [snum (str num)]
      (if (< pos (.length snum))
        (cons (char->int (nth snum pos))
              (champernowne num (inc pos)))
        (champernowne (inc num) 0)))))
  ([]
   (champernowne 1 0)))

(defn problem-40 []
  (->> [0 9 99 999 9999 99999 999999]
       (map #(nth (champernowne) %))
       (apply *)))

; https://projecteuler.net/problem=41
;; Create a function to check if a number is pandigital because we are seeing this a lot.
;; Base is 10 ftm
(defn get-digits [num]
  (->> [num []]
       (iterate (fn [[num digits]]
                  (when (> num 0)
                    [(quot num 10) (conj digits (rem num 10))])))
       (take-while some?)
       (last)
       (second)))

(defn is-pandigital? [num num-digits]
  (let [digits (get-digits num)]
    (and (not-any? #(> % num-digits) digits)
         (= num-digits (count digits))
         (apply distinct? digits))))

;; There's no need to use is-pandigital if we generate pan-digital permuatations in the first place.
(defn problem-41 []
  (->> (range 1 9)
       (map #(range 1 (inc %)))
       (map #(permutations %))
       (mapcat identity)
       (map #(map (fn [i] (char (+ 48 i))) %))
       (map #(apply str %))
       (map #(Integer. %))
       (filter prime?)
       (apply max)))

; https://projecteuler.net/problem=42

(defn gen-triangle-numbers
  ([n]
   (lazy-seq (cons (/ (* n (inc n)) 2)
                   (gen-triangle-numbers (inc n)))))
  ([]
   (gen-triangle-numbers 1)))

(defn word-value [word]
  (->> word
       (map #(- (int %) 64))
       (apply +)))

(defn problem-42 []
  (let [p42-tri-numbers (set (take-while #(< % 1000) (gen-triangle-numbers)))]
    (->> (str/split (slurp "p042_words.txt") #",")
         (map (fn [s]
                (apply str (remove #(= \" %) s))))
         (map word-value)
         ;; max is 416 so we are safe with < 1000 val tri numbers
                                        ;  (apply max))
         (filter p42-tri-numbers)
         (count))))

;; https://projecteuler.net/problem=43

(defn num-from-list [l]
  (->> l
       (reverse)
       (map * [1 10 100 1000 10000 100000 1000000 10000000 100000000 1000000000])
       (apply +)))

(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(defn sub-string-divisibility [digits]
  (->> (range 1 8)
       (map #(subvec (apply vector digits) % (+ 3 %)))
       (map num-from-list)
       (map #(/ %2 %1) [2 3 5 7 11 13 17])
       (every? int?)))

(defn problem-43 []
  (->> (range 0 10)
       (permutations)
       (remove #(= (first %) 0))
       (filter sub-string-divisibility)
       (map num-from-list)
       (apply +)))

;; https://projecteuler.net/problem=44

(defn gen-pentagonal-numbers
  ([n]
   (lazy-seq (cons (/ (* n (- (* 3 n) 1)) 2)
                   (gen-pentagonal-numbers (inc n)))))
  ([]
   (gen-pentagonal-numbers 1)))

(defn problem-44 []
  (let [p44-pentagonals (->> (gen-pentagonal-numbers)
                             (take 1000000)
                             (apply sorted-set))]
    (letfn [(p44-criteria-lz [n]
              (lazy-seq
               (let [pk (/ (* n (- (* 3 n) 1)) 2)
                     pjs (take-while #(< % pk) p44-pentagonals)
                     matches (->> pjs
                                  (map #(vector (+ pk %) (Math/abs (- pk %))))
                                  (filter #(and (p44-pentagonals (first %)) (p44-pentagonals (second %)))))]
                 (if (empty? matches)
                   (p44-criteria-lz (inc n))
                   (cons (second (first matches)) (p44-criteria-lz (inc n)))))))]
      (->> (p44-criteria-lz 1)
           (take 1)
           (first)))))

; https://projecteuler.net/problem=45

(defn lazy-seq-gen
  ([f n]
   (lazy-seq (cons (f n)
                   (lazy-seq-gen f (inc n)))))
  ([f]
   (lazy-seq-gen f 1)))

(def triangle-numbers (lazy-seq-gen #(/ (* % (+ % 1)) 2)))
(def pentagonal-numbers (lazy-seq-gen #(/ (* % (- (* 3 %) 1)) 2)))
(def hexagonal-numbers (lazy-seq-gen #(* % (- (* 2 %) 1))))

(defn problem-45 []
  (let [sz 10000000000
        make-set (fn [generator]
                   (apply sorted-set (take-while #(< % sz) generator)))
        tn (make-set triangle-numbers)
        pn (make-set pentagonal-numbers)
        hn (make-set hexagonal-numbers)]
    (nth
     (filter #(and (pn %) (hn %)) tn)
     2)))

; https://projecteuler.net/problem=46

(defn double-squares-less-than-minus [num]
  (->> (range 1 (inc (int (Math/sqrt num))))
       (map #(- num (* 2 % %)))
       (remove neg?)))

(defn problem-46 []
  (->> (iterate (partial + 2) 3)
       (remove prime?)
       (map #(vector % (double-squares-less-than-minus %)))
       (filter #(not-any? prime? (second %)))
       (take 1)
       (ffirst)))

; https://projecteuler.net/problem=47

;; https://en.wikipedia.org/wiki/Euclidean_algorithm

(defn divides? [num div]
  (= 0 (rem num div)))

;; Copied from https://gist.github.com/unclebob/632303 and modified
(defn factors-starting-at [num div]
  (cond
    (> div (Math/sqrt num)) (if (= num 1) [] [num])
    (divides? num div) (cons div (factors-starting-at div (/ num div)))
    :else (recur (inc div) num)))

(defn prime-factors-of [n]
  (distinct (factors-starting-at 2 n)))

(defn gen-adjacent-nums
  ([n count]
   (lazy-seq (cons (map (partial + n) (range 0 count))
                   (gen-adjacent-nums (inc n) count))))
  ([count]
   (gen-adjacent-nums 1 count)))

(defn problem-47 []
  (->> (gen-adjacent-nums 4)
       (map #(vector % (map prime-factors-of %)))
       (filter #(every? (fn [x] (= (count x) 4)) (second %)))
       (take 1)))

; https://projecteuler.net/problem=48

; I can just use bignums here. Let's try the hard way.
; https://en.wikipedia.org/wiki/Modular_exponentiation#Memory-efficient_method

(defn exp-modulo
  ([c e' b e m]
   (if (>= e' e)
     c
     (recur (mod (* b c) m) (inc e') b e m)))
  ([b e m]
   (exp-modulo 1 0 b e m)))

(defn problem-48 []
  (mod (->> (range 1 1001)
            (map #(exp-modulo % % 10000000000))
            (apply +))
       10000000000))

; https://projecteuler.net/problem=49

(defn get-digits [num]
  (->> num
       (str)
       (map #(- (int %) 48))
       (into '())))

(defn are-permutations-of-each-other? [num1 num2]
  (= (sort (get-digits num1)) (sort (get-digits num2))))

;; Taken from https://stackoverflow.com/a/7625207/466694
(defn gen-primes "Generates an infinite, lazy sequence of prime numbers"
  []
  (letfn [(reinsert [table x prime]
            (update-in table [(+ prime x)] conj prime))
          (primes-step [table d]
                       (if-let [factors (get table d)]
                         (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                                (inc d))
                         (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                                        (inc d))))))]
    (primes-step {} 2)))

(defn problem-49 []
  (let [four-digit-primes (->> (gen-primes)
                               (drop-while #(< % 1000))
                               (take-while #(< % 10000))
                               (apply sorted-set))]
    (->> (for [i four-digit-primes
               j four-digit-primes]
           [i j])
         (remove #(apply = %))
         (filter #(apply are-permutations-of-each-other? %))
         (filter (fn [[n1 n2]]
                   (let [mx (max n1 n2)
                         mn (min n1 n2)
                         diff (- mx mn)
                         next (+ mx diff)]
                     (and (< next 10000)
                          (four-digit-primes next)
                          (are-permutations-of-each-other? n1 next)))))
         (map sort)
         (distinct)
         (map #(cons (+ (second %) (- (second %) (first %))) %))
         (map sort)
         (map #(map str %))
         (map (partial apply str))
         (second))))

; https://projecteuler.net/problem=50

;; Any consecutive run of prime numbers, the sum of which results in a prime has to be odd-length

(defn problem-50 []
  (let [limit 1000000
        is-prime? (->> (gen-primes)
                       (take-while (partial > limit))
                       (set))
        sums (->> (gen-primes)
                  (take-while #(< % limit))
                  (reduce #(vector (conj (first %1) (second %1)) (+ (second %1) %2))
                          [[] 0])
                  (first)
                  (take-while #(< % limit)))]
    (->> (for [i (range (count sums))
               j (range i (count sums))]
           (vector (- (nth sums j) (nth sums i)) (- j i) {:i i :j j}))
         (filter #(is-prime? (first %)))
         (sort-by second >)
         (first))))

; https://projecteuler.net/problem=51


(defn get-neighbors [num]
  (let [str-num (str num)
        res
        (for [digit ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]]
          (when (str/includes? str-num digit)
            [(for [replacement ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]]
               (Integer. (str/replace str-num digit replacement))) num]))]
    (filter some? res)))

(defn problem-51 []
  (let [primes (->> (gen-primes)
                    (take-while #(< % 1000000))
                    (apply sorted-set))]
    (->> primes
         (map get-neighbors)
         (mapcat identity)
         (map (fn [[l num]] (vector (filter primes l) num)))
         (filter (fn [[l num]] (< 7 (count l))))
         (filter (fn [[l num]] (apply = (map #(.length (str %)) l))))
         (map second)
         (sort <)
         (first))))


; https://projecteuler.net/problem=52

(defn problem-52 []
  (->> (range)
       (drop 1)
       (map #(map * [1 2 3 4 5 6] (repeat %)))
       (filter (fn [list] (apply = (map #(sort (get-digits %)) list))))
       (ffirst)))

; https://projecteuler.net/problem=53

(defn n-C-r [n r]
  (/ (factorial n)
     (* (factorial r) (factorial (- n r)))))

(defn problem-53 []
  (->> (for [n (range 23 101)
             r (range 0 (inc n))]
         [n r])
       (map #(apply n-C-r %))
       (filter #(< 1000000 %))
       (count)))

; https://projecteuler.net/problem=54

;; Typical hand
; [[10 :clubs] [:jack :diamonds] [6 :clubs] [4 :hearts] [:ace :hearts]]

(def test-hands [[[[5 :H] [5 :C] [6 :S] [7 :S] [13 :D]]
                  [[2 :C] [3 :S] [8 :S] [8 :D] [10 :D]]]
                 [[[5 :D] [8 :C] [9 :S] [1 :S] [14 :C]]
                  [[2 :C] [5 :C] [7 :D] [8 :S] [12 :H]]]
                 [[[2 :D] [9 :C] [14 :S] [14 :H] [14 :C]]
                  [[3 :D] [6 :D] [7 :D] [10 :D] [12 :D]]]
                 [[[4 :D] [6 :S] [9 :H] [12 :H] [12 :C]]
                  [[3 :D] [6 :D] [7 :H] [12 :D] [12 :S]]]
                 [[[2 :H] [2 :D] [4 :C] [4 :D] [4 :S]]
                  [[3 :C] [3 :D] [3 :S] [9 :S] [9 :D]]]])

; (defn consecutive? [nums]
(defn consecutive? [nums]
  (let [sorted-nums (sort nums)
        res (reduce (fn [[last-num diffs] next-num]
                      (vector next-num (conj diffs (- next-num last-num))))
                    (vector (dec (first sorted-nums))
                            [])
                    sorted-nums)]
    (apply = 1 (second res))))

(defn card-order-greater [c1 c2]
  (> (first c1) (first c2)))

(defn same-suit? [cards]
  (apply = (map second cards)))

(defn royal-flush? [hand]
  (and (same-suit? hand)
       (= (ffirst hand) 10)
       (consecutive? (map first hand))
       (sort card-order-greater hand)))

; (royal-flush? [[10 :H] [11 :H] [12 :H] [13 :H] [14 :H]])
; (royal-flush? [[9 :H] [10 :H] [11 :H] [12 :H] [13 :H]])

(defn straight-flush? [hand]
  (and (same-suit? hand)
       (consecutive? (map first hand))
       (sort card-order-greater hand)))

; (straight-flush? [[10 :H] [11 :H] [12 :H] [13 :H] [14 :H]])
; (straight-flush? [[9 :H] [10 :H] [11 :H] [12 :H] [13 :H]])
; (straight-flush? [[8 :H] [10 :H] [11 :H] [12 :H] [13 :H]])

(defn are-all-equal-nums? [subhand]
  (apply = (map first subhand)))

(defn add-other-cards [hand subset]
  (concat (apply vector subset) (sort card-order-greater (cs/difference (set hand) (set subset)))))

(defn four-of-a-kind? [hand]
  (let [foak (->> (combo/combinations hand 4)
                  (filter are-all-equal-nums?)
                  (first))]
    (when foak
      (add-other-cards hand foak))))

; (four-of-a-kind? [[10 :H] [11 :H] [12 :H] [13 :H] [14 :H]])
; (four-of-a-kind? [[12 :H] [10 :H] [10 :S] [10 :D] [10 :C]])

(defn split-to-two-and-three [hand]
  (->> (combo/combinations hand 2)
       (map #(list % (vec (cs/difference (set hand) (set %)))))))

; (split-to-two-and-three [[10 :H] [11 :H] [12 :H] [13 :H] [14 :H]])

(defn full-house? [hand]
  (let [fh (->> (split-to-two-and-three hand)
                (filter #(every? are-all-equal-nums? %))
                (first))]
    (when fh
      (apply concat (reverse fh)))))

; (full-house? [[12 :H] [12 :S] [10 :S] [10 :D] [10 :C]])
; (full-house? [[12 :H] [12 :S] [9 :S] [10 :D] [10 :C]])
; (full-house? [[10 :H] [12 :S] [12 :S] [10 :D] [10 :C]])
; (full-house? [[10 :H] [12 :S] [9 :S] [5 :D] [10 :C]])

(defn flush? [hand]
  (and (same-suit? hand)
       (sort card-order-greater hand)))

; (flush? [[8 :H] [11 :H] [6 :H] [13 :H] [14 :H]])
; (flush? [[10 :H] [11 :S] [12 :H] [13 :H] [14 :H]])

(defn straight? [hand]
  (and (consecutive? (map first hand))
       (sort card-order-greater hand)))

(straight? [[11 :H] [12 :H] [13 :H] [10 :H] [14 :H]])
; (straight? [[10 :H] [10 :S] [12 :H] [13 :H] [14 :H]])

(defn three-of-a-kind? [hand]
  (let [toak (->> (split-to-two-and-three hand)
                  (map second)
                  (filter are-all-equal-nums?)
                  (first))]
    (when toak
      (add-other-cards hand toak))))

; (three-of-a-kind? [[10 :H] [10 :S] [12 :D] [10 :C] [14 :H]])
; (three-of-a-kind? [[10 :H] [10 :S] [12 :H] [13 :H] [14 :H]])

(defn two-pairs? [hand]
  (let [splits (->> (split-to-two-and-three hand)
                    (filter #(and (are-all-equal-nums? (first %))
                                  (->> (combo/combinations (second %) 2)
                                       (map are-all-equal-nums?)
                                       (some true?))))
                    (first))
        tp (when splits
             (vector (first splits)
                     (first (filter are-all-equal-nums? (combo/combinations (second splits) 2)))))]
    (when tp
      (->> tp
           (apply concat)
           (sort card-order-greater)
           (add-other-cards hand)))))

; (two-pairs? [[10 :H] [10 :S] [12 :D] [9 :C] [14 :H]])
; (two-pairs? [[10 :H] [10 :S] [12 :H] [14 :H] [14 :C]])

(defn one-pair? [hand]
  (let [op (->> (combo/combinations hand 2)
                (filter are-all-equal-nums?)
                (first))]
    (when op
      (add-other-cards hand op))))

; (one-pair? [[10 :H] [10 :S] [12 :D] [9 :C] [14 :H]])
; (one-pair? [[10 :H] [11 :S] [12 :H] [4 :H] [14 :C]])

;; Taken from internet
(defmacro cond-let
  "An implementation of cond-let that is as similar as possible to if-let. Takes multiple
  test-binding/then-form pairs and evalutes the form if the binding is true. Also supports
  :else in the place of test-binding and always evaluates the form in that case.
 
  Example:
   (cond-let [b (bar 1 2 3)] (println :bar b)
             [f (foo 3 4 5)] (println :foo f)
             [b (baz 6 7 8)] (println :baz b)
             :else           (println :no-luck))"
  [test-binding then-form & more]
  (let [test-binding (if (= :else test-binding) `[t# true] test-binding)
        else-form    (when (seq more) `(cond-let ~@more))]
    `(if-let ~test-binding
       ~then-form
       ~else-form)))

(defn hand-type [hand]
  (cond-let [rf (royal-flush? hand)] [9 rf]
            [sf (straight-flush? hand)] [8 sf]
            [foak (four-of-a-kind? hand)] [7 foak]
            [fh (full-house? hand)] [6 fh]
            [f (flush? hand)] [5 f]
            [s (straight? hand)] [4 s]
            [toak (three-of-a-kind? hand)] [3 toak]
            [tp (two-pairs? hand)] [2 tp]
            [op (one-pair? hand)] [1 op]
            :else [0 (sort card-order-greater hand)]))

(defn is-player1-winner-same-type? [det1 det2]
  (= 1
     (compare (apply vector (map first det1))
              (apply vector (map first det2)))))

(defn is-player1-winner? [hand1 hand2]
  (let [[res1 det1] (hand-type hand1)
        [res2 det2] (hand-type hand2)
        p1-winner (cond (= res1 res2) (is-player1-winner-same-type? det1 det2)
                        (> res1 res2) true
                        :else false)]
    (do
      ; (println "Winner = " (if p1-winner "p1" "p2") "... Rank1 = " res1 ", Rank2 = " res2 ", det1 = " det1 ", det2 = " det2)
      p1-winner)))

(defn problem-54 []
  (->> (str/split (slurp "p054_poker.txt") #"\s")
       (map (fn [s]
              (vector (nth s 0) (nth s 1))))
       (map (fn [[s-num s-suite]]
              (vector (case s-num
                        \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9 \T 10 \J 11 \Q 12 \K 13 \A 14)
                      (keyword (str s-suite)))))
       (partition 10)
       (map #(partition 5 %))
       (filter (fn [[hand1 hand2]]
                 (is-player1-winner? hand1 hand2)))
       count))

; https://projecteuler.net/problem=55

(defn reverse-number [num]
  (->> (str num)
       (reverse)
       (apply str)
       (bigint)))

(defn is-lychrel?
  ([num iteration]
   (if (> iteration 50)
     true
     (let [next-num (+ num (reverse-number num))]
       (if (is-palindrome-number? next-num)
         false
         (recur next-num (inc iteration))))))
  ([num]
   (is-lychrel? (bigint num) 0)))

(defn problem-55 []
  (->> (range 1 10000)
       (filter is-lychrel?)
       (count)))

; https://projecteuler.net/problem=56

(defn b-pow [a b]
  (nth (iterate #(* a %) (bigint a)) (dec b)))

(defn problem-56 []
  (->> (for [a (range 1 100)
             b (range 1 100)]
         (b-pow a b))
       (map sum-digits)
       (sort >)
       (first)))


; https://projecteuler.net/problem=57

(def sqrt-denom
  (memoize
   (fn [iterations]
     (/ 1 (if (= iterations 0)
            2
            (+ 2 (sqrt-denom (dec iterations))))))))

(defn problem-57 []
  (->> (range 1000)
       (map (comp inc sqrt-denom))
       (map #(vector (numerator %) (denominator %)))
       (map #(map str %))
       (filter #(> (.length (first %)) (.length (second %))))
       (count)))

; https://projecteuler.net/problem=58

(defn problem-58 []
  (let [ltt (->> (diagonal-vals)
                 (take 15000)
                 (map #(count (filter prime? %)))
                 (reductions + 0)
                 (drop 1)
                 (map-indexed #(/ %2 (inc (* 4 (inc %1)))))
                 (filter #(< % 1/10))
                 (first)
                 (denominator))]
    (/ (inc ltt) 2)))

; https://projecteuler.net/problem=59

(def common-words #{"the" "of" "and" "to" "a" "in" "for" "is" "on" "that" "by" "this" "with" "i" "you" "it" "not" "or" "be" "are" "from" "at" "as" "your" "all" "have" "new" "more" "an" "was" "we" "will" "home" "can" "us" "about" "if" "page" "my" "has" "search" "free" "but" "our" "one" "other" "do" "no" "information" "time" "they" "site" "he" "up" "may" "what" "which" "their" "news" "out" "use" "any" "there" "see" "only" "so" "his" "when" "contact" "here" "business" "who" "web" "also" "now" "help" "get" "pm" "view" "online" "c" "e" "first" "am" "been" "would" "how" "were" "me" "s" "services" "some" "these" "click" "its" "like" "service" "x" "than" "find"})

(defn problem-59 []
  (let [lcr (range 97 123)
        cipher (map #(Integer/parseInt (str/trim %))
                    (str/split (slurp "p059_cipher.txt") #","))]
    (->>
     (for [i lcr
           j lcr
           k lcr]
       (let [res (->> [i j k]
                      (repeat)
                      (flatten)
                      (map bit-xor cipher)
                      (map char)
                      (apply str))
             wc (->> (str/split res #"\s")
                     (filter common-words)
                     (count))]
         (when (> wc 20)
           res)))
     (filter #(not= nil %))
     (first)
     (map int)
     (reduce +))))

; https://projecteuler.net/problem=60

; https://stackoverflow.com/a/4140746/466694
(defn- concat-numbers [a b]
  "b should be greater than zero"
  (+ (int (* a (Math/pow 10 (Math/floor (inc (Math/log10 b))))))
     b))

(def are-c-primes?
  (memoize (fn [a b]
             (if (>= a b)
               (and (prime? (concat-numbers a b))
                    (prime? (concat-numbers b a)))
               (are-c-primes? b a)))))

(defn- are-concat-primes-5? [[a b c d e]]
  (every? #(apply are-c-primes? %) [[a b] [a c] [a d] [b c] [b d] [c d] [a e] [b e] [c e] [d e]]))

(defn- are-concat-primes-4? [[a b c d]]
  (every? #(apply are-c-primes? %) [[a b] [a c] [a d] [b c] [b d] [c d]]))

;; Takes a long time to come up with the solution. But it does.
(defn problem-60 []
  (let [ps (take-while #(< % 10000) (gen-primes))
        p2s (->> (combo/combinations ps 2)
                 (filter #(apply are-c-primes? %)))
        p4s (->> (for [i p2s j p2s]
                   (concat i j))
                 (filter are-concat-primes-4?))]
    (->> (for [i ps j p4s]
           (conj j i))
         (filter are-concat-primes-5?)
         (first)
         (reduce +))))

; https://projecteuler.net/problem=61
(defn- gen-polygonal-numbers
  ([n polygon-fn]
   (lazy-seq (cons (polygon-fn n)
                   (gen-polygonal-numbers (inc n) polygon-fn))))
  ([polygon-fn]
   (gen-polygonal-numbers 1 polygon-fn)))

(def triangle-numbers
  (gen-polygonal-numbers (fn [n] (/ (* n (inc n)) 2))))

(def square-numbers
  (gen-polygonal-numbers (fn [n] (* n n))))

(def pentagonal-numbers
  (gen-polygonal-numbers (fn [n] (/ (* n (dec (* 3 n))) 2))))

(def hexagonal-numbers
  (gen-polygonal-numbers (fn [n] (* n (dec (* 2 n))))))

(def heptagonal-numbers
  (gen-polygonal-numbers (fn [n] (/ (* n (- (* 5 n) 3)) 2))))

(def octagonal-numbers
  (gen-polygonal-numbers (fn [n] (* n (- (* 3 n) 2)))))

(defn- get-4-digit-polygonal-numbers [series]
  (->> series
       (drop-while #(< % 1000))
       (take-while #(< % 10000))))

(defn- shares-last-and-first-two-digits? [a b]
  (let [sa (str a)
        sb (str b)]
    (= (subs sa 2) (subs sb 0 2))))

; (shares-first-and-last-two-digits? 1235 3456)

(defn- get-chain [possible-nums last-num current-chain]
  (if (or (= (count current-chain) 6)
          (= (count possible-nums) 0))
    current-chain
    (let [chainable-nums (filter #(or (not last-num) (shares-last-and-first-two-digits? last-num %))
                                 (disj possible-nums last-num))]
      (for [i chainable-nums]
        (get-chain (cs/difference possible-nums (set [last-num i])) i (conj current-chain i))))))

(defn problem-61 []
  (let [polys
        (->> [triangle-numbers square-numbers pentagonal-numbers hexagonal-numbers heptagonal-numbers octagonal-numbers]
             (map get-4-digit-polygonal-numbers)
             (map #(set %)))
        all-polys
        (->> polys
             (apply concat)
             (set))
        comb1 (combo/combinations all-polys 2)
        comb2 (map reverse comb1)
        combs (concat comb1 comb2)
        fc1 (filter #(apply shares-last-and-first-two-digits? %) combs)
        fc2 (filter #(apply shares-last-and-first-two-digits? %) combs)
        cn (cs/intersection (set (map first fc1)) (set (map first fc2)))
        chains (filter vector? (tree-seq (complement vector?) seq (get-chain cn nil [])))
        circular-chains (filter #(shares-last-and-first-two-digits? (last %) (first %)) chains)
        p6s (permutations (range 6))
        apply-permutation (fn [chain permutation] (every? some? (map #((nth polys %1) %2) permutation chain)))
        res (map (fn [permutation]
                   (filter (fn [chain]
                             (apply-permutation chain permutation))
                           circular-chains))
                 p6s)
        res (filter (complement empty?) res)]
    (reduce + (ffirst res))))

; https://projecteuler.net/problem=62

(defn problem-62 []
  (->> (range 10000)
       (map #(* % % %))
       (map #(hash-map (sort (get-digits %)) [%]))
       (apply merge-with into)
       (vals)
       (filter #(= 5 (count %)))
       (map first)
       (sort)
       (first)))

; https://projecteuler.net/problem=63

(defn problem-63 []
  (->> (range 1 10)
       (map (fn [b] (map (fn [e] [e (.length (str (expt (biginteger b) e)))]) (drop 1 (range)))))
       (map (fn [l] (take-while #(= (first %) (second %)) l)))
       (map (fn [l] (map first l)))
       (flatten)
       (count)))

; https://projecteuler.net/problem=64

(defn- next-continued-fraction [[a n x y]]
  (let [sq (int (Math/sqrt n))
        x2 (quot (- n (* y y)) x)
        tu (mod (+ y sq) x2)
        y2 (- sq tu)
        a2 (quot (+ y sq) x2)]
    [a2 n x2 y2]))

(defn- continued-fractions [n]
  (iterate next-continued-fraction [0 n 1 (int (Math/sqrt n))]))

(defn- first-duplicates
  ([[x & rest_xs] index index_map]
   (let [found_index (index_map x)]
     (if (nil? found_index)
       (first-duplicates rest_xs (inc index) (assoc index_map x index))
       [found_index index])))
  ([xs]
   (first-duplicates xs 0 {})))

(defn- find-period-of-continued-fraction [n]
  (let [cfs (continued-fractions n)]
    (apply - (reverse (first-duplicates (continued-fractions n))))))

(defn- is-perfect-square? [n]
  (let [sq (int (Math/sqrt n))]
    (= n (* sq sq))))

(defn problem-64 []
  (->> (range 2 10001)
       (filter (complement is-perfect-square?))
       (map find-period-of-continued-fraction)
       (filter odd?)
       (count)))

; https://projecteuler.net/problem=65

(defn- continued-fraction-term [cf]
  (->> (reverse cf)
       (reduce (fn [prev-sum t] (+ t (/ 1 prev-sum))))))

(defn- e-fractions []
  (concat [2]
          (flatten (iterate (fn [[x y z]] [x (+ y 2) z]) [1 2 1]))))

(defn problem-65 []
  (->> (take 100 (e-fractions))
       (continued-fraction-term)
       (numerator)
       (sum-digits)))

; https://projecteuler.net/problem=66
; http://mathworld.wolfram.com/PellEquation.html
(defn- continued-fractions-simplified [n]
  (let [cfs (continued-fractions n)]
    (concat [(last (first cfs))]
            (map first (drop 1 cfs)))))

(defn solve-pells-equation [D]
  (let [cfs (continued-fractions-simplified D)
        r (find-period-of-continued-fraction D)
        num-terms (if (odd? r) (* 2 r) r)
        last-term (continued-fraction-term (take num-terms cfs))]
    (if (ratio? last-term)
      [(numerator last-term) (denominator last-term)]
      [last-term 1])))

(defn problem-66 []
  (->> (range 1001)
       (filter (complement is-perfect-square?))
       (map #(concat [%] (solve-pells-equation %)))
       (sort #(> (second %1) (second %2)))
       (ffirst)))

; https://projecteuler.net/problem=67

; A node can be reached via maximum of two paths from previous level.
; Create max totals for each row. This can be used in turn to calculate next row
(defn- solve-triangle-max [previous-row current-row]
  (->> (range (count current-row))
       (map (fn [i] (cond (= i 0) (nth previous-row 0)
                          (= i (count previous-row)) (nth previous-row (dec i))
                          :else (max (nth previous-row (dec i)) (nth previous-row i)))))
       (map + current-row)))

(defn problem-67 []
  (let [els (->> (str/split (slurp "p067_triangle.txt") #"\s")
                 (map #(Integer/parseInt %))
                 (apply vector))
        triangle (map #(subvec els (first %) (second %))
                      (first (reduce (fn [[coll last] i]
                                       [(conj coll [last (+ last i)]) (+ last i)])
                                     [[] 0]
                                     (range 1 101))))]
    (first (sort > (reduce solve-triangle-max triangle)))))


; https://projecteuler.net/problem=68

(defn problem-68 []
  (->> (combo/permutations (range 1 11))
       (filter (fn [[a b c d e f g h i j]] (and (= (+ a b c) (+ d c e) (+ f e g) (+ h g i) (+ j i b))
                                                (< a d) (< a f) (< a h) (< a j))))
       (map (fn [[a b c d e f g h i j]] (->> [a b c d c e f e g h g i j i b]
                                             (map str)
                                             (apply str))))
       (filter #(= (.length %) 16))
       (map biginteger)
       (sort >)
       (first)))

; https://projecteuler.net/problem=69
; Totient value calculated (See totient-vals) using formula here: https://en.wikipedia.org/wiki/Euler%27s_totient_function#Euler's_product_formula


(defn- get-factors-map [max]
  (let [dict (apply hash-map (interleave (range 2 max) (take (- max 2) (repeat []))))
        primes (take-while #(< % max) (gen-primes))
        factors-map (reduce (fn [d i]
                              (reduce (fn [dd ii]
                                        (update dd ii conj i))
                                      d
                                      (range i max i)))
                            dict
                            primes)]
    factors-map))

(defn- get-totient-vals [max]
  (let [factors-map (get-factors-map max)
        totient-vals (into {} (for [[k v] factors-map]
                                [k (reduce (fn [p i] (* p (- 1 (/ 1 i))))
                                           k v)]))]
    totient-vals))

(defn problem-69 []
  (let [max 1000001
        totient-vals (get-totient-vals max)
        phi-vals (into (sorted-map-by >) (for [[k v] totient-vals]
                                           [(/ k v) k]))]
    (second (first phi-vals))))

; https://projecteuler.net/problem=70
(defn problem-70 []
  (let [max 10000000
        totient-vals (get-totient-vals max)
        permutations (filter (fn [[k v]] (are-permutations-of-each-other? k v)) totient-vals)
        phi-vals (map (fn [[k v]] [(/ k v) k]) permutations)
        sorted-phis (sort-by first < phi-vals)]
    (second (first sorted-phis))))

; https://projecteuler.net/problem=70
;; https://en.wikipedia.org/wiki/Farey_sequence#Farey_neighbours
;; See mediant :
;; If a/b and c/d are two adjacent terms in a Farey seq F(n), the next num between them is (a+c)/(b+d) first in F(b+d)

(defn- next-mediant [[a b c d]]
  [(+ a c) (+ b d) c d])

(defn problem-71 []
  (->> (iterate next-mediant [2 5 3 7])
       (take-while (fn [[_ b _ _]] (< b 1000000)))
       (last)
       (first)))

;; https://projecteuler.net/problem=72
;; 21 is sum of totients (> 1 & <= 8)
(defn- problem-72 []
  (->> (get-totient-vals 1000001)
       (map second)
       (reduce +)))
