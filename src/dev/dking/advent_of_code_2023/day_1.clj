(ns dev.dking.advent-of-code-2023.day-1
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

;; Part 1

(defn is-digit
  [char]
  (Character/isDigit char))

(defn parse-int
  [string]
  (Integer/parseInt string))


(defn part-1
  [input]
  (->> input
       s/split-lines
       (map (fn [line]
              (let [digit-characters
                    (into [] (filter #(Character/isDigit %)) (seq line))]
                [(first digit-characters) (last digit-characters)])))
       (map #(apply str %))
       (map #(Integer/parseInt %))
       (reduce +)))

(defn part-1-v2
  [char-seq]
  (->> char-seq
       (partition-by (partial = \newline))
       (filter (partial not= '(\newline)))
       (map #(filter is-digit %))
       (map #(vector (first %1) (last %1)))
       (map #(apply str %))
       (map parse-int)
       (reduce +)))

;; Part 2

(def digit-names
  {"one" \1
   "two" \2
   "three" \3
   "four" \4
   "five" \5
   "six" \6
   "seven" \7
   "eight" \8
   "nine" \9})

(defn create-prefix-tree
  [names-to-values]
  (reduce (fn [tree [name value]]
            (assoc-in tree (vec name) value))
          {}
          names-to-values))

(def forward-prefix-tree
  (create-prefix-tree digit-names))

(def reverse-prefix-tree
  (create-prefix-tree (->> digit-names
                           (map (fn [[k v]] [(reverse k) v]))
                           (into {}))))

(defn convert-digit-names-to-numbers
  [input]
  (let [initial-prefix-tree
        (reduce (fn [tree [digit-name digit-value]]
                  (assoc-in tree (vec digit-name) digit-value)) {} digit-names)]
    (loop [remaining (seq input)
           prefix-tree initial-prefix-tree
           out-buffer []
           out []]
      (if-not (seq remaining)
        (apply str (into out out-buffer))
        (let [next-char (first remaining)
              prefix-tree-element (get prefix-tree next-char)]
          (cond
            ;; The next element in the prefix tree is a map, there are still
            ;; multiple possible digit names that we could match. Add the
            ;; current character into the buffer, and go deeper into the tree.
            (map? prefix-tree-element)
            (recur (rest remaining)
                   prefix-tree-element
                   (conj out-buffer next-char)
                   out)

            ;; The next element in the prefix tree is a char, we have matched
            ;; a full digit name. Add the actual digit character into the output
            ;; and reset the prefix tree and buffer.
            (char? prefix-tree-element)
            (recur (rest remaining)
                   initial-prefix-tree
                   []
                   (conj out prefix-tree-element))

            ;; The curretn character doesn't match any of the keys in the
            ;; prefix tree.
            (nil? prefix-tree-element)
            (if (empty? out-buffer)
              ;; If the buffer is empty, we are not currently in the middle of
              ;; matching any digit names. Just put the current character into
              ;; the output and move on.
              (recur (rest remaining)
                     initial-prefix-tree
                     []
                     (conj out next-char))
              ;; If the buffer is non-empty, we are in the middle of matching a
              ;; digit name, but we've failed to find a full match. Flush the
              ;; buffer into the output, and re-try the same character starting
              ;; from the top of the prefix tree.
              (recur remaining
                     initial-prefix-tree
                     []
                     (into out out-buffer)))))))))

(defn part-2
  [input]
  (part-1 (convert-digit-names-to-numbers input)))

(defn get-earliest-digit
  [char-seq prefix-tree]
  (loop [remaining-chars char-seq
         remaining-tree prefix-tree]
    (when-not (empty? remaining-chars)
      (let [next-char (first remaining-chars)
            next-tree-element (get remaining-tree next-char)]
        (cond
          (is-digit next-char)
          next-char

          (map? next-tree-element)
          (recur (rest remaining-chars) next-tree-element)

          (char? next-tree-element)
          next-tree-element

          (nil? next-tree-element)
          (if (= remaining-tree prefix-tree)
            (recur (rest remaining-chars) prefix-tree)
            (recur remaining-chars prefix-tree)))))))

(def digits-re-pattern (->> digit-names
                            (map (fn [[k v]] (str k "|" v)))
                            (s/join "|")))
(def digits-forward-regexp (re-pattern digits-re-pattern))
(def digits-reverse-regexp (re-pattern (apply str (reverse digits-re-pattern))))



(defn get-first-digit
  [char-seq]
  (get-earliest-digit char-seq forward-prefix-tree))

(defn get-first-digit-v2
  [line]
  (let [match (re-find digits-forward-regexp line)]
    (if (= (count match) 1)
      (first match)
      (digit-names match))))

(defn get-last-digit
  [char-seq]
  (get-earliest-digit (reverse char-seq) reverse-prefix-tree))

(defn get-last-digit-v2
  [line]
  (let [match (s/reverse (re-find digits-reverse-regexp (s/reverse line)))]
    (if (= (count match) 1)
      (first match)
      (digit-names match))))

(defn part-2-v2
  [char-seq]
  (->> char-seq
       (partition-by #(= % \newline))
       (filter #(not= % '(\newline)))
       (map (fn [line-seq]
              [(get-first-digit line-seq) (get-last-digit line-seq)]))
       (map #(apply str %))
       (map parse-int)
       (reduce +)))

(comment
  (def input (slurp (io/resource "day_1.txt")))
  ;; Solutions
  (part-1 (slurp (io/resource "day_1.txt")))
  (part-1-v2 (slurp (io/resource "day_1.txt")))
  (part-2 (slurp (io/resource "day_1.txt")))
  (part-2-v2 (slurp (io/resource "day_1.txt")))
  ;; Currently gives 54933, which is too low.
  ;; I downloaded someone else's solution and ran it, and it gave
  ;; 54980.
  ;; Something's not right with my solution. I think I'll start over.

  ;; Scratchwork 
  (def test-input
    "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet")

  (seq "1abc2")
  (->> (seq "1abc2")
       (filter #(Character/isDigit %)))

  (->> test-input
       s/split-lines
       (map (fn [line]
              (let [digit-characters
                    (into [] (filter #(Character/isDigit %)) (seq line))]
                [(first digit-characters) (last digit-characters)])))
       (map #(apply str %))
       (map #(Integer/parseInt %))
       (reduce +))


  (def test-input-2
    "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

  (s/replace test-input-2 "two" "2")

  (part-1 (reduce (fn [acc [digit-name digit-char]] (s/replace acc digit-name digit-char))
                  test-input-2
                  digit-names))

  (def out-stack [])
  (def digit-counters {"one" 0 "two" 0})
  (def next-char \o)
  (loop [remaining-digit-counters {"one" 0 "two" 0}]
    (if-not (seq remaining-digit-counters)
      [(conj out-stack next-char)])
    (let [[digit-name count] (first remaining-digit-counters)]))

  (loop [remaining (seq test-input-2)
         out-stack []
         digit-counters (into {} (map (fn [[k _]] [k 0])) digit-names)]
    (if-not (seq remaining)
      (apply str out-stack)
      (let [next-char (first remaining)
            [next-digit-counters next-out-stack]
            (loop [remaining-digit-counters digit-counters]
              (let []))])
      (let [next-char (first remaining)
            next-digit-counters (->> digit-counters
                                     (map (fn [[digit count]]
                                            (if (= next-char (get digit count))
                                              [digit (inc count)]
                                              [digit count]))))]
        (recur (rest remaining)
               (conj out-stack (first remaining))
               digit-counters))))

  (vec "hello")

  (def initial-prefix-tree
    (reduce (fn [tree [digit-name digit-value]]
              (assoc-in tree (vec digit-name) digit-value)) {} digit-names))

  (loop [remaining (seq test-input-2)
         prefix-tree initial-prefix-tree
         out-buffer []
         out []]
    (if-not (seq remaining)
      (apply str out-stack)
      (let [next-char (first remaining)
            prefix-tree-element (get prefix-tree next-char)]
        (cond
          (map? prefix-tree-element)
          (recur (rest remaining)
                 prefix-tree-element
                 (conj out-buffer next-char)
                 out)

          (char? prefix-tree-element)
          (recur (rest remaining)
                 initial-prefix-tree
                 []
                 (conj out prefix-tree-element))

          (nil? prefix-tree-element)
          (recur (rest remaining)
                 initial-prefix-tree
                 []
                 (conj (into out out-buffer) next-char))))))

  (part-2 test-input-2)
  (println (convert-digit-names-to-numbers (slurp (io/resource "day_1.txt"))))

  (def char-seq (seq "a1b3c\nd4e5f"))
  (->> char-seq
       (partition-by (partial = \newline))
       (filter (partial not= '(\newline)))
       (map #(filter is-digit %))
       (map #(vector (first %1) (last %1)))
       (map #(apply str %))
       (map parse-int)
       (reduce +))

  forward-prefix-tree
  reverse-prefix-tree

  (def easy "3hello4")
  (def hard "threehellotwonezzzz")
  (let [char-seq
        #_easy
        #_(reverse easy)
        #_hard
        (reverse hard)
        prefix-tree
        #_forward-prefix-tree
        reverse-prefix-tree]
    (loop [remaining-chars char-seq
           remaining-tree prefix-tree]
      (when-not (empty? remaining-chars)
        (let [next-char (first remaining-chars)
              next-tree-element (get remaining-tree next-char)]
          (cond
            (is-digit next-char)
            next-char

            (map? next-tree-element)
            (recur (rest remaining-chars) next-tree-element)

            (char? next-tree-element)
            next-tree-element

            (nil? next-tree-element)
            (if (= remaining-tree prefix-tree)
              (recur (rest remaining-chars) prefix-tree)
              (recur remaining-chars prefix-tree)))))))

  (get-first-digit hard)
  (get-last-digit hard)
  (part-2-v2 test-input-2)
  (get-first-digit "thretwozzzzz")
  (get-last-digit "zzzzzztwohree")
  (get-first-digit "threzzztwo")
  (get-last-digit "4gc6cskjfptjxbpone")
  [(get-first-digit "2z") (get-last-digit "2z")]
  (get-first-digit "seight3qvmq2f1kkfone")  ;; WROOOOOOONG
  (get-first-digit "fone9")

  (def char-seq (slurp (io/resource "day_1.txt")))
  (def result
    (->> char-seq
         (partition-by #(= % \newline))
         (filter #(not= % '(\newline)))
         (map (fn [line-seq]
                [(get-first-digit line-seq) (get-last-digit line-seq)]))
         (map #(apply str %))
         (map parse-int)
         (reduce + 0)))
  (doseq [thing result]
    (println thing))

  (re-find #"[a-z]" (slurp (io/resource "day_1.txt")))

  (def foo (->> char-seq
                (partition-by #(= % \newline))
                (filter #(not= % '(\newline)))
                (map (fn [el]
                       {:seq el
                        :string (apply str el)
                        :first-digit (get-first-digit el)
                        :last-digit (get-last-digit el)}))))
  (doseq [el foo]
    (println el))

  (def outside-calibrations
    (->> (slurp (io/resource "day_1_calibrations.txt"))
         (s/split-lines)
         (map #(Integer/parseInt %))))

  (def my-calibrations
    (->> (slurp (io/resource "day_1.txt"))
         (s/split-lines)
         (map (fn [line]
                [(get-first-digit line) (get-last-digit line)]))
         (map #(apply str %))
         (map #(Integer/parseInt %))))

  (def zipped-calibrations
    (map vector my-calibrations outside-calibrations))

  (->> zipped-calibrations
       (map-indexed (fn [i [mine theirs]] [i mine theirs]))
       (filter (fn [[i mine theirs]]
                 (not= mine theirs))))

  (re-find digits-forward-regexp "threight9")
  (s/reverse (re-find digits-reverse-regexp (s/reverse "threight")))

  (->> input
       (s/split-lines)
       (map (fn [line]
              [(re-find digits-forward-regexp line)
               (s/reverse (re-find digits-reverse-regexp (s/reverse line)))]))
       (map (fn [[first last]]))))

