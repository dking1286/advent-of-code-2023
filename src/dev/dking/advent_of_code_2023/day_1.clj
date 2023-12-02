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

(comment
  ;; Solutions
  (part-1 (slurp (io/resource "day_1.txt")))
  (part-1-v2 (slurp (io/resource "day_1.txt")))
  (part-2 (slurp (io/resource "day_1.txt")))

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
  reverse-prefix-tree)
