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
                    (into [] (filter is-digit) (seq line))]
                [(first digit-characters) (last digit-characters)])))
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

(def digits-re-pattern (->> digit-names
                            (map (fn [[k v]] (str k "|" v)))
                            (s/join "|")))
(def digits-forward-regexp (re-pattern digits-re-pattern))
(def digits-reverse-regexp (re-pattern (apply str (reverse digits-re-pattern))))


(defn get-first-digit
  [line]
  (let [match (re-find digits-forward-regexp line)]
    (if (= (count match) 1)
      (first match)
      (digit-names match))))

(defn get-last-digit
  [line]
  (let [match (s/reverse (re-find digits-reverse-regexp (s/reverse line)))]
    (if (= (count match) 1)
      (first match)
      (digit-names match))))

(defn part-2
  [input]
  (->> input
       (s/split-lines)
       (map (fn [line] [(get-first-digit line) (get-last-digit line)]))
       (map #(apply str %))
       (map parse-int)
       (reduce +)))

(comment
  (def input (slurp (io/resource "day_1.txt")))
  ;; Solutions
  (part-1 input)
  (part-2 input)

  ;; Currently gives 54933, which is too low.
  ;; I downloaded someone else's solution and ran it, and it gave
  ;; 54980.
  ;; Something's not right with my solution. I think I'll start over.

  (get-first-digit "threight9")
  (get-last-digit "threight9")
  (get-last-digit "onee")


  (->> input
       (s/split-lines)
       (map (fn [line]
              [(get-first-digit line) (get-last-digit line)]))
       (map #(apply str %))
       (map parse-int)
       (reduce +)))

