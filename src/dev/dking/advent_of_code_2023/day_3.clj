(ns dev.dking.advent-of-code-2023.day-3
  (:refer-clojure :exclude [symbol?])
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]))

(defn- re-groups-seq
  ([re s] (re-groups-seq (re-matcher re s)))
  ([matcher]
   (when (.find matcher)
     (cons {:group (.group matcher) :start (.start matcher)}
           (lazy-seq (re-groups-seq matcher))))))

(defn- symbol?
  [char]
  (nil? (s/index-of "0123456789." char)))

(defn- force-to-interval
  [num minimum maximum]
  (max minimum (min maximum num)))

(defn- pad-input-lines
  [input-lines]
  (concat [""] input-lines [""]))

(defn- get-adjacent-characters
  [previous-line line next-line {:keys [group start]}]
  (let [end
        (+ start (dec (count group)))

        previous-line-length
        (count previous-line)

        previous-line-adjacent-characters
        (subs previous-line
              (force-to-interval (dec start) 0 previous-line-length)
              (force-to-interval (+ 2 end) 0 previous-line-length))

        next-line-length
        (count next-line)

        next-line-adjacent-characters
        (subs next-line
              (force-to-interval (dec start) 0 next-line-length)
              (force-to-interval (+ 2 end) 0 next-line-length))]
    (filter (complement nil?)
            (concat [(get line (dec start)) (get line (inc end))]
                    previous-line-adjacent-characters
                    next-line-adjacent-characters))))

(defn- part-number?
  [previous-line line next-line {:keys [group start]}]
  (some symbol? (get-adjacent-characters previous-line
                                         line
                                         next-line
                                         {:group group :start start})))

(defn- get-part-numbers
  [previous-line line next-line]
  (->> (re-groups-seq #"\d+" line)
       (filter #(part-number? previous-line line next-line %))
       (map :group)
       (map #(Integer/parseInt %))))

(defn part-1
  [input]
  (->> input
       s/split-lines
       pad-input-lines
       (partition 3 1)
       (mapcat #(apply get-part-numbers %))
       (reduce +)))

(comment
  ;; Solutions
  (def input (slurp (io/resource "day_3.txt")))
  (part-1 input) ; 525911
  ;; Scratchworx
  (def test-input
    "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

  (re-seq #"\d+" "467..114..")
  (def matcher (re-matcher #"\d+" "467..114.."))
  (.find matcher)
  (.start matcher)
  (.group matcher)

  (re-groups-seq #"\d+" "467..114..")
  (pad-input-lines (s/split-lines test-input))

  (part-1 test-input)

  (get-adjacent-characters ".cd*e....."
                           ".a35b.633."
                           ".fghi.#..."
                           {:start 2
                            :group "35"})

  (subs "" 0 0))

(deftest force-to-interval-test
  (testing "returns number unchanged if it is in the interval"
    (is (= 3 (force-to-interval 3 1 4))))
  (testing "returns the minimum if the number is below the minimum"
    (is (= 1 (force-to-interval 0 1 4))))
  (testing "returns the maximum if the number is above the maximum"
    (is (= 4 (force-to-interval 5 1 4)))))

(deftest get-adjacent-characters-test
  (testing "returns all adjacent characters"
    (is (= '(\a \b \c \d \* \e \f \g \h \i)
           (get-adjacent-characters ".cd*e....."
                                    ".a35b.633."
                                    ".fghi.#..."
                                    {:start 2
                                     :group "35"}))))
  (testing "returns all adjacent characters for a number at the beginning of
            a line"
    (is (= '(\* \. \. \. \. \. \. \. \.)
           (get-adjacent-characters "......#..."
                                    "617*......"
                                    ".....+.58."
                                    {:start 0
                                     :group "617"}))))
  (testing "returns all adjacent characters for a number at the end of a 
            line"
    (is (= '(\* \# \. \. \. \. \5 \8 \.)
           (get-adjacent-characters "......#..."
                                    "......*617"
                                    ".....+.58."
                                    {:start 7
                                     :group "617"}))))
  (testing "returns all adjacent characters when the previous line is empty"
    (is (= '(\* \. \5 \8 \.)
           (get-adjacent-characters ""
                                    "......*617"
                                    ".....+.58."
                                    {:start 7
                                     :group "617"}))))
  (testing "returns all adjacent characters when the next line is empty"
    (is (= '(\* \. \5 \8 \.)
           (get-adjacent-characters ".....+.58."
                                    "......*617"
                                    ""
                                    {:start 7
                                     :group "617"})))))

(deftest part-number-test
  (testing "Returns true if the number is adjacent to a symbol"
    (is (true? (part-number? "...*......"
                             "..35..633."
                             "......#..."
                             {:start 2
                              :group "35"}))))
  (testing "Returns a falsey value if the number is not adjacent to a symbol"
    (is (not (part-number? ".........."
                           "..35..633."
                           "......#..."
                           {:start 2
                            :group "35"})))))

(deftest get-part-numbers-test
  (testing "Returns all part numbers in the line"
    (is (= '(35 633)
           (get-part-numbers "...*......"
                             "..35..633."
                             "......#...")))
    (is (= '(35 633)
           (get-part-numbers ".........."
                             "..35@.633."
                             "......#...")))
    (is (= '(633)
           (get-part-numbers ".........."
                             "..35..633."
                             "......#...")))))