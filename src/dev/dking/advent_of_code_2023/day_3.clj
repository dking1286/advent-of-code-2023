(ns dev.dking.advent-of-code-2023.day-3
  (:refer-clojure :exclude [symbol?])
  (:require [clojure.string :as s]
            [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]))

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
  [[previous-line-idx previous-line]
   [line-idx line]
   [next-line-idx next-line]
   {:keys [group start]}]
  (let [end
        (+ start (dec (count group)))

        previous-line-length
        (count previous-line)

        previous-line-search-start
        (force-to-interval (dec start) 0 previous-line-length)

        previous-line-search-end
        (force-to-interval (+ 2 end) 0 previous-line-length)

        previous-line-adjacent-characters
        (->> (subs previous-line
                   previous-line-search-start
                   previous-line-search-end)
             (map-indexed (fn [idx char]
                            {:char char
                             :row previous-line-idx
                             :col (+ idx previous-line-search-start)})))


        next-line-length
        (count next-line)

        next-line-search-start
        (force-to-interval (dec start) 0 next-line-length)

        next-line-search-end
        (force-to-interval (+ 2 end) 0 next-line-length)

        next-line-adjacent-characters
        (->> (subs next-line
                   next-line-search-start
                   next-line-search-end)
             (map-indexed (fn [idx char]
                            {:char char
                             :row next-line-idx
                             :col (+ idx next-line-search-start)})))]
    (->> (concat [{:char (get line (dec start))
                   :row line-idx
                   :col (dec start)}
                  {:char (get line (inc end))
                   :row line-idx
                   :col (inc end)}]
                 previous-line-adjacent-characters
                 next-line-adjacent-characters)
         (filter #(not (nil? (:char %)))))))


(defn- part-number?
  [[previous-line-idx previous-line]
   [line-idx line]
   [next-line-idx next-line]
   {:keys [group start]}]
  (->> (get-adjacent-characters [previous-line-idx previous-line]
                                [line-idx line]
                                [next-line-idx next-line]
                                {:group group :start start})
       (map :char)
       (some symbol?)))

(defn- get-part-numbers
  [[previous-line-idx previous-line]
   [line-idx line]
   [next-line-idx next-line]]
  (->> (re-groups-seq #"\d+" line)
       (filter #(part-number? [previous-line-idx previous-line]
                              [line-idx line]
                              [next-line-idx next-line]
                              %))
       (map :group)
       (map #(Integer/parseInt %))))

(defn part-1
  [input]
  (->> input
       s/split-lines
       pad-input-lines
       (map-indexed vector)
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

  (subs "" 0 0)

  (->> '(\* \. \5 \8 \.)
       (map (fn [char] {:char char}))))

(deftest force-to-interval-test
  (testing "returns number unchanged if it is in the interval"
    (is (= 3 (force-to-interval 3 1 4))))
  (testing "returns the minimum if the number is below the minimum"
    (is (= 1 (force-to-interval 0 1 4))))
  (testing "returns the maximum if the number is above the maximum"
    (is (= 4 (force-to-interval 5 1 4)))))

(deftest get-adjacent-characters-test
  (testing "returns all adjacent characters"
    (is (= '({:char \a :row 2 :col 1}
             {:char \b :row 2 :col 4}
             {:char \c :row 1 :col 1}
             {:char \d :row 1 :col 2}
             {:char \* :row 1 :col 3}
             {:char \e :row 1 :col 4}
             {:char \f :row 3 :col 1}
             {:char \g :row 3 :col 2}
             {:char \h :row 3 :col 3}
             {:char \i :row 3 :col 4})
           (get-adjacent-characters [1 ".cd*e....."]
                                    [2 ".a35b.633."]
                                    [3 ".fghi.#..."]
                                    {:start 2
                                     :group "35"}))))
  (testing "returns all adjacent characters for a number at the beginning of
            a line"
    (is (= '({:char \* :row 2 :col 3}
             {:char \. :row 1 :col 0}
             {:char \. :row 1 :col 1}
             {:char \. :row 1 :col 2}
             {:char \. :row 1 :col 3}
             {:char \. :row 3 :col 0}
             {:char \. :row 3 :col 1}
             {:char \. :row 3 :col 2}
             {:char \. :row 3 :col 3})
           (get-adjacent-characters [1 "......#..."]
                                    [2 "617*......"]
                                    [3 ".....+.58."]
                                    {:start 0
                                     :group "617"}))))
  (testing "returns all adjacent characters for a number at the end of a 
            line"
    (is (= '({:char \* :row 2 :col 6}
             {:char \# :row 1 :col 6}
             {:char \. :row 1 :col 7}
             {:char \. :row 1 :col 8}
             {:char \. :row 1 :col 9}
             {:char \. :row 3 :col 6}
             {:char \5 :row 3 :col 7}
             {:char \8 :row 3 :col 8}
             {:char \. :row 3 :col 9})
           (get-adjacent-characters [1 "......#..."]
                                    [2 "......*617"]
                                    [3 ".....+.58."]
                                    {:start 7
                                     :group "617"}))))
  (testing "returns all adjacent characters when the previous line is empty"
    (is (= '({:char \* :row 2 :col 6}
             {:char \. :row 3 :col 6}
             {:char \5 :row 3 :col 7}
             {:char \8 :row 3 :col 8}
             {:char \. :row 3 :col 9})
           (get-adjacent-characters [1 ""]
                                    [2 "......*617"]
                                    [3 ".....+.58."]
                                    {:start 7
                                     :group "617"}))))
  (testing "returns all adjacent characters when the next line is empty"
    (is (= '({:char \* :row 2 :col 6}
             {:char \. :row 1 :col 6}
             {:char \5 :row 1 :col 7}
             {:char \8 :row 1 :col 8}
             {:char \. :row 1 :col 9})
           (get-adjacent-characters [1 ".....+.58."]
                                    [2 "......*617"]
                                    [3 ""]
                                    {:start 7
                                     :group "617"})))))

(deftest part-number-test
  (testing "Returns true if the number is adjacent to a symbol"
    (is (true? (part-number? [1 "...*......"]
                             [2 "..35..633."]
                             [3 "......#..."]
                             {:start 2
                              :group "35"}))))
  (testing "Returns a falsey value if the number is not adjacent to a symbol"
    (is (not (part-number? [1 ".........."]
                           [2 "..35..633."]
                           [3 "......#..."]
                           {:start 2
                            :group "35"})))))

(deftest get-part-numbers-test
  (testing "Returns all part numbers in the line"
    (is (= '(35 633)
           (get-part-numbers [1 "...*......"]
                             [2 "..35..633."]
                             [3 "......#..."])))
    (is (= '(35 633)
           (get-part-numbers [1 ".........."]
                             [2 "..35@.633."]
                             [3 "......#..."])))
    (is (= '(633)
           (get-part-numbers [1 ".........."]
                             [2 "..35..633."]
                             [3 "......#..."])))))

(deftest part-1-test
  (testing "gives correct answer on the test input"
    (is (= 4361 (part-1 test-input)))))