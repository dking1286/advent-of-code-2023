(ns dev.dking.advent-of-code-2023.day-2
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-cubes
  [cubes-str]
  (let [[_ num-str color-str] (re-find #"(\d+)(\w+)" cubes-str)]
    [(keyword color-str) (Integer/parseInt num-str)]))

(defn parse-round
  [round-str]
  (->> (s/split round-str #"\,")
       (map parse-cubes)
       (into {})))

(defn round-possible?
  [limits round]
  (every? (fn [[color num]] (<= num (limits color))) round))

(defn parse-game
  [game-str]
  (let [clean-game-str (s/replace game-str #"\s" "")
        [game-id-str rounds-str] (s/split clean-game-str #"\:")
        game-id (Integer/parseInt (second (re-find #"^Game(\d+)$" game-id-str)))
        rounds (->> (s/split rounds-str #";")
                    (map parse-round)

                    (into []))]
    {:game-id game-id
     :rounds rounds}))

(defn game-possible?
  [limits game]
  (every? #(round-possible? limits %) (game :rounds)))

(defn sum-of-possible-game-ids
  [limits input]
  (->> (s/split-lines input)
       (map parse-game)
       (filter #(game-possible? limits %))
       (map :game-id)
       (reduce +)))

(defn part-1
  [input]
  (sum-of-possible-game-ids {:red 12 :green 13 :blue 14} input))

(defn safe-max
  [& args]
  (apply max (map #(or % 0) args)))

(defn fewest-possible-cubes
  [game]
  (reduce #(merge-with safe-max %1 %2) (game :rounds)))

(defn power
  [cubes]
  (apply * (map second cubes)))

(defn part-2
  [input]
  (->> (s/split-lines input)
       (map parse-game)
       (map fewest-possible-cubes)
       (map power)
       (reduce +)))

(comment
  ;; Inputs
  (def example-input
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green")
  (def input (slurp (io/resource "day_2.txt")))

  ;; Solutions
  (part-1 input)
  (part-2 input)

  ;; Scratchwork
  (part-2 example-input)
  (safe-max 0 12)
  (safe-max nil 12)
  (safe-max 12 13 nil)
  (def eg-input-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

  (re-find #"(\d+)(\w+)" "12blue")


  (let [clean-input-line (s/replace eg-input-line #"\s" "")
        [game-id-str rounds-str] (s/split clean-input-line #"\:")
        game-id (Integer/parseInt (second (re-find #"^Game(\d)$"
                                                   game-id-str)))
        rounds (->> (s/split rounds-str #";")
                    (map parse-round)
                    (into []))]
    {:game-id game-id
     :rounds rounds})

  (round-possible? {:red 12 :green 13 :blue 14} {:red 13 :green 2})
  (game-possible? {:red 12 :green 13 :blue 14}
                  {:game-id 1
                   :rounds [{:red 13
                             :green 1}
                            {:blue 2}]})

  (sum-of-possible-game-ids {:red 12 :green 13 :blue 14}
                            example-input)

  (doseq [thing (sum-of-possible-game-ids {:red 12 :green 13 :blue 14}
                                          input)]
    (println thing))

  (fewest-possible-cubes {:game-id 1
                          :rounds [{:red 13
                                    :green 1}
                                   {:red 14
                                    :blue 1}
                                   {:blue 2}]})
  (power {:red 13 :green 2 :blue 0}))
