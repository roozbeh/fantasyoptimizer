(ns ga.core)

(defn rand-in-range
  "generates a random value within the given range"
  [min max]
  (int (+ (* (Math/random) (inc (- max min))) min)))

(defn mutate
  "randomly mutates values in members of the population using the mutator function"
  [population mutator threshold fitness]
  (for [member population]
    (if (< (rand) threshold)
      (let [value (into {}  (map mutator (:value member)))]
        {:value value
         :fitness (fitness value)})
      member)))

(defn rank
  "ranks the population by fitness"
  [population]
  (reverse (sort-by :fitness population)))

(defn- update-vals
  "randomly selects a value from either the first or the second memeber for each position, 
  preferring the first member, as the front of the population is more fit"
  [fitness  {v1 :value} {v2 :value}]
  (let [value (map #(if (> (rand) 0.3) %1 %2) v1 v2)]
    {:value value :fitness (fitness value )}))

(defn mate
  "splits the population in half and mates all the members"
  [population fitness]
  (apply map 
         (partial update-vals fitness)
         (split-at (/ (count population) 2) population)))

(defn evolve-step
  "mutate the population, then promote top members and add mated members to the end"
  [size population mutator threshold fitness]
  (let [mutated (rank (mutate population mutator threshold fitness))
        promote-size (/ size 5)
        keep-size (- (/ size 2) promote-size)
        [xs ys] (split-at keep-size mutated)]
    (concat xs (take promote-size ys) (mate mutated fitness))))

(defn gen-member
  "generates a new member for the population"
  [init-value fitness]
  (let [value (init-value)]
    {:value value :fitness (fitness value)}))

(defn init-population
  "creates a new population"
  [size fitness init-value]
  (rank (take size (repeatedly #(gen-member init-value fitness)))))

(defn calc-best-yet
  [person1 person2]
  (if (> (:fitness person1) (:fitness person2))
    person1
    person2))

(defn evolve2
  "evolves the population until at least one member is fit"
  [size threshold init-value mutator fitness max-iterations]
  (println "evolve2")
  (println (str "size: " size))
  (println (str "threshold: " threshold))
  (loop [iteration 1
         population (init-population size fitness init-value)
         best-yet (first population)]
    (println (str "Running iteration #" iteration ", best: " (:fitness (first population))
                  ", worst: " (:fitness (last population))))
    (if (or (> iteration max-iterations) (zero? (:fitness (first population))))
      best-yet
      (recur (inc iteration)
             (evolve-step size population mutator threshold fitness)
             (calc-best-yet best-yet (first population))))))
