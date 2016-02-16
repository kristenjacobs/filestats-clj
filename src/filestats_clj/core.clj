(ns filestats-clj.core
  (:gen-class))

(defn- num-words-in-line
  [line]
  (if (= line "")
    0
    (count (clojure.string/split line #"\s+"))))

(defn- get-letters
  [line]
  (->> (char-array line)
      (filter #(Character/isLetter %)) 
      (map clojure.string/lower-case))) 

(defprotocol statistic 
  (next-line [stat line])
  (display-result [stat]))

(defrecord line-count [num-lines]
  statistic
    (next-line 
      [stat line]
      (assoc stat :num-lines 
             (inc (:num-lines stat))))
    (display-result 
      [stat]
      (println "line-count: " (:num-lines stat))))

(defrecord word-count [num-words]
  statistic
    (next-line 
      [stat line]
      (assoc stat :num-words 
             (+ (:num-words stat) 
                (num-words-in-line line))))
    (display-result 
      [stat]
      (println "word-count: " (:num-words stat))))

(defrecord avg-word-length [num-letters num-words]
  statistic
    (next-line 
      [stat line]
      (-> stat
        (assoc :num-letters 
               (+ (:num-letters stat) 
                  (count (get-letters line))))
        (assoc :num-words 
               (+ (:num-words stat) 
                  (count (clojure.string/split line #"\s+"))))))
    (display-result 
      [stat]
      (if (= num-words 0)
        (println "avg-word-length: " 0)
        (println "avg-word-length: " (/ (:num-letters stat) 
                                        (:num-words stat))))))

(defrecord most-freq-letter [letter-freq-map]
  statistic
    (next-line 
      [stat line]
      (assoc stat 
             :letter-freq-map 
             (merge-with + 
                         (frequencies (get-letters line)) 
                         (:letter-freq-map stat))))
    (display-result 
      [stat]
      (println "most-freq-letter: " 
               (first (first (sort-by val > (:letter-freq-map stat)))))))

(defn process-line
  [stats line]
  (doall (map #(next-line %1 line) stats)))

(defn -main [& args]
  (with-open [rdr (clojure.java.io/reader (first args))]
    (let [stats [(->line-count 0)
                 (->word-count 0)
                 (->avg-word-length 0 0)
                 (->most-freq-letter {})]]
      (doseq [stat (reduce process-line stats (line-seq rdr))]
        (display-result stat)))))

