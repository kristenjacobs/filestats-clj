(ns filestats-clj.core-test
  (:require [clojure.test :refer :all]
            [filestats-clj.core :refer :all]))

(deftest line-count-stat-test
  (testing "Given a line, ensure we increment the line count."
    (is (= (:num-lines (next-line (->line-count 0) "")) 1))
    (is (= (:num-lines (next-line (->line-count 1) "")) 2))))

(deftest word-count-stat-test
  (testing "Given a line, ensure we calculate the correct word count."
    (is (= (:num-words (next-line (->word-count 0) "")) 0))
    (is (= (:num-words (next-line (->word-count 0) "hello")) 1))
    (is (= (:num-words (next-line (->word-count 0) "hello there")) 2))))

(deftest process-line-test
  (testing "Given a line, ensure we update all the passed stats."
    (let [updated-stats (process-line [(->line-count 1) 
                                       (->line-count 2)
                                       (->line-count 3)] "hello")]
      (is (= (:num-lines (nth updated-stats 0)) 2)) 
      (is (= (:num-lines (nth updated-stats 1)) 3)) 
      (is (= (:num-lines (nth updated-stats 2)) 4)))))

