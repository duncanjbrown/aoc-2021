(ns aoc
  (:require [clojure.test :refer [is deftest testing]]
            [clojure.string]))

(def input-lines
  (-> (slurp "input")
      (clojure.string/split-lines)))

(def opening-pairs {\( \) \{ \} \[ \] \< \>})
(def closing-pairs {\) \( \} \{ \] \[ \> \<})

(def error-scores {\) 3 \] 57 \} 1197 \> 25137})
(def autocomplete-scores {\) 1 \] 2 \} 3 \> 4})

(defn parse [input]
  (loop [chars input
         stack '()]
    ;; (println "**** LOOP ITERATION ****")
    ;; (println (str "chars < " (apply str chars)))
    ;; (println (str "stack > " (apply str stack)))
    (cond
      (and (empty? chars) (empty? stack))
      {:state :done}

      (empty? chars)
      {:state :incomplete :completion-string (map #(get opening-pairs %1) stack)}

      :else
      (let [current-char (first chars)
            top-of-stack (first stack)
            openings (set (keys opening-pairs))]
        (cond
          ; the only time things can go wrong is when we're closing brackets
          (contains? openings current-char)
          (recur (rest chars) (conj stack current-char))

          ; is the current closing char the one we're waiting for?
          (= (get closing-pairs current-char) top-of-stack)
          (do
           ;; (println (str "Matched " current-char " with " top-of-stack "!"))
           (recur (rest chars) (drop 1 stack)))

          ; got the wrong char
          :else
          {:state :error :error-char current-char})))))

;; (parse "{}")
;; (parse "{{{}}}")
;; (parse "{{}}")

;; (parse "{>")
;; (parse "{{{{{<<<")

(defn score-error [char]
  (get error-scores char 0))

(defn score-autocomplete [chars]
  (reduce (fn [total ch]
            (+ (get autocomplete-scores ch) (* 5 total)))
          0 chars))

(defn middle-value [s]
  (nth s (quot (count s) 2)))

(comment
  (def parsed-lines (map parse input-lines))
  (def lines-with-errors (filter #(= :error (get %1 :state)) parsed-lines))
  (def incomplete-lines (filter #(= :incomplete (get %1 :state)) parsed-lines))

  (def part-1
    (reduce
     (fn [total error-line]
       (+ total (score-error (:error-char error-line))))
     0 lines-with-errors))

  (def part-2
    (let [completion-scores (map score-autocomplete (map :completion-string incomplete-lines))]
      (middle-value (sort completion-scores)))))


(testing "pair matching"
  (deftest basic-pair
    (let [input [\( \)]]
      (is (= {:state :done} (parse input)))))

  (deftest bad-pair
    (let [input [\( \]]]
      (is (= {:state :error :error-char \]} (parse input)))))

  (deftest nested-pairs
    (let [input [\( \( \) \)]]
      (is (= {:state :done} (parse input)))))

  (deftest nested-bad-pair
    (let [input [\( \( \) \]]]
      (is (= {:state :error :error-char \]} (parse input)))))

  (deftest example-1
    (let [input [\{ \( \[ \( \< \{ \} \[ \< \> \[ \] \} \> \{ \[ \] \{ \[ \( \< \( \) \>]]
      (is (= {:state :error :error-char \}} (parse input)))))

  (deftest example-2
    (let [input [\[ \[ \< \[ \( \[ \] \) \) \< \( \[ \[ \{ \} \[ \[ \( \) \] \] \]]]
      (is (= {:state :error :error-char \)} (parse input))))))

(testing "scoring"
  (deftest incomplete-line
    (let [input [\[ \( \{ \( \< \( \( \) \) \[ \] \> \[ \[ \{ \[ \] \{ \< \( \) \< \> \>]]
      (is (= 0 (score-error (:error-char (parse input)))))))

  (deftest bad-paren
    (let [input [\[ \)]]
      (is (= 3 (score-error (:error-char (parse input))))))))

(testing "completion"
  (deftest incomplete
    (let [input [\[ \( \{ \( \< \( \( \) \) \[ \] \> \[ \[ \{ \[ \] \{ \< \( \) \< \> \>]]
      (is (= {:state :incomplete :completion-string '(\} \} \] \] \) \} \) \])} (parse input))))))

(testing "scoring autocomplete"
  (deftest example-auto-1
    (let [input [\} \} \] \] \) \} \) \]]]
      (is (= 288957 (score-autocomplete input))))))

(comment
  (require '[clojure.tools.namespace.repl :refer [refresh]])
  (refresh))
