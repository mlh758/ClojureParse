(require '[clojure.string :as string])
(defn split-line [line]
  (string/split line #" "))

(defn read-lines [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (doall (map split-line (line-seq reader)))))


(defn id? [check]
  (not (string/blank? (re-matches #"[A-Za-z]" check))))
(defn number-char? [check]
  (not (string/blank? (re-matches #"[0-9]" check))))
(defn valid-operator? [operator]
  (some #(= % operator) ["+" "-" "*" "/"]))
(defn error [problem location]
  (println "Reject, invalid" problem " at " (if (nil? location) "end" location)))
(defn accept []
  (println "Accept"))
(defn id-or-numchar? [lexeme]
  (or
    (number-char? lexeme)
    (id? lexeme)))

(declare validate-factor validate-expression validate-term)
(defn validate-expression [lexemes paren-depth]
  (if (empty? lexemes)
    (if (= paren-depth 0)
      #(accept) ;then
      #(error "expression" lexemes)) ;else
    #(validate-factor lexemes paren-depth)) ;outer else
  )

(defn validate-term [lexemes paren-depth]
  (if-not (empty? lexemes)
    (let [[head & tail] lexemes]
      (cond
        (valid-operator? head) #(validate-factor tail paren-depth)
        (= head ")") #(validate-term tail (dec paren-depth))
        :else #(error "term" lexemes)))
    #(validate-expression lexemes paren-depth)))

(defn validate-factor [lexemes paren-depth]
  (if-not (empty? lexemes)
    (let [[head & tail] lexemes]
      (cond
        (id-or-numchar? head) #(validate-term tail paren-depth)
        (= head "(") #(validate-factor tail (inc paren-depth))
        :else #(error "factor" lexemes)))
    #(error "factor" lexemes)))

(defn parse [input]
  (trampoline validate-expression input 0))

(defn parse-file [filename]
  (let [lines (read-lines filename)]
    (doseq [line lines]
      (parse line))))

(parse-file "C:\\Users\\osiri_000\\Documents\\GitHub\\ClojureParse\\commands.txt")