(require '[clojure.string :as string])
(defn split-line [line]
  (string/split line #" "))

(defn read-lines [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (doall (map split-line (line-seq reader)))))



(defn id? [check]
  (not (string/blank? (re-matches #"[A-Z]" check))))
(defn number-char? [check]
  (not (string/blank? (re-matches #"[0-9]" check))))
(defn valid-operator? [operator]
  (some #(= % operator) ["+"]))
(defn error []
  println "Reject")
(defn accept []
  println "Accept")

(declare validate-factor validate-expression validate-term)
(defn validate-expression [lexemes state]
  (if (empty? lexemes)
    (if (= (:paren-depth state) 0)
      #(accept) ;then
      #(error)) ;else
    #(validate-term lexemes (assoc state :from "expression"))) ;outer else
  )

(defn validate-term [lexemes state]
  (let [from (:from state)]
    (if (= from "factor")
      (if (empty? lexemes)
        #(validate-expression lexemes (assoc state :from "term"))
        (let [lexeme (first lexemes)]
          (cond
            (valid-operator? lexeme) #(validate-factor (rest lexemes) (assoc state :from "term"))
            (= lexeme ")") #(validate-factor lexemes (assoc state :from "term"))
            :else #(error))))
      #(validate-factor lexemes (assoc state :from "term")))))

(defn validate-factor [lexemes state]
  (if (empty? lexemes)
    #(error)
    (let [lexeme (first lexemes) paren-depth (:paren-depth state)]
      (cond
        (id? lexeme) #(validate-term (rest lexemes) (assoc state :paren-depth paren-depth :from "factor"))
        (number-char? lexeme) #(validate-term (rest lexemes) (assoc state :paren-depth paren-depth :from "factor"))
        (= lexeme "(") #(validate-expression (rest lexemes) (assoc state :paren-depth (inc paren-depth) :from "factor"))
        (= lexeme ")") #(validate-term (rest lexemes) (assoc state :paren-depth (dec paren-depth) :from "factor"))
        :else #(error)))))


(defn parse [input]
  (let [state {:paren-depth 0 :from nil} ]
    (println (trampoline validate-expression input state))))

(defn parse-file [filename]
  (let [lines (read-lines filename)]
    (doseq [line lines]
      (parse line))))

(parse-file "C:\\Users\\osiri_000\\Documents\\GitHub\\ClojureParse\\commands.txt")