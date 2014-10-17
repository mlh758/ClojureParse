(require '[clojure.string :as str])
(defn split-line [line]
  "Splits a line of text into a sequence of strings"
  (str/split line #" "))

(defn read-lines [filename]
  "Reads a file and returns a sequence made up of a vector for each line"
  (with-open [reader (clojure.java.io/reader filename)]
    (for [line (doall (line-seq reader))] (split-line line))))

(defn id? [check]
  "Checks if CHECK is an ID"
  (not (clojure.string/blank? (re-matches #"[A-Z]" check))))
(defn number-char? [check]
  "Checks if CHECK is a number character"
  (not (clojure.string/blank? (re-matches #"[0-9]" check))))
(defn error []
  println "Reject")
(defn accept []
  println "Accept")
(defn valid-operator? [lexeme]
  (cond
    (= lexeme "+") true
    :else false))

(declare factor? expression? term?)
(defn expression? [lexemes state]
  "Verifies if input is an expression, updates STATE with FROM"
  (if (empty? lexemes)
    (if (= (:paren-depth state) 0)
      #(accept) ;then
      #(error)) ;else
    #(term? lexemes (assoc state :from "expression"))) ;outer else
  )

(defn term? [lexemes state]
  "Verifies if input is a term, updates STATE with FROM"
  (let [from (:from state)]
    (if (= from "factor")
      (if (empty? lexemes)
        #(expression? lexemes (assoc state :from "term"))
        (let [lexeme (first lexemes)]
          (cond
            (valid-operator? lexeme) #(factor? (rest lexemes) (assoc state :from "term"))
            (= lexeme ")") #(factor? lexemes (assoc state :from "term"))
            :else #(error))))
      #(factor? lexemes (assoc state :from "term")))))


(defn factor? [lexemes state]
  "Checks if input is a factor, updates STATE with FROM and PAREN-DEPTH"
  (if (empty? lexemes)
    #(error)
    (let [lexeme (first lexemes) paren-depth (:paren-depth state)]
      (cond
        (id? lexeme) #(term? (rest lexemes) (assoc state :paren-depth paren-depth :from "factor"))
        (number-char? lexeme) #(term? (rest lexemes) (assoc state :paren-depth paren-depth :from "factor"))
        (= lexeme "(") #(expression? (rest lexemes) (assoc state :paren-depth (inc paren-depth) :from "factor"))
        (= lexeme ")") #(term? (rest lexemes) (assoc state :paren-depth (dec paren-depth) :from "factor"))
        :else #(error)))))


(defn parse [input]
  "Takes a vector of strings and either accepts or rejects it"
  (let [state {:paren-depth 0 :from nil} ]
    (println (trampoline expression? input state))))

(defn parse-file [filename]
  "Reads a file and parses each line"
  (let [lines (read-lines filename)]
    (doseq [line lines]
      (parse line))))

(parse-file "C:\\Users\\osiri_000\\Documents\\GitHub\\ClojureParse\\commands.txt")
;(parse ["A" "+" "(" "C" ")" "+" "(" "1" "+" "5" ")" ])




