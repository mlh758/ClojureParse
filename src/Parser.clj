(require '[clojure.string :as str])
(defn split-line [line]
  (str/split line #" "))

(defn read-lines [filename]
  (with-open [reader (clojure.java.io/reader filename)]
    (for [line (doall (line-seq reader))] (split-line line))))

;(doseq [line (read-lines "C:\\Users\\osiri_000\\Documents\\GitHub\\ClojureParse\\commands.txt")]
;  (println line))

(defn id? [check]
  (not (clojure.string/blank? (re-matches #"[A-Z]" check))))
(defn number-char? [check]
  (not (clojure.string/blank? (re-matches #"[0-9]" check))))
(defn error []
  println "Reject")
(defn accept []
  println "Accept")

(declare factor? expression? term?)
(defn expression? [lexemes state]
  (if (empty? lexemes)
    (if (= (:paren-depth state) 0)
      #(accept) ;then
      #(error)) ;else
    #(term? lexemes (assoc state :from "expression"))) ;outer else
  )

(defn term? [lexemes state]
  (let [from (:from state)]
    (if (= from "factor")
      (if (empty? lexemes)
        #(expression? lexemes (assoc state :from "term"))
        (let [lexeme (first lexemes)]
          (cond
            (= lexeme "+") #(factor? (rest lexemes) (assoc state :from "term"))
            (= lexeme ")") #(factor? lexemes (assoc state :from "term"))
            :else #(error))))
      #(factor? lexemes (assoc state :from "term")))))

(defn factor? [lexemes state]
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
  (let [state {:paren-depth 0 :from nil} ]
    (println (trampoline expression? input state))))

(defn parse-file [filename]
  (let [lines (read-lines filename)]
    (doseq [line lines]
      (parse line))))

(parse-file "C:\\Users\\osiri_000\\Documents\\GitHub\\ClojureParse\\commands.txt")
;(parse ["A" "+" "(" "C" ")"])




