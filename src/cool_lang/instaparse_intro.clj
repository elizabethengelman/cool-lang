(ns cool_lang.instaparse-intro
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

(def parser
  (insta/parser
   "expr = number | vector | operation | exit | string
    exit = 'exit'
    operation = operator space+ vector
    operator = '+' | '-' | '*' | '/' | 'S+'
    vector = ((space)* number (space)*)+ | ((space)* string (space)*)+
    <space> = <#'[\\s\\t\\n\\,]+'>
    number = #'[0-9]+'
    string = #'\"(.+)\"'"))

(defn goaway [op]
  (println "Guess you're not cool...")
  (System/exit 0)
)

(defn choose-operator [op]
  (case op
    "+" +
    "-" -
    "*" *
    "/" /
    "S+" string/join))

(def transform-options
  {:exit goaway
   :number read-string
   :vector (comp vec list)
   :operator choose-operator
   :operation apply
   :expr identity
   :string identity})

(defn parse [input]
  (->> (parser input)
     (insta/transform transform-options)))

(defn complete-input? [input]
  (let [lbrackets (count (re-find #"\[" input))
        rbrackets (count (re-find #"]" input))
        rparens (count (re-find #"\(" input))
        lparens (count (re-find #"\)" input))
        ]
    (and (= lparens rparens) (= lbrackets rbrackets))))

(defn get-input [input]
  (let [new-input (str input (read-line))]
    (if (complete-input? new-input)
      new-input
      (do
        (print "  ..cool> ")
        (flush)
        (recur (str new-input "\n"))))))

(defn repl []
  (do
    (print "MyCoolLang> ")
    (flush))
  (let [input (get-input "")]
    (if-not (= input "quit")
     (do
       (println (try (parse input)
                     (catch Exception e (str "Sorry: " e " - " (.getMessage e)))))
       (recur))
     (do (println "Bye!")
         (System/exit 0)))))

(defn -main [& args]
  (repl))

