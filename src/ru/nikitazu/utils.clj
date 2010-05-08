;;;;
;;;; Utility functions
;;;;

(ns ru.nikitazu.utils
  ;(:require )
  ;(:use )
  ;(:import )
  )


(defn separate-by [sep items]
  "Make a string out of items separated by sep."
  (apply str (interpose sep items)))

(defn comma-sep
  "Creates a comma-separated string from a sequence of items."
  [items]
  (separate-by ", " items))

(defn new-line-sep
  "Creates a new-line-separated string from a sequence of items."
  [items]
  (separate-by "\n" items))

(defn if-nil? [value1 value2]
  (if (nil? value1) value2 value1))
