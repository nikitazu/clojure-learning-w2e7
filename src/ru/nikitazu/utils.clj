;;;;
;;;; Utility functions
;;;;

(ns ru.nikitazu.utils
  ;(:require )
  ;(:use )
  (:import java.util.Calendar)
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

(defn inc-nil [value]
  (if (nil? value) 1 (inc value)))


;;; Date calendar wrapper
;;; =====================

(defn date
  "Create new date object. If year, month and day are not specified,
    then current date object will be created."
  ([]
    (Calendar/getInstance))

  ([year month day]
    (doto (Calendar/getInstance)
      (.set year month day))))

(defn year
  ([] (year (date)))
  ([date] (if (nil? date) 
            nil 
            (.get date Calendar/YEAR))))

(defn month
  ([] (month (date)))
  ([date] (if (nil? date) 
            nil
            (.get date Calendar/MONTH))))

(defn day
  ([] (day (date)))
  ([date] (if (nil? date) 
            nil
            (.get date Calendar/DATE))))

(defn date->string
  ([] (date->string (date)))
  ([date] (if (nil? date) 
            nil
            (str (year date) "-" (month date) "-" (day date)))))

(defn date=
  ([d1] (date= d1 (date)))
  ([d1 d2] (if (or (nil? d1)
                   (nil? d2))
             nil
             (= (.compareTo d1 d2) 0))))

(defn date>
  ([d1] (date> d1 (date)))
  ([d1 d2] (if (or (nil? d1)
                   (nil? d2))
             nil
             (= (.compareTo d1 d2) 1))))

(defn date<
  ([d1] (date< d1 (date)))
  ([d1 d2] (if (or (nil? d1)
                   (nil? d2))
             nil
             (= (.compareTo d1 d2) -1))))





