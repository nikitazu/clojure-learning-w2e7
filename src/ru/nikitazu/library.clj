;; A personal book library management appication

(ns ru.nikitazu.library
  (:use ru.nikitazu.utils)
  (:use clojure.contrib.def)
  (:gen-class))

(defstruct book :title :authors :owner :loan-data)
(defstruct loan-data :posessor :return-by)

(def *libraries* (ref {}))

(def *me* "Nikita B. Zuev")

(defmacro with-login [name & actions]
  "Execute actions while authorised as name."
  `(binding [*me* ~name]
     ~@actions))

(defn is-me? [name]
  (= name *me*))


(defn lib-keys []
  "Get a list of all keys to all libraries that we have."
  (keys @*libraries*))


(defn lib-books
  "Get a list of all books in a library."
  ([] (:default @*libraries*))
  ([key] (get @*libraries* key)))


(defnk new-book
  "Create a new book.
     title    : A title of book
     keywords : authors     - A list of book authors.
                owner       - The one who owns the book.
                posessor    - The one who have the book.
                return-by   - When the book should be returned to its owner.
     default values for keywords:
                authors     []
                owner       *me*
                posessor    *me*
                return-by   nil"
  [title
   :authors     []
   :owner       *me*
   :posessor    *me*
   :return-by   nil]
  (struct book
    title authors owner
    (struct loan-data
      posessor return-by)))


(defn add-book [lib book]
  "Add book to a library."
  (conj lib book))

(defn remove-book [lib book]
  "Remove book from a library."
  (disj lib book))


(defmacro lib-alter! [alter-fn & args]
  "Change libraries, updating their contence.

   Macro applies alter-fn function to a collection of libraries
   supplying args as additional arguments."
  `(dosync
     (alter *libraries* ~alter-fn ~@args)
     nil))


(defn lib-update-key! [key func]
  "Change a single library.

   Applies func function to a library, using the `key` to access it."
  (lib-alter! update-in [key] func))


(defn add-book!
  "Add book to a library.
    key  : A key to access the library.
    book : A new book, that will be added."
  ([book]
    (add-book! :default book))

  ([key book]
    (lib-update-key! key (fn [lib] (add-book lib book)))))


(defn remove-book!
  "Remove a book from a library.
    key  : A key to access the library.
    book : A new book, that will be added."
  ([book]
    (remove-book! :default book))

  ([key book]
    (lib-update-key! key (fn [lib] (remove-book lib book)))))


(defn library-exists? [libs key]
  (contains? libs key))

(defn add-library-anyway [libs key books]
  "Add new library with books to a list of libraries.
    libs    : A list of libraries to add new library.
    key     : A key for a new library.
    books   : A list of books to add to new library.

    Overwrites, if library with the same key already exists."
  (assoc libs
         key
         (reduce add-book #{} books)))


(defn add-library [libs key books]
  "Add new library with books to a list of libraries.
    libs    : A list of libraries to add new library.
    key     : A key for a new library.
    books   : A list of books to add to new library.

    Throws exception, if library with the same key already exists."
  (if (library-exists? libs key)
    (throw
      (Exception. (str "Library with key " key " already exists.")))
    (add-library-anyway libs key books)))


(defn make-library-anyway!
  "Create new library.
    key     : A key to access new library.
    books   : A collection of books to add to new library.

    Overwrites, if library with the same key already exists.
    If no arguments are supplied default empty library will be created"
  ([]
    (make-library-anyway! :default))
  
  ([key & books]
    (lib-alter! add-library-anyway key books)))


(defn make-library!
  "Create new library.
    key     : A key to access new library.
    books   : A collection of books to add to new library.

    Throws exception, if library with the same key already exists.
    If no arguments are supplied default empty library will be created"
  ([]
    (make-library! :default))
  
  ([key & books]
    (lib-alter! add-library key books)))


(defn burn-library [libs key]
  (dissoc libs key))

(defn burn-library! [key]
  "Destroy a library that is not needed anymore."
  (lib-alter! burn-library key))



(defn check-owner [book message]
  "Check if the user is owner of a book or not.
    book    : A book, that is checked for ownership.
    message : A message, that will be part of a thrown exception.

   In case user doesn't own a book, the exception is thrown."
  (let [owner (book :owner)]
    (when-not (is-me? owner)
      (throw
        (Exception. 
          (str "This book belongs to " owner ". " message))))))

;; todo give-to
;; to give book you should be the owner and the posessor

(defn loan-to [person book return-by]
  "Loan a book to some person for some time.
    person    : The one who will be the posessor of a book.
    book      : A book, that will be given to a person.
    return-by : A date by wich a person must give the book back to you.

   If you are not the owner, an exception will be thrown.
   Successful execution sets book loan-data."
  (check-owner book "It is impolite to loan somebody else's book.")
  (assoc book :loan-data (struct loan-data person return-by)))


(defn accept-return [book]
  "Accept that a book was returned to you.
    book      : A book, that will be given to a person.

   If you are not the owner, an exception will be thrown.
   Successful execution clears book loan-data."
  (check-owner book "You can't accept it.")
  (assoc book :loan-data (struct loan-data *me* nil)))


(defnk loan-to! [key person book :return-by nil]
  (lib-update-key! 
    key
    (fn [lib]
      (let [loaned (loan-to person book return-by)
            without (remove-book lib book)]
        (add-book without loaned)))))


(defn book->string
  [ { :keys [title authors loan-data owner] :as book } ]
  (let [{ :keys [posessor return-by] } loan-data
        [fst snd & more] authors
        short-authors (comma-sep (filter seq
                                   [fst snd (when more "et. al.")]))]
    (str "Title       : " title "\n"
         "  Author    : " short-authors "\n"
         "  Owner     : " owner "\n"
         "  Posessor  : " posessor "\n"
         "  Return by : " (date->string return-by) "\n"
         "  Raw       : " (pr-str book))))

(defn books->string [& books]
  (let [book-strings (map book->string books)]
    (str (apply str (new-line-sep book-strings))
         "\n==========\n"
         "Printed information on " (count books) " books.")))

(defn print-books [& books]
  (let [str (apply books->string books)]
    (println str)))


;;; Querying
;;; ========

(defn filter-books [key pred]
  "Filter books in a library by predicate.
    key  : library key
    pred : predicate for a filter"
  (filter pred (lib-books key)))


(defn has-author? [book author]
  "Determine whether book has an specified author."
    (some #{author} (:authors book)))


(defn has-many-authors? [book]
  "Determine whether book has many authors."
  (> (count (:authors book)) 1))


(defn loaned? [book]
  (let [posessor (->> book :loan-data :posessor)]
    (not (is-me? posessor))))


(defn loan-expired? [book]
  "Determine whether book is loanded and should already be returned but not."
  (and (loaned? book)
       (date< (->> book :loan-data :return-by))))

;;; Querying usage: combine predicates with filter
;;; ==============================================

(defn books-by-author
  ([author]
    (books-by-author :default author))
  
  ([key author]
    (filter-books key #(has-author? % author))))


(defn books-with-many-authors
  ([]
    (books-with-many-authors :default))
  
  ([key]
    (filter-books key has-many-authors?)))


(defn books-loan-expired
  ([]
    (books-loan-expired :default))

  ([key]
    (filter-books key loan-expired?)))


;;; Some funcs for main
;;; ===================

(defn print-all-books []
  (doseq [k (lib-keys)]
    (println "\nContence of library" k)
    (println "==========")
    (apply print-books (lib-books k))))


(defn make-default-library! []
  (make-library! :default
    (new-book "Peter Pan"                    :authors ["J. M. Barrie"])
    (new-book "The Adventures of Tom Sawyer" :authors ["Mark Twain "])
    (new-book "The Two Captains"             :authors ["Veniamin Kaverin"])))


(defn make-programming-library! []
  (make-library! :programming
    (new-book "Structure and Interpretation of Computer Programs"
              :authors ["Harold Abelson"
                        "Gerald Jay Sussman"
                        "Julie Sussman"])

    (new-book "Programming Clojure"
              :authors ["Stuart Halloway"])

    (new-book "Modern Compiler Implementation in ML"
              :authors ["Andrew W. Appel"])))


(defn make-best-sellers-library! []
  (make-library! :best-sellers
    (new-book "The Big Short" :authors ["Michael Lewis"])
    (new-book "The Help"      :authors ["Kathryn Stockett"])
    (new-book "Food Rules"    :authors ["Michael Pollan"])

    (new-book "Chelsea Chelsea Bang Bang" :authors ["Chelsea Handler"])
    (new-book "Courage and Consequence"   :authors ["Karl Rove"])
    (new-book "The 48 Laws of Power"      :authors ["Robert Greene"])

    (new-book "Change Your Prain, Change Your Body"
               :authors ["Daniel G. Amen M.D."])))


(defn add-some-books! []
  (let [boring-book (new-book "Boring book")
        interesting-book (new-book "Interesting book" :authors ["Unknown U.K."])]
    (add-book! boring-book)
    (add-book! interesting-book)
    (loan-to! :default
              "Daniil Kabluchkov"
              interesting-book
              :return-by (date 2010 10 10))
    (remove-book! boring-book)))


(defn make-3-best-sellers-loan-expired! []
  (let [bs1 (new-book "A Patriot's History of the United States"
              :authors ["Larry Schweikart","Michael Allen"])

        bs2 (new-book "The Five Thousand Year Leap"
              :authors ["W. Cleon Skousen",
                        "James Michael Pratt",
                        "Carlos L Packard",
                        "Evan Frederickson"])

        bs3 (new-book "The Kind Diet"
              :authors ["Alicia Silverstone","Neal D. Barnard M.D."])

        friend "Daniil Kabluchkov"
        expiration-date (date 2008 10 10)]
    
    (doseq [book [bs1 bs2 bs3]]
      (add-book! :best-sellers book)
      (loan-to! :best-sellers friend book :return-by expiration-date))))


(comment defn unreliable-guys [key]
  (let [expired (books-loan-expired key)
        guys    (->> book :loan-data :posessor)]))

(defn unreliable-guys2 [key]
  (loop [books (books-loan-expired key)
         stats {}]
    (let [head (first books)
          guy (->> head :loan-data :posessor)
          stats (assoc stats guy (inc-nil (get stats guy)))]
      (if-let [books (next books)]
        (recur books stats)
        stats))))


(defn -main []
  (make-default-library!)
  (make-programming-library!)
  (make-best-sellers-library!)
  (add-some-books!)
  (make-3-best-sellers-loan-expired!)

  (println "\nUnreliable guys:")
  (doseq [[guy books] (unreliable-guys2 :best-sellers)]
    (println "  " guy ":" books " books."))

  (print-all-books)

  (println "\nProgramming books by Stuart Halloway:")
  (apply print-books
         (books-by-author :programming
                          "Stuart Halloway"))

  (println "\nMultiauthored best sellers:"
           (count (books-with-many-authors :best-sellers))))

