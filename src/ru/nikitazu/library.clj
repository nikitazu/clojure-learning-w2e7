;; A personal book library management appication

(ns ru.nikitazu.library
  (:import java.util.Date)
  (:use ru.nikitazu.utils)
  (:use clojure.contrib.def))

(defstruct book :title :authors :loan-data)
(defstruct loan-data :owner :posessor :return-by)

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
    title authors
    (struct loan-data
      owner posessor return-by)))


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
  (let [owner (-> book :loan-data :owner)]
    (when (not (is-me? owner))
      (throw
        (Exception. 
          (str "This book belongs to " owner ". " message))))))

;; todo give-to
;; to give book you should be the owner and the posessor

;; todo return-by date should not be now but a defined value

(defnk loan-to [person book :return-by (Date.)]
  "Loan a book to some person for some time.
    person    : The one who will be the posessor of a book.
    book      : A book, that will be given to a person.
    return-by : A date by wich a person must give the book back to you.

   If you are not the owner, an exception will be thrown.
   Successful execution sets book loan-data."
  (check-owner book "It is impolite to loan somebody else's book.")
  (assoc book :loan-data (struct loan-data *me* person return-by)))


(defn accept-return [book]
  "Accept that a book was returned to you.
    book      : A book, that will be given to a person.

   If you are not the owner, an exception will be thrown.
   Successful execution clears book loan-data."
  (check-owner book "You can't accept it.")
  (assoc book :loan-data (struct loan-data *me* *me* nil)))


(defnk loan-to! [key person book :return-by (Date.)]
  (lib-update-key! key (fn [lib]
                          (let [loaned (loan-to person 
                                                book
                                                :return-by return-by)
                                without (remove-book lib book)]
                            (add-book without loaned)))))


(defn book->string
  [ { :keys [title authors loan-data] :as book } ]
  (let [{ :keys [owner posessor return-by] } loan-data
        [fst snd & more] authors
        short-authors (comma-sep (filter seq
                                   [fst snd (when more "et. al.")]))]
    (str "Title       : " title "\n"
         "  Author    : " short-authors "\n"
         "  Owner     : " owner "\n"
         "  Posessor  : " posessor "\n"
         "  Return by : " return-by "\n"
         "  Raw       : " (pr-str book))))

(defn books->string [& books]
  (let [book-strings (map book->string books)]
    (str (apply str (new-line-sep book-strings))
         "\n==========\n"
         "Printed information on " (count books) " books.")))

(defn print-books [& books]
  (let [str (apply books->string books)]
    (println str)))


(defn -main []
  (make-library!
    :default
    (new-book "Peter Pan"
              :authors ["J. M. Barrie"])

    (new-book "The Adventures of Tom Sawyer"
              :authors ["Mark Twain "])

    (new-book "The Two Captains"
              :authors ["Veniamin Kaverin"]))

  (make-library!
    :programming
    (new-book
            "Structure and Interpretation of Computer Programs"
            :authors ["Harold Abelson"
                      "Gerald Jay Sussman"
                      "Julie Sussman"])

    (new-book
            "Programming Clojure"
            :authors ["Stuart Halloway"])

    (new-book
            "Modern Compiler Implementation in ML"
            :authors ["Andrew W. Appel"]))

  (make-library!
    :best-sellers
    (new-book
            "The Big Short"
            :authors ["Michael Lewis"])
    (new-book
            "The Help"
            :authors ["Kathryn Stockett"])
    (new-book
            "Change Your Prain, Change Your Body"
            :authors ["Daniel G. Amen M.D."])
    (new-book
           "Food Rules"
            :authors ["Michael Pollan"])
    (new-book
            "Courage and Consequence"
            :authors ["Karl Rove"])
    (new-book
            "A Patriot's History of the United States"
            :authors ["Larry Schweikart","Michael Allen"])
    (new-book
            "The 48 Laws of Power"
            :authors ["Robert Greene"])
    (new-book
            "The Five Thousand Year Leap"
            :authors ["W. Cleon Skousen",
                      "James Michael Pratt",
                      "Carlos L Packard",
                      "Evan Frederickson"])
    (new-book
            "Chelsea Chelsea Bang Bang"
            :authors ["Chelsea Handler"])
    (new-book
            "The Kind Diet"
            :authors ["Alicia Silverstone","Neal D. Barnard M.D."]))

  (let [boring-book (new-book "Boring book")
        interesting-book (new-book "Interesting book" :authors ["Unknown U.K."])]
    (add-book! boring-book)
    (add-book! interesting-book)
    (loan-to! :default "Daniil Kabluchkov" interesting-book)
    (remove-book! boring-book))

  (doseq [k (lib-keys)]
    (println "\nContence of library" k)
    (println "==========")
    (apply print-books (lib-books k))))
