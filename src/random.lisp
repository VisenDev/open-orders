(defpackage #:open-orders.random
  (:use #:cl)
  (:export
   #:street
   #:full-name
   #:n-digit-number
   #:date
   #:get-current-year
   #:capital-letter
   #:random-value))
(in-package #:open-orders.random)

(defparameter *first-names*
  '("James" "Michael" "John" "Robert" "David" "William"
    "Richard" "Joseph" "Thomas" "Christopher" "Charles" "Daniel"
    "Matthew" "Anthony" "Mark" "Steven" "Andrew" "Donald"
    "Joshua" "Paul" "Kevin" "Kenneth" "Brian" "Timothy"
    "Ronald" "Jason" "George" "Edward" "Jeffrey" "Jacob"
    "Ryan" "Nicholas" "Gary" "Eric" "Jonathan" "Stephen"
    "Larry" "Justin" "Benjamin" "Scott" "Brandon" "Samuel"
    "Alexander" "Gregory" "Patrick" "Jack" "Frank" "Raymond"
    "Dennis" "Aaron" "Tyler" "Jerry" "Jose" "Nathan"
    "Adam" "Henry" "Zachary" "Douglas" "Peter" "Noah"
    "Ethan" "Kyle" "Christian" "Jeremy" "Austin" "Keith"
    "Sean" "Terry" "Roger" "Dylan" "Walter" "Gerald"
    "Jordan" "Gabriel" "Carl" "Bryan" "Jesse" "Logan"
    "Lawrence" "Elijah" "Arthur" "Bruce" "Harold" "Billy"
    "Liam" "Alan" "Juan" "Joe" "Mason" "Lucas"
    "Randy" "Willie" "Wayne" "Vincent" "Caleb" "Albert"
    "Luke" "Isaac" "Bradley" "Cameron" "Mary" "Patricia"
    "Jennifer" "Linda" "Elizabeth" "Barbara" "Susan" "Jessica"
    "Karen" "Lisa" "Nancy" "Sandra" "Ashley" "Emily"
    "Kimberly" "Michelle" "Donna" "Margaret" "Carol" "Betty"
    "Amanda" "Melissa" "Deborah" "Stephanie" "Rebecca" "Sharon"
    "Cynthia" "Laura" "Amy" "Kathleen" "Angela" "Emma"
    "Shirley" "Dorothy" "Brenda" "Nicole" "Pamela" "Samantha"
    "Anna" "Olivia" "Katherine" "Christine" "Debra" "Rachel"
    "Maria" "Carolyn" "Janet" "Heather" "Diane" "Julie"
    "Catherine" "Victoria" "Joyce" "Lauren" "Kelly" "Christina"
    "Helen" "Joan" "Hannah" "Judith" "Andrea" "Evelyn"
    "Megan" "Cheryl" "Ruth" "Virginia" "Sophia" "Madison"
    "Jacqueline" "Abigail" "Isabella" "Teresa" "Sara" "Charlotte"
    "Janice" "Kathryn" "Martha" "Gloria" "Ann" "Judy"
    "Amber" "Danielle" "Denise" "Grace" "Julia" "Natalie"
    "Diana" "Marilyn" "Brittany" "Beverly" "Alice" "Theresa"
    "Jean" "Kayla" "Alexis" "Frances" "Ava" "Tiffany"
    "Lori" "Sarah"))


(defparameter *last-names*
  '("Smith" "Johnson" "Williams" "Brown" "Jones" "Garcia" "Miller"
    "Rodriguez" "Martinez" "Hernandez" "Gonzales" "Wilson" "Anderson"
    "Thomas" "Taylor" "Moore" "Jackson" "Martin" "Lee" "Perez" "Thompson"
    "White" "Harris" "Sanchez" "Clark" "Ramirez" "Lewis" "Robinson"
    "Walker" "Young" "Allen" "King" "Wright" "Scott" "Torres"
    "Nguyen" "Hill" "Flores" "Green" "Adams" "Nelson" "Baker"
    "Hall" "Rivera" "Campbell" "Mitchell" "Carter" "Roberts" "Gomez"
    "Phillips" "Evans" "Turner" "Diaz" "Parker" "Cruz" "Edwards"
    "Collins" "Reyes" "Stewart" "Morris" "Morales" "Murphy" "Cook"
    "Rogers" "Gutierrez" "Ortiz" "Morgan" "Cooper" "Peterson" "Bailey"
    "Reed" "Kelly" "Howard" "Ramos" "Kim" "Cox" "Ward"
    "Richardson" "Watson" "Brooks" "Chavez" "Wood" "James" "Bennet"
    "Gray" "Mendoza" "Ruiz" "Hughes" "Price" "Alvarez" "Castillo"
    "Sanders" "Patel" "Myers" "Long" "Ross" "Foster" "Jimenez"))

(defparameter *last-name-prefixes*
  '("Von " "Van " "Del " "O'"))

(defparameter *trees*
  '("Ash" "Oak" "Post Oak" "Live Oak" "Cedar"
    "Juniper" "Elm" "Cedar Elm" "Birch" "Apple"
    "Orange" "Lemon" "Walnut" "Pecan"))

(defparameter *flowers*
  '("Rose" "Daisy" "Violet" "Dandelion" "Briar"
    "Sunflower" "Petunia" "Tulip" "Bluebonnet"
    "Forget Me Not" "Sunwheel" "Indian Paintbrush"
    "Mayflower"))

(defparameter *rocks*
  '("Granite" "Diorite" "Slate" "Mica" "Limestone" "Basalt"))

(defparameter *presidents* 
  '("Bush" "Clinton" "Reagan" "Carter" "Ford"
    "Nixon" "Johnson" "Fitzgerald" "Eisenhower" "Truman"
    "Roosevelt" "Hoover" "Coolidge" "Harding" "Wilson"
    "Taft" "Roosevelt" "McKinley" "Cleveland" "Harrison"
    "Cleveland" "Arthur" "Garfield" "Hayes" "Grant"
    "Johnson" "Lincoln" "Buchanan" "Pierce" "Fillmore"
    "Taylor" "Polk" "Tyler" "Harrison" "Van"
    "Jackson" "Adams" "Monroe" "Madison" "Jefferson"
    "Washington"))

(defparameter *vegetables*
  '("Pumpkin" "Squash" "Butternut" "Radish" "Melon"
    "Cucumber" "Carrot"))

(defparameter *berry-types* 
  '("Hack" "Huckle" "Straw" "Blue" "Black" "Mul"
     "Rasp" "Cran" "Elder" "Goji" "Dew"))
(defparameter *berries* (mapcar (lambda (berry)
                                  (concatenate 'string berry "berry"))
                                *berry-types*))

(defparameter *outdoor-places*
  '("Meadow" "Mountain" "Forest" "Woods" "Patch"
    "Knoll" "Plains"))

(defparameter *outdoor-place-adjectives*
  '("Grassy" "Sunny" "Peaceful" "Somber" "Quiet"
    "Cloudy" "Green" "Gray" "Rocky" "Stoney" "Leafy"
    "Pleasant"))

(defparameter *outdoor-place-prefixes*
  (concatenate 'list *vegetables* *berries* *trees*
               *outdoor-place-adjectives*))

(defparameter *basic-street-names*
  (concatenate 'list *trees* *flowers* *rocks* *presidents*
               *vegetables*))

(defparameter *other-street-types*
  '("Avenue" "Parkway" "Circle" "Way" "Loop"
    "Highway" "Trail" "Path"))

(defparameter *full-name-suffixes*
  '("Junior" "Senior" "the Second" "the Third"))

(defparameter *nicknames*
  '("Bobby" "Johnny" "Dan" "Danny" "Bob" "Jon"
    "Bubba" "Jose" "Big John" "Mikey" "Slim" "Little" "Hands"
    "Tiny" "Scar" "Biggie" "Jim" "Buck" "99"))

(defparameter *street-name-other-street-type-chance* 2/3)
(defparameter *street-name-single-letter-chance* 1/25)
(defparameter *street-name-numerical-chance* 1/15)
(defparameter *street-name-full-name-chance* 1/20)
(defparameter *street-name-highway-chance* 1/20)
(defparameter *street-name-outdoor-place-chance* 1/15)

(defun random-street-type ()
  (if (rarely *street-name-other-street-type-chance*)
      (random-value *other-street-types*)
      "Street"))

;; initials for first name like JK or DH
;; suffixes for full name like Junior, Senior, The Second
;; nicknames in the middle like "Bobby", or "Jonny"

(defun upcase-first-letter (string)
  (let ((copy (copy-seq string)))
    (setf (char copy 0) (char-upcase (char string 0)))
    copy))

(defun capital-letter ()
  (make-string
   1 :initial-element
   (code-char (+ (char-code #\A)
                 (random (- (char-code #\Z) (char-code #\A)))))))

(defparameter *vowels* '(#\a #\e #\i #\o #\u))
(defun vowelp (letter) (member letter *vowels*))
(defparameter *consonants*
  (loop :for code :from (char-code #\a) :to (char-code #\z)
        :for char = (code-char code) 
        :unless (vowelp char)
          :collect char))
(defun consonantp (letter)
  (member letter *consonants*))

(defparameter *last-name-prefix-chance* 1/15)
(defparameter *first/last-swap-chance* 1/10)
(defparameter *name-change-letter-chance* 1/12)
(defparameter *full-name-suffix-chance* 1/25)
(defparameter *nickname-chance* 1/28)

(defun random-index (list)
  (random (length list)))

(defun random-value (list)
  (nth (random-index list) list))

(defun rarely (chance)
  "chance should be a fraction like 1/10, or 1/100"
  (assert (>= chance 0/1))
  (assert (<= chance 1/1))
  (= 1 (truncate (random (* 10 (ceiling (/ 1 chance)))) 10)))

(defun random-consonant ()
  (random-value *consonants*))

(defun random-vowel ()
  (random-value *vowels*))

(defun change-random-letter (string)
  "changes a random letter to a random consonant or vowel"
  (let* ((copy (copy-seq string))
         (i (random-index string))
         (ch (aref string i))
         (new-letter (if (vowelp ch)
                         (random-vowel)
                         (random-consonant))))
    (setf (aref copy i)
          (if (upper-case-p ch)
              (char-upcase new-letter)
              new-letter))
    copy))

(defun rarely-change-letter (string)
  (if (rarely *name-change-letter-chance*)
      (change-random-letter string)
      string))

(defun first-name ()
  (rarely-change-letter
   (random-value
    (if (rarely *first/last-swap-chance*)
        *last-names*
        *first-names*))))

(defun last-name ()
  (rarely-change-letter
   (random-value
    (if (rarely *first/last-swap-chance*)
        *first-names*
        *last-names*))))

(defun rarely-last-name-prefix ()
  (if (rarely *last-name-prefix-chance*)
      (random-value *last-name-prefixes*)
      ""))

(defun rarely-nickname ()
  (if (rarely *nickname-chance*)
      (format nil " \"~a\"" (random-value *nicknames*))
      ""))

(defun rarely-full-name-suffix ()
  (if (rarely *full-name-suffix-chance*)
      (concatenate 'string " "
                   (random-value *full-name-suffixes*))
      ""))

(defparameter *three-initials-chance* 1/20)

(defun initials ()
  (concatenate
   'string
   (capital-letter)
   "."
   (capital-letter)
   "."
   (if (rarely *three-initials-chance*)
       (concatenate 'string (capital-letter) ".")
       "")))

(defparameter *initials-as-first-name-chance* 1/20)

(defun full-name ()
  (concatenate
   'string
   (if (rarely *initials-as-first-name-chance*)
       (initials)
       (first-name))
   (rarely-nickname)
   " " (rarely-last-name-prefix) (last-name)
   (rarely-full-name-suffix)))


(defun street ()
  (cond
    ((rarely *street-name-single-letter-chance*)
     (format nil "Avenue ~a" (capital-letter)))
    ((rarely *street-name-numerical-chance*)
     (upcase-first-letter
      (format nil "~:R ~a" (random 100) (random-street-type))))
    ((rarely *street-name-full-name-chance*)
     (concatenate 'string (full-name)
                  " " (random-street-type)))
    ((rarely *street-name-highway-chance*)
     (format nil "Highway ~a" (random 10000)))
    ((rarely *street-name-outdoor-place-chance*)
     (concatenate 'string (random-value *outdoor-place-prefixes*)
                  " " (random-value *outdoor-places*)
                  " " (random-street-type)))
    (t (concatenate 'string (random-value *basic-street-names*)
                    " " (random-street-type)))))

(defun n-digit-number (n)
  (if (plusp n)
      (format nil "~a~a"
              (random 10)
              (n-digit-number (1- n)))
      ""))

(defparameter *months*
  '((:name "January"   :abbrev "Jan" :days 31)
    (:name "February"  :abbrev "Feb" :days 28)
    (:name "March"     :abbrev "Mar" :days 31)
    (:name "April"     :abbrev "Apr" :days 30)
    (:name "May"       :abbrev "May" :days 31)
    (:name "June"      :abbrev "Jun" :days 30)
    (:name "July"      :abbrev "Jul" :days 31)
    (:name "August"    :abbrev "Aug" :days 31)
    (:name "September" :abbrev "Sep" :days 30)
    (:name "October"   :abbrev "Oct" :days 31)
    (:name "November"  :abbrev "Nov" :days 30)
    (:name "December"  :abbrev "Dec" :days 31)))

(defun get-current-year ()
  (nth-value 5
             (decode-universal-time
              (get-universal-time))))

(defun date (&key
               (min-year (get-current-year))
               (max-year (1+ (get-current-year))))
  (let ((month (random-value *months*)))
    (format nil "~a ~a, ~a"
            (getf month :name)
            (1+ (random (getf month :days)))
            (+ min-year (random (1+ (- max-year min-year)))))))
