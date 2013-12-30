(in-package :cl-user)

(defpackage :cl-transliterate
  (:use :cl :iterate)
  (:export :transliterate))

(in-package :cl-transliterate)

(defparameter *transliterate-table*
  '(#\а "a"
    #\б "b"
    #\в "v"
    #\г "g"
    #\д "d"
    #\е "e"
    #\ё "e"
    #\ж "zh"
    #\з "z"
    #\и "i"
    #\й "i"
    #\к "k"
    #\л "l"
    #\м "m"
    #\н "n"
    #\о "o"
    #\п "p"
    #\р "r"
    #\с "s"
    #\т "t"
    #\у "u"
    #\ф "f"
    #\х "kh"
    #\ц "ts"
    #\ч "ch"
    #\ш "sh"
    #\щ "shch"
    #\ь "-"
    #\ы "y"
    #\ъ "-"
    #\э "e"
    #\ю "iu"
    #\я "ia"))

(defun trans (character &optional (table *transliterate-table*))
  (if (alpha-char-p character)
      (if (upper-case-p character)
          (string-capitalize (getf table
                                   (char-downcase character)
                                   (make-string 1 :initial-element character)))
          (getf table
                character
                (make-string 1 :initial-element character)))
      (make-string 1 :initial-element character)))

(defun transliterate (string &optional (table *transliterate-table*))
  (apply #'concatenate 'string
         (iter (for char in-string string)
               (collect (trans char table)))))
