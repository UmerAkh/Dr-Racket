#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CS 3180 Fall Assignment 01
;; Muhammad Umer Akhter
;;
;; Only following references were used to inform the
;; development solutions in this file:
;; https://www.rosettacode.org/wiki/Count_occurrences_of_a_substring---> for regexp
;; https://docs.racket-lang.org/reference/strings.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The program will output all seven letter words that do not
;; contain the letters, e,i,o,u. In other words, the only vowels are a or y.
;; The case of the letters will not impact the program. The words in the outputs
;; must all be on the same line delimited by commas with no quotation marks and no
;; trailing comma after the last word. The program will also output a count of total
;; number of letter 'z' found in the entire file regardless of word length.


(require 2htdp/batch-io)


;; DESCRIPTION
;; wordIsValid function will determine the valid word from thetxt file
;; If the word is valid it will return true otherwise it will return false.
;; This function will be called by other function to check the validity of the word

(define (wordIsValid? word)
  [cond
    ;if the word contain e OR E the function will return False for that particular word
    [(string-contains? (string-downcase word) "e") #f]
    ;if the word contain i OR I the function will return False for that particular word
    [(string-contains? (string-downcase word) "i") #f]
    ;if the word contain o OR O the function will return False for that particular word
    [(string-contains? (string-downcase word) "o") #f]
    ;if the word contain u OR U the function will return False for that particular word
    [(string-contains? (string-downcase word) "u") #f]
    ; if the length of word is not equal to 7 return false.
    [(not (eq? (string-length word) 7)) #f]
    ; if the length of word is equal to 7 return true.
    [(eq? (string-length word) 7) #t]];; ending condtion
  ) ;Function ends


;; DESCRIPTION
;; wordExtractor function will extract the valid/required words from the file and
;; adding comma at the end of the word.
;; This fucntion will be using wordIsValid and filter function in order perform the
;; operation.
(define (wordExtractor wordlst)
(displayln(string-join (filter wordIsValid? wordlst) ", "));  display the result
  ); Function ends
  
(wordExtractor (file->lines "words.txt")) ; calling the function in order to read the words from each line

;; The below fucntion is also reading strings from the txt file which will be later used
;; to count the total number of z in the file
(define words_string (file->string "words.txt"))

;; The fucntion countNumberOfZ will actually count the z's from the file.

(define countNumberOfZ
  (compose length regexp-match*)) ; regexp will be used to get the z's from the file.


;; Adding total number of lower-case z in the file and total number of Upper-case z
;; in the file together.
(+(countNumberOfZ "z" words_string)(countNumberOfZ "Z" words_string))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PROGRAM Ends






