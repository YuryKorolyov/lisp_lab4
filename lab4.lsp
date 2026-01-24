
(defun extract-min (lst key test)
  (if (null lst)
      (values nil nil)
      ;; Використовуємо reduce для пошуку мінімуму за один прохід
      (let* ((min-pair (reduce (lambda (best current)
                                 (let ((best-val (car best))
                                       (best-key (cdr best))
                                       (curr-key (funcall key current)))
                                   ;; Порівнюємо ключі, щоб мінімізувати виклики key
                                   (if (funcall test curr-key best-key)
                                       (cons current curr-key) ;; Новий мінімум
                                       best)))                 ;; Залишаємо поточний
                               (rest lst)
                               ;; Початкове значення акумулятора: (елемент . ключ)
                               :initial-value (cons (first lst) (funcall key (first lst)))))
             (min-val (car min-pair)))
        
        ;; Повертаємо два значення: знайдений елемент та список без нього
        ;; :count 1 гарантує видалення лише однієї копії
        (values min-val (remove min-val lst :count 1)))))

(defun selection-sort-func (lst &key (key #'identity) (test #'<))
  (cond
    ((null lst) nil)
    (t 
     ;; Використовуємо multiple-value-bind для отримання мінімуму і решти списку
     (multiple-value-bind (min-val rest-lst) (extract-min lst key test)
       (cons min-val 
             (selection-sort-func rest-lst :key key :test test))))))

;;; --- ТЕСТИ ДЛЯ СОРТУВАННЯ ---

(defun check-first-function (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (selection-sort-func input) expected)
          name))

(defun test-first-function ()
  (format t "~%--- Testing Selection Sort ---~%")
  (check-first-function "test 1 (nums)" '(5 3 4 1 2) '(1 2 3 4 5))  
  (check-first-function "test 2 (sorted)" '(1 2 3 4 5) '(1 2 3 4 5)) 
  (check-first-function "test 3 (duplicates)" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-first-function "test 4 (mixed)" '(2 2 3 3 1) '(1 2 2 3 3))
  (check-first-function "test 5 (empty)" nil nil)
  ;; Перевірка роботи ключових параметрів
  (format t "~:[FAILED~;passed~] test key length~%"
          (equal (selection-sort-func '((1 2 3) (1) (1 2)) :key #'length) 
                 '((1) (1 2) (1 2 3)))))

;;; -------- ВАРІАНТ 11 -----------

(defun merge-spinning-tuples-fn (&key (shift-step 1))
  (let ((counter 0)) ;; Змінна стану замикання
    (lambda (&rest args)
      (let* ((len (length args))
             (current-shift (* counter shift-step))
             ;; Обчислюємо ефективний зсув (mod len), щоб уникнути зайвих обертів
             (eff-shift (if (plusp len) (mod current-shift len) 0))
             ;; Виконуємо зсув вліво на eff-shift
             (result (if (zerop eff-shift)
                         args
                         (append (subseq args eff-shift) 
                                 (subseq args 0 eff-shift)))))
        
        ;; Оновлюємо стан для наступного кроку
        (setq counter (1+ counter))
        
        ;; Повертаємо результат
        result))))

;;; ----- ТЕСТИ ------

(defun check-second-function (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal input expected)
          name))

(defun test-second-function ()
  (format t "~%--- Testing Spinning Tuples (Variant 11) ---~%")
  
  (check-second-function "test 1 (default step)" 
                         (mapcar (merge-spinning-tuples-fn) 
                                 '(1 2 3) '(a b c)) 
                         '((1 A) (B 2) (3 C)))
  
  (check-second-function "test 2 (step 2)" 
                         (mapcar (merge-spinning-tuples-fn :shift-step 2) 
                                 '(a b c) '(1 2 3) '(d e f)) 
                         '((A 1 D) (E B 2) (3 F C))))


(defun run-all-tests ()
  (test-first-function)
  (test-second-function))

(run-all-tests)