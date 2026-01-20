<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Корольов Юрій Ігорович КВ-23</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання

1.  **Рефакторинг алгоритму сортування:** Переписати функціональну реалізацію алгоритму сортування з попередньої лабораторної роботи, використовуючи функції вищого порядку (HOF) для роботи з послідовностями. Додати підтримку ключових параметрів `key` та `test`, забезпечивши мінімальну кількість викликів функції `key` (кешування значення ключа).
2.  **Робота із замиканнями:** Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом (обробка списків за допомогою `mapcar` зі збереженням стану між ітераціями).

# Частина 1.
**Алгоритм сортування вибором (Selection Sort).**

## Лістинг реалізації
```lisp
(defun extract-min (lst key test)
  "Знаходить мінімальний елемент і повертає:
   1. Саме значення мінімального елемента.
   2. Список без першого входження цього елемента."
  (if (null lst)
      (values nil nil)
      ;; Використовуємо reduce для пошуку мінімуму за один прохід
      ;; Акумулятор зберігає пару (best-element . best-key)
      (let* ((min-pair (reduce (lambda (best current)
                                 (let ((best-val (car best))
                                       (best-key (cdr best))
                                       (curr-key (funcall key current)))
                                   ;; Порівнюємо ключі, використовуючи test
                                   (if (funcall test curr-key best-key)
                                       (cons current curr-key) ;; Новий мінімум
                                       best)))                 ;; Залишаємо поточний
                               (rest lst)
                               ;; Початкове значення: перший елемент та його ключ
                               :initial-value (cons (first lst) (funcall key (first lst)))))
             (min-val (car min-pair)))
        
        ;; Повертаємо два значення: знайдений елемент та список без нього
        ;; :count 1 гарантує видалення лише однієї копії
        (values min-val (remove min-val lst :count 1)))))

(defun selection-sort-func (lst &key (key #'identity) (test #'<))
  "Рекурсивна функція сортування вибором з використанням HOF."
  (cond
    ((null lst) nil)
    (t 
     ;; Використовуємо multiple-value-bind для отримання результатів extract-min
     (multiple-value-bind (min-val rest-lst) (extract-min lst key test)
       (cons min-val 
             (selection-sort-func rest-lst :key key :test test))))))
```

## Лістинг тестових утиліт
```lisp
(defun check-first-function (name input expected)
  "Виконує selection-sort-func на input, порівнює з expected і друкує статус."
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (selection-sort-func input) expected)
          name))

(defun test-first-function ()
  (format t "~%--- Testing Selection Sort ---~%")
  (check-first-function "test 1 (simple numbers)" '(5 3 4 1 2) '(1 2 3 4 5))  
  (check-first-function "test 2 (already sorted)" '(1 2 3 4 5) '(1 2 3 4 5)) 
  (check-first-function "test 3 (duplicates)" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-first-function "test 4 (mixed duplicates)" '(2 2 3 3 1) '(1 2 2 3 3))
  (check-first-function "test 5 (empty)" nil nil)
  
  ;; Перевірка параметрів :key та :test
  ;; Сортування списків за їх довжиною
  (format t "~:[FAILED~;passed~] test key (length)~%"
          (equal (selection-sort-func '((1 2 3) (1) (1 2)) :key #'length) 
                 '((1) (1 2) (1 2 3))))
  ;; Сортування за спаданням
  (format t "~:[FAILED~;passed~] test test (>)"
        (equal (selection-sort-func '(1 5 2 4) :test #'>) 
               '(5 4 2 1))))
```
## Тестування

;;; Результат виконання (stdout):

```
--- Testing Selection Sort ---
passed test 1 (simple numbers)
passed test 2 (already sorted)
passed test 3 (duplicates)
passed test 4 (mixed duplicates)
passed test 5 (empty)
passed test key (length)
passed test test (>)
```

# Частина 2. Варіант 11

## Завдання
Написати функцію merge-spinning-tuples-fn, яка повертає замикання. Це замикання має приймати аргументи (елементи списків) через mapcar, об'єднувати їх у список та виконувати циклічний зсув елементів вліво. Величина зсуву залежить від номера ітерації (індексу) та параметра shift-step: shift = index * shift-step.

## Лістинг реалізації

```lisp
(defun merge-spinning-tuples-fn (&key (shift-step 1))
  "Повертає функцію-замикання, яка об'єднує аргументи в список
   і виконує циклічний зсув, що залежить від кількості викликів (ітерації)."
  (let ((counter 0)) ;; Змінна стану замикання (лічильник ітерацій)
    (lambda (&rest args)
      (let* ((len (length args))
             (current-shift (* counter shift-step))
             ;; Обчислюємо ефективний зсув (mod len), щоб уникнути зайвих обертів
             (eff-shift (if (plusp len) (mod current-shift len) 0))
             ;; Виконуємо зсув вліво на eff-shift: (rest... + start...)
             (result (if (zerop eff-shift)
                         args
                         (append (subseq args eff-shift) 
                                 (subseq args 0 eff-shift)))))
        
        ;; Оновлюємо стан для наступного виклику
        (setq counter (1+ counter))
        
        ;; Повертаємо результат
        result))))
```

## Тестові набори

```lisp
(defun check-second-function (name input expected)
  "Порівнює результат виконання із очікуваним."
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal input expected)
          name))

(defun test-second-function ()
  (format t "~%--- Testing Spinning Tuples (Variant 11) ---~%")
  
  ;; Приклад 1: крок 1 (за замовчуванням)
  ;; Iter 0: (1 a) -> зсув 0 -> (1 a)
  ;; Iter 1: (2 b) -> зсув 1 -> (b 2)
  ;; Iter 2: (3 c) -> зсув 2 (mod 2 = 0) -> (3 c)
  (check-second-function "test 1 (default step)" 
                         (mapcar (merge-spinning-tuples-fn) 
                                 '(1 2 3) '(a b c)) 
                         '((1 A) (B 2) (3 C)))
  
  ;; Приклад 2: крок 2
  ;; Iter 0: (a 1 d) -> зсув 0 -> (a 1 d)
  ;; Iter 1: (b 2 e) -> зсув 2 -> (e b 2)
  ;; Iter 2: (c 3 f) -> зсув 4 (mod 3 = 1) -> (3 f c)
  (check-second-function "test 2 (step 2)" 
                         (mapcar (merge-spinning-tuples-fn :shift-step 2) 
                                 '(a b c) '(1 2 3) '(d e f)) 
                         '((A 1 D) (E B 2) (3 F C))))
```

## Тестування
```
;;; Результат виконання (stdout):

--- Testing Spinning Tuples (Variant 11) ---
passed test 1 (default step)
passed test 2 (step 2)
```


