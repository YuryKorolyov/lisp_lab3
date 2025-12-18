<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>:Корольов Юрій Ігорович, група КВ-23</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і імперативно.

1. Функціональний варіант реалізації має базуватись на використанні рекурсії і конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного списку. Не допускається використання: деструктивних операцій, циклів, функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і деструктивних функцій (псевдофункцій). Не допускається використання функцій вищого порядку або функцій для роботи зі списками/послідовностями, що використовуються як функції вищого порядку. Тим не менш, оригінальний список цей варіант реалізації також не має змінювати, тому перед виконанням деструктивних змін варто застосувати функцію copy-list (в разі необхідності). Також реалізована функція не має бути функціоналом (тобто приймати на вхід функції в якості аргументів).


## Варіант 11 % 5 = 1. Алгоритм сортування вибором за незменшенням.
1. Написати функцію `remove-thirds`, яка видаляє зі списку кожен третій елемент.
2. Написати функцію `list-set-union-3`, яка визначає об'єднання трьох множин, заданих списками атомів.

## Лістинг програми
```lisp
;; Допоміжна функція для пошуку мінімального елемента
(defun get-min (lst)
  (cond ((null (cdr lst)) (car lst)) ;; Якщо один елемент, він і є мінімум
        (t (let ((min-rest (get-min (cdr lst))))
             (if (<= (car lst) min-rest)
                 (car lst)
                 min-rest)))))

;; Допоміжна функція для видалення першого входження елемента
;; (потрібна, щоб передати решту списку далі)
(defun remove-one (item lst)
  (cond ((null lst) nil)
        ((= item (car lst)) (cdr lst)) ;; Знайшли - пропускаємо (видаляємо) і повертаємо хвіст
        (t (cons (car lst) (remove-one item (cdr lst)))))) ;; Інакше будуємо список далі

;; Основна функція сортування вибором (функціональна)
(defun selection-sort-func (lst)
  (cond ((null lst) nil)
        (t (let ((min-val (get-min lst)))
             (cons min-val 
                   (selection-sort-func (remove-one min-val lst)))))))

;; --- ТЕСТИ ДЛЯ ФУНКЦІОНАЛЬНОГО ВАРІАНТУ ---

(defun check-first-function (name input expected)
  "Execute `selection-sort-func' on `input', compare result with `expected' and print status"
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (selection-sort-func input) expected)
          name))

;; Основна функція сортування вибором (імперативна)
(defun selection-sort-imper (lst)
  (let ((result (copy-list lst))    ;; Копіюємо список, щоб не змінювати оригінал
        (len (length lst)))
    
    ;; Зовнішній цикл: i від 0 до length-2
    (dotimes (i (1- len))
      (let ((min-idx i))
        
        ;; Внутрішній цикл: j від i+1 до length-1
        (do ((j (1+ i) (1+ j)))
            ((= j len)) ;; Умова виходу
          ;; Якщо поточний елемент менший за запам'ятований мінімум
          (when (< (nth j result) (nth min-idx result))
            (setf min-idx j)))
        
        ;; Обмін елементів (Swap), якщо знайшли новий мінімум
        (when (/= min-idx i)
          (let ((temp (nth i result)))
            (setf (nth i result) (nth min-idx result))
            (setf (nth min-idx result) temp)))))
    
    result))

;; --- ТЕСТИ ДЛЯ ІМПЕРАТИВНОГО ВАРІАНТУ ---

(defun check-second-function (name input expected)
  "Execute `selection-sort-imper' on `input', compare result with `expected' and print status"
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (selection-sort-imper input) expected)
          name))

(defun test-second-function ()
  (check-second-function "test 1" '(5 3 4 1 2) '(1 2 3 4 5))  
  (check-second-function "test 2" '(1 2 3 4 5) '(1 2 3 4 5)) 
  (check-second-function "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-second-function "test 4" '(2 2 3 3 1) '(1 2 2 3 3))
  (check-second-function "test 5" nil nil))


(defun test-first-function ()
  (check-first-function "test 1" '(5 3 4 1 2) '(1 2 3 4 5))  
  (check-first-function "test 2" '(1 2 3 4 5) '(1 2 3 4 5)) 
  (check-first-function "test 3" '(1 1 1 1 1) '(1 1 1 1 1))
  (check-first-function "test 4" '(2 2 3 3 1) '(1 2 2 3 3))
  (check-first-function "test 5" nil nil))
```
## Результати тестування
```lisp
CL-USER> (test-first-function)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
NIL
CL-USER> (test-second-function)
passed test 1
passed test 2
passed test 3
passed test 4
passed test 5
NIL
CL-USER
```
