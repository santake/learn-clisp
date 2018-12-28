
Note for Common Lisp
======================

# Reference
- Land of Lisp (Book)
- [Simplified Common Lisp Reference](http://jtra.cz/stuff/lisp/sclr/index.html)

# Basic
- Boot REPL:  `> clisp`
- Shut it down: `[x]> (exit)  or (quit)`
- load file in REPL: ` [x]> (load 'FILE-NAME.lisp)`
- Exit REPL: `[x]> (exit)` or `(quit)`
 
# Words
 - clisp : the command to boot common lisp
 - REPL: Read-eval-event-loop ... command line when you enter 'clisp'
 - alist: Association List (連想配列)
 - cons-cell: fundamental data structure of lisp


# Grammer / Syntax

## Function: `defun`
 ```lisp
(defun square(val)
    (* val val) )
```

## Global variables: `defparameter`, `defvar`
```lisp
;; you can overwrite the value:
(defparameter *foo* 3)
;; you canNOT overwrite it:
(defvar *bar* 3)
```
example of `defvar`:
```lisp
> (defvar *b* 5)
*B*
> *b*
5
> (defvar *b* 2)
*B*
> *b*
5
```
**NOTE1:** Use *earmuffs* (`*`) to define global variables (actually you can define it without earmuffs...).

**NOTE2:** You *CAN* overwrite the value by using `setf`, even though you define it with `defvar`.


## Local variables: `let`
```lisp
(let ((a 5) (b 1))
    (+ a b) )
```

## Local functions: `flet`
```lisp
(flet ((func(n)
          (+ n 10)))
      (func 5) )
```

## `labels`

TBD






# Rules

- everything is 'list' = (xxx yyy zzz)

- Upper/lower case does not matter
 almost of all use lower case though

   - To use upper/lower case in characters, enclose it with `|`:
        e.g. 
        ```lisp
        > (print '|Hanage|)
        |Hanage|
        ```
        (But you will get the result with pipe...)


- Code mode & Data mode
  - Code mode: `(command val val val)`
  - Data mode:  `'(a b c)`  
    - Put *single quote* at the top of the list.   
    (This is to let Lisp not to evaluate the list.)
  - Code in Data mode: `` `(value ,(code) value)`` 
      - not single-quote(`'`) but backquote (`` ` ``)
      - `,(code)` is the mixed in 
  
- Data Structure: 'Cons'-cell
  - 'nil' at the end of the list (cell)
  - Functions to handle list: cons, car, cdr, etc.

- Conditions: `if`, `when`, `cond`, etc.
 

# Functions

## `write`, `print`, `prin1`, `princ`

Thanks to [Stakoverflow](https://stackoverflow.com/questions/19756296/whats-the-difference-between-write-print-pprint-princ-and-prin1):

- `write` is the general entry point to the Lisp printer.
- `prin1` produces output suitable for input to read.
- `princ` is just like prin1 except that the output has no escape characters. princ is intended to look good to people, while output from prin1 is intended to be acceptable to read.
  - for human
- `print` is just like prin1 except that the printed representation of object is preceded by a newline and followed by a space.
  - for computers
- `pprint` produces pretty output.
e.g. :
```lisp
[]> (write 'hanage)
HANAGE
HANAGE
[]> (prin1 'hanage)
HANAGE
HANAGE
[]> (princ 'hanage)
HANAGE
HANAGE
[]> (print 'hanage)

HANAGE 
HANAGE
[]> (pprint 'hanage)

HANAGE

[]> ...
```

e.g. `print`:
```lisp
>(defun sayhello0()
    (print "Enter your name:")
    (let ((name (read)))
        (print "Nice to meet you, ")
        (print name)))
>(sayhello0)
"Enter your name:" Hanage

"Nice to meet you, " 
HANAGE 
```
e.g. `princ`:
```lisp
>(defun sayhello1()
    (princ "Enter your name:")
    (let ((name (read)))
        (princ "Nice to meet you, ")
        (princ name)))
>(sayhello1)
Enter your name: Hanage
Nice to meet you, HANAGE
```

e.g. Simply:
```lisp
>(progn 
    (princ "This sentence will be interrupted")
    (princ #\newline)
    (princ "by an annoying newline character.") )
This sentence will be interrupted
by an annoying newline character.

>(progn 
    (print "This sentence will be interrupted")
    (print #\newline)
    (print "by an annoying newline character.") )    
"This sentence will be interrupted" 
#\Newline 
"by an annoying newline character." 
```









## `cons`
To connect two data into one.
```lisp 
> (cons 'chicken 'pork') 
(CHICKEN . PORK)
```
```lisp
> (cons 'chicken nil) 
(CHIKEN)
;; (cons 'chicken ()) is the same.
```  

## `list`
To create list, simply.
```lisp
> (list 'pork 'chiken 'beef)
(PORK CHICKEN BEEF)
```

## `car`
To get the first slot from a list.
```lisp
> (car '(pork chicken beef))
PORK
```

## `cdr`
To get the second slot of the list = rest of the list, **not the second record!**
```lisp
> (cdr '(pork chicken beef))
(CHIKEN BEEF)
```
(see below also)

## `cadr`
To get the second **record** from the list.
```lisp
(cadr '(pork chicken beef))
CHICKEN
```
NOTE: this is equivalent to:
```lisp
(car (cdr '(pork chicken beef)))
```

## `caddr`, `cadddr`
To get the 3rd, and 4th record from the list. (There is no 'caddddr' function though).
```lisp
> (car '(a b c d e f g h))
A
> (cadr '(a b c d e f g h))
B
> (caddr '(a b c d e f g h))
C
> (cadddr '(a b c d e f g h))
D
> (caddddr '(a b c d e f g h))
***ERROR***
```


## `cddr`,`cdddr`, `cddddr` 
To get the 3rd, 4th, and 5th slot from the list.
```lisp
> (cdr '(a b c d e f g h))
(B C D E F G H)
> (cddr '(a b c d e f g h))
(C D E F G H)
> (cdddr '(a b c d e f g h))
(D E F G H)
> (cddddr '(a b c d e f g h))
(E F G H)
> (cdddddr '(a b c d e f g h))
***ERROR***
```


## `equal` functions:

### you should remember:
1) `=`: to compare numbers
2) `eq`: to compare symbols (characters)
3) `equal`: to compare any other than symbols (e.g. arrays, numbers)
   
### nice to have:
4) `eql`: to compare symbols, numbers, characters ... almost same as equal (?)
5) `equalp`: to compare 
    - string with ignoring Upper/lower-case
    - different number types (int and float) ...???
    ```lisp
    (equal "Abc" "abc") => nil
    (equalp "Abc" "abc") => T
    ```
6) `string-equal`: for string
7) `char-equal`: for character

 
## `mapcar`
To apply a function to each item from the list. The size of the list will not change.

```lisp
> (mapcar #'sqrt '(1 2 3 4))
(1 1.4142135 1.7320508 2)
```

NOTE: `#'` is the abbreviation of the function operator.
```lisp
> (mapcar #'car '((foo bar) (baz fob)))
(FOO BAZ)
> (mapcar (function car) '((foo bar) (baz fob)))
(FOO BAZ)
```

## `progn` (special operator) 
PROGN calls its expression in the order they have been written. 
Resulting value is the value of the last form unless non-local 
control flow forced earlier return. See also PROG1, PROG2.
[See this in detail](http://jtra.cz/stuff/lisp/sclr/progn.html).

Simply, you can put an expression into a list (and returns the result of the last evaluation).
e.g.
```lisp
> (defvar *numisodd* nil)
> (if (oddp 5)
    (progn (setf *numisodd* t)
            'odd-number)
    'even-number)
==> ODD-NUMBER
```



## `eval`
The book says it is powerful but dangerous.
You have to avoid using them as possible as you can because
it can execute 'malicious' codes from outside.

```lisp
> (defparameter *foo* '(+ 1 2))
*FOO*
> *foo*
(+ 1 2)
> (eval *foo*)
3
```

* Also you should be careful with 'reader macro', which is `#.(function)`.
  * To avoid this, set global variable `*read-eval*` with `nil`.


