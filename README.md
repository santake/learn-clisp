
Note for Common Lisp
======================

# Reference
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
**NOTE:** Use *earmuffs* (`*`) to define global variables (actually you can define it without earmuffs...).


## Local variable: `let`
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


# Rules

- everything is 'list' = (xxx yyy zzz)

- Upper/lower case does not matter
 almost of all use lower case though

- Code mode & Data mode
  - Code mode: `(command val val val)`
  - Data mode:  `'(a b c)`  
    Put *single quote* at the top of the list.   
    (This is to let Lisp not to evaluate the list.)
  
- Data Structure: 'Cons'-cell
  - 'nil' at the end of the list (cell)
  - Functions to handle list: cons, car, cdr, etc.

- Conditions: `if`, `when`, `cond`, etc.
 

# Functions

## `print`, `prin1`, `princ`

TBD


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

### Mandatory:
1) `=`: to compare numbers
2) `eq`: to compare symbols (characters)
3) `equal`: to compare any other than symbols (e.g. arrays, numbers)
   
### Nice to have:
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
To apply function for each item in the list.
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

[See](http://jtra.cz/stuff/lisp/sclr/progn.html)





