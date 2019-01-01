
Note for Common Lisp
======================

# Reference
- Land of Lisp (Book)
- [Simplified Common Lisp Reference](http://jtra.cz/stuff/lisp/sclr/index.html)

# Basic
- Boot REPL:  `> clisp`
- Shut it down: `[x]> (exit)  or (quit)`
- load file in REPL: ` [x]> (load 'FILE-NAME.lisp)` 
    - you can omit `.lisp` extension
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
[See](https://stackoverflow.com/questions/9105870/whats-the-difference-between-flet-and-labels-in-common-lisp):

In Common Lisp, both flet and labels make lexical bindings of slightly different sorts:

- flet is a special form for local function binding.   
  Bindings are not recursive and **cannot** refer to each other. Each binding contains function name, arguments, and function body.
- labels is a special form for local function binding.   
  Bindings can be recursive and **can** refer to each other. Each binding contains function name, arguments, and function body.





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
  - list consists of connected cons-cells
  - 'nil' at the end of the list (cell)
  - Functions to handle list: cons, car, cdr, etc.

- Conditions: `if`, `when`, `cond`, etc.
 


# What is *list*
 [See next page](DiveIntoList.md)



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
To connect two lists into one.
```lisp 
> (cons 'chicken 'pork) 
(CHICKEN . PORK)
```
```lisp
> (cons 'chicken nil) 
(CHIKEN)
;; (cons 'chicken ()) is the same.
```  
```lisp
> (cons 1 (cons 2 (cons 3 nil)))
(1 2 3)
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


## `mapc`
Variation of `mapcar`; it does not return a result list.


## `maplist`
Similar to `mapcar` but it seeks all of the list. Comparing it with `mapcar`;

e.g. `mapcar`:
```lisp
> (mapcar #'print '(a b c))
A
B
C
```
e.g. `maplist`:
```lisp
> (maplist #'print '(a b c))
(A B C)
(B C)
(C)
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



## 'Lambda'

By using *lambda* function/capability, you can make a function without declaring it.  
(Atually *lambda* is not a function but a *macro*.)

Usually when you define a function, it goes like this:
```lisp
> (defun half(n)
    (/ n 2))
> (half 4)
2
> #'half
#<FUNCTION HALF (N) (DECLARE (SYSTEM::IN-DEFUN HALF)) (BLOCK HALF (/ N 2))>
...
```
NOTE: `#'` is to obtain function itself.

*Lambda* is to do this at once. It is a capability to define function Without the function name.
```lisp
> (lambda (n) (/ n 2))
#<FUNCTION :LAMBDA (N) (/ N 2)>
```

So, you can use it like:
```lisp
> (mapcar (lambda (n) (/ n 2)) 
          '(2 4 6 8 10))
(1 2 3 4 5)
```


## `substitute-if`
Replace the value with the specified character according to the condition matches.
```lisp
> (substitute-if 0 
        #'oddp 
        '(1 2 3 4 5 6 7))
(0 2 0 4 0 6 0)
```
```lisp
> (substitute-if #\e 
        #'digit-char-p 
        "I'm a l33t hark3r!")
"I'm a leet harker!"
```

## `complement`
Function to do reverse the result of a function.
e.g. To get characters that are not alphabet nor numbers
```lisp
(complement #'alphanumericp)
```

## `with-open-file`
A function to write a file.
e.g.
```lisp
(with-open-file 
    (my-stream "./testfile.txt"
        :direction :output
        :if-exists :supersede)
    (princ "Hello file" my-stream) )
```
Structure:
```lisp
(with-open-file 
  (my-stream ...)
  (... describe the contents here ...)
)
```
### Keyword variables for with-open-file (Stream)
- `:direction :output` : open a file for output (not input)
- `:if-exists :supersede` : overwrite the contents if there is already a file

**NOTE**: the colon is a *keyword symbol*. It is a 'symbol' itself, no other than that.  
    e.g.   
    ```lisp
    > :cigar
    :CIGAR
    > (let ((:cigar 5)) :cigar)
    *** - LET: :CIGAR is a constant, may not be used as a variable
    ```

