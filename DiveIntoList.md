# Dive into *list*
Digging into list.


## Dot List
It is a list not ending with 'nil'.

Basic list:
```lisp
> (cons 1 (cons 2 (cons 3 nil)))
(1 2 3)
```

Without 'nil':
```lisp
> (cons 1 (cons 2 3))
(1 2 . 3)
```

What is 'dot'? 
```lisp
> '(1 . (2 . (3 . nil)))
(1 2 3)
Break 3 [13]> 
```

What now?
```lisp
> (cons 2 3)
(2 . 3)
```
In short, 'dot' of dot list is to express the last cons-cell.



## Pair

It is convenient to have a 'pair' of the data as a list.
```lisp
> (cons 2 3)
(2 . 3)
```
You can use `car` and `cdr` to easily obtain the record.



## Circular List
You can make a list that the last cell points to the first cell, by enabling `*print-circle*` cariable:

```lisp
> (setf *print-circle* t)
```
Then, create a list like:
```lisp
> (setf *print-circle* t)
T
> (defparameter foo (list 1 2 3))
FOO
> (setf (cdddr foo) foo)
#1=(1 2 3 . #1#)
```
NOTE: if you do not enable *print-circle*, you cannot execute the last command.


## Association List : `alist`
`alist` is a key-value pair list.

```list
> (defparameter *drink-order* '(
    (bill . double-espresso)
    (lisa . small-drip-coffee)
    (john . beer)
))
```

To obtain a record from the alist by using a key, use `assoc`:
```lisp
(assoc 'lisa *drink-order*)
```

To add and/or update records, use `push`:
```lisp
;; Adding:
> (push '(bob . milk) *drink-order*)
((BOB . MILK) (BILL . DOUBLE-ESPRESSO) (LISA . SMALL-DRIP-COFFEE) (JOHN . BEER))

;; Updating:
> (push '(bob . whiskey) *drink-order*)
((BOB . WHISKEY) (BOB . MILK) (BILL . DOUBLE-ESPRESSO) (LISA . SMALL-DRIP-COFFEE) (JOHN . BEER))
```

NOTE: A downside of `alist` is that it is not for large data.