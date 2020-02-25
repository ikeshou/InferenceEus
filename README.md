# InferenceEus
(Incomplete, but useful) type inference system for [Euslisp](https://github.com/euslisp/EusLisp). This project is a part of [PyEus](https://github.com/ikeshou/PyEus) FFI project.
<br>
<br>

## :black_nib: Author
***
Ikezaki Shoya <ikezaki@csg.ci.i.u-tokyo.ac.jp>


## :computer: Platform and  Languages
***
InferenceEus supports both Linux and Mac OS X.


## :warning: Caution
***
<div id=caution>

The library is **developer version**. Be sure that supported built-in functions, methods, special forms, and macros are limited.

- Supported built-in functions: see the `buitin_type_data.l` file
- Supported built-in methods: None
- Supported built-in special forms: `and`, `or`, `defun`, `let`, `let*`  
- Supported built-in macros: None

If you want to see whole builti-in functions, methods, special forms, and macros, see the [Documentation of Euslisp (eng)](#http://euslisp.github.io/EusLisp/manual.html) or [Documentation of Euslisp (ja)](#http://euslisp.github.io/EusLisp/jmanual.html).

Note that this library do **not** guarantee the type safety at all. However, it suggests the possible types of arguments and returned value for Euslisp functions.
<br>
<br>


## Usage
***
```lisp
irteus "inference_eusfunc.l"

> (infer-file "sample_functions.l")    ; the path of your Euslisp file you want to infere here
```

`infer-file` function returns the hash-table (re-defined in `bugfixed_hash.l` since there are some bugs in the implementation of built-in hash-table) with which the association list of inferred types of returned value and arguments is registered. The format of association list is described as below:
```lisp
;;; note that each type information is expressed in a string notation, not a class object
'(returned-type (arg1-type arg2-type ...))
```

For example, let's assume that you want to infer the functions below that test whether the argument is the non-nil terminated list. The argument `dot-cons` is the argument of `cdr`, so `dot-cons` should be a list. The returned value of this function is the returned value of `listp` function, sot returned value should be a symbol (`t` or `nil`). 
```lisp
;;; "sample_functions.l"
(defun dot-cons-p (dot-cons)
  (listp (cdr dot-cons)))

; other fucntions here...
```

```lisp
(setq hsh (infer-file "sample_functions.l"))
(BUGFIXED-HASH::gethash 'dot-cons-p hsh)
; => ("SYMBOL" ("CONS"))
```
<br>

## What is it for?
***
With this library, my FFI library get to be able to know the type information of the foreign functions when user loaded the Euslisp file. Thanks to the type information of the foreign funcitons, my FFI library can detect some `NameError` and `TypeError` inside the foreign function calls from host language (in my case, Python) before calling the foreign function. It helps users to debug their FFI program.

<br>


## Additional Information for `infer-file` function
***
### 1. Class for nil and numbers, and union types

Though `nil` is defined as one of symbols in Euslisp, it is special (We can deal with `nil` as if it were cons class). It is defined as a special class in `inference_eusfunc.l`.
```lisp
(defclass nil-class :super object)
```

Since numbers are not objects in Euslisp, they are defined as classes in `inference_eusfunc.l`.
```lisp
(defclass number-class :super object)
(defclass integer-class :super number-class)
(defclass float-class :super number-class)
(defclass rational-class :super number-class) 
```

A lot of Euslisp functions takes multiple types such as cons and vector as an argument. For example, `elt` function takes cons or vector as a first argument. Since both cons and vector are thought as a sequence, abstract class, a special union class is defined.
```lisp
(defclass sequence-class :super object)    ; Union(cons, vector)
(defclass pathlike-class :super object)    ; Union(pathname, string)
(defclass symbollike-class :super object)    ; Union(symbol, string)
(defclass packagelike-class :super object)    ; Union(package, string)
```

### 2. Functions that need no argument
If you infer the function that needs no argument like below, the type information is described as `"NIL-CLASS"`.
```lisp
;;; sample_functions.l
(defun no-arg-ret-nil ()
  nil)
```
```lisp
(setq hsh (infer-file "sample_functions.l"))
(BUGFIXED-HASH::gethash 'no-arg-ret-nil hsh)
; => ("NIL-CLASS" ("NIL-CLASS"))
```

### 3. Functions that need a number or a union class as an argument
If you infer the function that needs a number as an argument like below, the type information is described as `"NUMBER-CLASS"` or `INTEGER-CLASS` or `FLOAT-CLASS` or `RATIONAL-CLASS`. Union class is also the same.
```lisp
(defun cube (num)
  (expt num 3))
```
```lisp
(setq hsh (infer-file "sample_functions.l"))
(BUGFIXED-HASH::gethash 'cube hsh)
; => ("NUMBER-CLASS" ("NUMBER-CLASS"))
```

### 4. Functions that need additional parameters (&optional, &rest, and &key)

Let's assume that you want to infer a Euslisp file that contains functions listed below:
```lisp
;;; sample_functions.l
(defun add-four (a b &optional c (d 4))
    (+ a b c d))

(defun sum-up-head2 (a &rest ls)
  (+ a (car ls)))

(defun key-lover (a &key b (c (list 1 2 3)))
   (+ (length a) (length b) (length c)))
```

The format of the association list is as follows. 
```lisp
(setq hsh (infer-file "sample_functions.l"))

(BUGFIXED-HASH::gethash 'add-four hsh)
; => ("NUMBER-CLASS" ("NUMBER-CLASS" "NUMBER-CLASS" (optional . "NUMBER-CLASS") (optional . "INTEGER-CLASS")))

(BUGFIXED-HASH::gethash 'sum-up-head2 hsh)
; => ("NUMBER-CLASS" ("NUMBER-CLASS" (rest)))


(BUGFIXED-HASH::gethash 'key-lover hsh)
; => ("NUMBER-CLASS" ("SEQUENCE-CLASS" (b . "SEQUENCE-CLASS") (c . "CONS")))
```

### 5. Limitations
Add to the limitations described [above](#caution), there are some tough problems in this program.

- My library cannot keep track on the type information after applying constructor functions such as `cons`. For example, though `my-function` is called with integers as arguments in the code below, my inference system can only know `my-function` is called with objects as arguments because the returned type of `car` and `cdr` can be every object.
```lisp
(let ((tmp (cons 1 2))) (my-function (car tmp) (cdr tmp)))
```
- If you set object whose type is different from the previous one to a variable, type inference fails. For example, the result of the code fragment below is `9` and no error occurrs. However, since my inference system formularize two incompatible type equations for `tmp` such as "tmp should be subtype of cons" and "tmp should be subtype of integer", the type inference fails.
```lisp
(let ((tmp '(1 2 3))) (setq tmp 4) (+ tmp 5))
```
<br>


## Algorithm
***
(Now writing)


## notes
***
This folder is migrated from ikeshou/CSG_research repository (private, for research use). Do not worry about the small number of commitment!<br>
Now implementing:
- add other special forms