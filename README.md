# janet-bounds

Code for determining bounds of things in strings of Janet source code.

## Demo

Given the file `data/bounds-input-symbol.txt` with content:

```
Line: 1
Column: 9

(defn my-fun
  [x y]
  (+ x (* 2 y)))
```

The bounds of what is at line 1, column 9 can be determined by:

```
cat data/bounds-input-symbol.txt | janet janet-bounds/bounds.janet
```

The result should be:

```
1 7 1 13
```

which corresponds to the bounds of the symbol `my-fun`, which starts
at line 1, column 7 and extends to line 1, column 13.

## Use

The code is currently used via Emacs Lisp code in
[janet-editor-elf](https://github.com/sogaiu/janet-editor-elf) in two
ways.  One is directly via process invocation and another is
indirectly as library code via a Janet program for "wrapping" existing
forms.

