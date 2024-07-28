(import ./janet-delims/janet-delims :as delims)
(import ./janet-peg/janet-peg/bounds :as bnds)

(defn deprintf
  [fmt & args]
  (when (os/getenv "VERBOSE")
    (eprintf fmt ;args)))

(defn ast
  [code]
  (->> code
       (peg/match bnds/bounds-grammar)
       first))

(comment

  (ast ";[1 2]")
  # =>
  '(:splice
     (:bracket-tuple
       (:number "1" 1 3 1 4) (:whitespace " " 1 4 1 5)
       (:number "2" 1 5 1 6)
       1 2 1 7)
     1 1 1 7)

  )

(defn node-type
  [node]
  (first node))

(defn span
  [node]
  (slice node -5))

(defn content
  [node]
  (slice node 1 -5))

(defn spans?
  [[s-line-a s-col-a e-line-a e-col-a]
   [s-line-b s-col-b e-line-b e-col-b]]
  #
  (defn before?
    [line-a col-a line-b col-b]
    (cond
      (< line-a line-b)
      true
      #
      (= line-a line-b)
      (<= col-a col-b)
      #
      false))
  #
  (and (before? s-line-a s-col-a
                s-line-b s-col-b)
       (before? e-line-b e-col-b
                e-line-a e-col-a)))

(comment

  (spans? [1 2 1 3]
          [1 2 1 2])
  # =>
  true

  (spans? [1 1 2 2]
          [1 2 1 2])
  # =>
  true

  (spans? [4 15 5 33]
          [4 37 4 37])
  # =>
  true

  )

# find all, then find smallest
(defn find-bounds
  [tree target-span]
  (def ctxt @[tree])
  (defn helper
    [node]
    (when (= :tuple (type node))
      (def inner (content node))
      #
      (each item inner
        (when (= :tuple (type item))
          (when (not= :whitespace
                      (node-type item))
            (def i-span (span item))
            (deprintf "item: %p" item)
            (deprintf "span: %p" i-span)
            (when (spans? i-span target-span)
              (array/push ctxt item))))
        (helper item))))
  (helper tree)
  # XXX: is this sensible? if this always works, just keep
  #      overwriting above instead of accumulating in array?
  # XXX
  (deprintf "ctxt: %p" ctxt)
  (last ctxt))

(defn calc-bounds-helper!
  [input-lines delims target-span]
  # if there are any missing delimiters add them as a new line
  (when (not (empty? delims))
    (array/push input-lines (string/join delims)))
  (def new-region
    (string/join input-lines "\n"))
  # XXX
  (deprintf "new region: %p" new-region)
  (def tree
    (ast new-region))
  #
  (def node (find-bounds tree target-span))
  (deprintf "bounds node: %p" node)
  (when (indexed? node)
    (span node)))

(defn calc-bounds
  # XXX: pass in span
  [fragment line column]
  (def delims
    (delims/closing-delims fragment))
  # missing-delims had a problem (e.g. too many closing delimiters)
  (when (nil? delims)
    (break -2))
  #
  (def input-lines
    (string/split "\n" fragment))
  # XXX
  (def target-span
    [line column
     line column])
  # XXX
  (deprintf "target span: %p" target-span)
  (calc-bounds-helper! input-lines delims target-span))

(defn main
  [& args]
  (def input
    (file/read stdin :all))
  (def pieces
    (string/split "\n\n" input 0 2))
  #
  (def header-lines
    (string/split "\n" (first pieces)))
  (var line nil)
  (var col nil)
  # XXX: extend to work with region
  (each hl header-lines
    (when-let [[num]
               (peg/match ~(sequence "Column: "
                                     (capture :d+)
                                     -1)
                          hl)]
      (set col (scan-number num)))
    (when-let [[num]
               (peg/match ~(sequence "Line: "
                                     (capture :d+)
                                     -1)
                          hl)]
      (set line (scan-number num))))
  (when (or (nil? line)
            (nil? col))
    (deprintf "failed to find line and/or column in header lines")
    (print "-1")
    (os/exit 0))
  #
  (def fragment
    (get pieces 1))
  (def bounds
    # XXX: pass in span
    (calc-bounds fragment line col))
  (deprintf "bounds: %p" bounds)
  (cond
    (indexed? bounds)
    (printf "%d %d %d %d" ;bounds)
    #
    (neg? bounds)
    (print bounds)
    #
    (print -1)))

