(ns user
  (:require [absint.concrete-interpreter :as coni]
            [absint.prepare-interpretation :as prei]
            [absint.helpers.tree-printer :as tp]))

(defn print-tree
  "prints a single s-expr"
  [code]
  (tp/print-recursively (prei/form-to-cleansed-list code)))

(defn con-interpret
  "interprets a single s-expr concretely
   ex:
   (con-interpret '(let [a 1] (+ a 1)))
   => 2"
  [code]
  (let [nodes (prei/form-to-cleansed-list code)
        root (nth nodes 0)
        parent-child-lookup (prei/index-by-parent-and-role nodes)]
    (coni/interpret parent-child-lookup root {})))

; (defn abs-interpret
;   "interprets a single s-expr abstractly
;   [MISSING EXAMPLE GOES HERE]"
;   [code]
;   (let [nodes (prei/form-to-cleansed-list code)
;         root (nth nodes 0)
;         parent-child-lookup (prei/index-by-parent-and-role nodes)]
;    (absi/interpret parent-child-lookup root {})))