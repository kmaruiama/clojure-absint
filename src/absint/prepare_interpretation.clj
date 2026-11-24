(ns absint.prepare-interpretation
  (:require [clojure.tools.analyzer.ast :as ast]
            [clojure.tools.analyzer.jvm :as ana]
            [absint.domain-maps :as fields])
  (:import (java.util UUID)))

(defn positioned-children
  "takes a node and returns its children, with the extra
   search fields added. For children that are inside a vector
   also adds a position.
   ex: :then inside :if => single node => no position
       :bindings inside let => multi node => position"
  [node]
  (mapcat (fn [[role children]]
            (if (vector? children)
              (map-indexed (fn [index child]
                             (assoc child
                               :parent (:id node)
                               :my-position index
                               :my-role role))
                           children)
              [(assoc children
                 :parent (:id node)
                 :my-role role)]))
    (ast/children* node)))

(defn clean-up-node
  [node]
  (let [op (:op node)
        final-fields (into (get fields/ast-fields op) fields/always-present)]
    (as-> node n
      (select-keys n final-fields)
      (apply dissoc n (:children node)))))

(defn clean-up-tree
  "takes the root node of the s-expression, and performs a BFS
   over the tree selecting the useful parameters of each node"
  [root]
  (loop [current root
         cleaning-queue []
         cleaned-queue []]
    (let [identified-node (assoc current :id (UUID/randomUUID))
          next-cleaning (into cleaning-queue
                              (positioned-children identified-node))
          next-clean (conj cleaned-queue (clean-up-node identified-node))]
      (if (empty? next-cleaning)
        next-clean
        (recur (first next-cleaning) (subvec next-cleaning 1) next-clean)))))

(defn form-to-cleansed-list
  "Transforms a form into a flat vector of maps.
   to change the parameters of what will be filtered for each
   AST node, add or remove items in absint.domain-maps/ast-fields.
   the returned vector of maps is flattened, and the parent-child
   relationships between nodes are represented by keys in those
   maps"
  [code]
  (->> [code]
       (mapv ana/analyze)
       (mapv clean-up-tree)
       (flatten)
       (vec)))

(defn index-by-parent-and-role
  "Builds the map used by the interpreter to discover children by
   their parent, keys by both role and father, and in case of
   multiple children under the same role (bindings, for instance)
   sorts by position in the vector"
  [nodes]
  (let [unsorted (group-by (juxt :parent :my-role) nodes)
        sorted (update-vals unsorted #(sort-by :my-position %))]
    sorted))
