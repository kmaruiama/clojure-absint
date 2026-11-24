(ns absint.helpers.tree-printer
  (:require [clojure.string]))

(defn print-recursively
  [node-list]
  (loop [buffer []
         node (assoc (first node-list) :level 0)]
    (print (-> :level
               node
               (* 5)
               (repeat " ")
               (clojure.string/join)))
    (print "└─")
    (print (str (:my-role node) "/" (:op node) "\n"))
    (let [new-buffer (into buffer
                           (->> node-list
                                (filter #(= (:parent %) (:id node)))
                                (map #(assoc % :level (inc (:level node))))))]
      (if (not-empty new-buffer)
        (recur (rest new-buffer) (first new-buffer))
        nil))))


