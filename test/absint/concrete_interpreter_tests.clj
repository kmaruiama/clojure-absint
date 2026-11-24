(ns absint.concrete-interpreter-tests
  " @llm "
  (:require [clojure.test :refer :all]
            [absint.concrete-interpreter :as coni]
            [absint.prepare-interpretation :as prei]))

(defn run
  " @llm "
  [code]
  (let [nodes (prei/form-to-cleansed-list code)]
    (coni/interpret (prei/index-by-parent-and-role nodes) (first nodes) {})))

(defn check
  " @llm compares each form's result in real Clojure and in the interpreter"
  [forms]
  (doseq [form forms]
    (testing (pr-str form)
      (is (= (eval form) (run form))))))

(deftest constants
  (check '[1 "s" :k nil true \c
           (quote x)
           (quote [a b])
           (quote {:a (b c)})]))

; DEPENDS ON [constants]
(deftest calls-and-interop
  (check '[(+ 1 2)
           (str "a" "b")
           (Math/abs -1)
           Math/PI
           (String. "abc")
           (.substring "abcdef" 1 3)
           (.length "abc")
           (instance? String "a")
           (instance? Long "a")
           (map String/.length ["ab" "c"])
           (map Math/abs [-1 -2])]))

; DEPENDS ON [constants calls-and-interop]
(deftest collections
  (check '[[1 2]
           #{1 2}
           {:a 1}
           [(+ 1 2) (* 2 3)]
           {:a (+ 1 1)}
           (:k {:k 1})
           (meta ^:m [1])
           (meta ^{:m (+ 1 1)} [1])]))

; DEPENDS ON [constants calls-and-interop]
(deftest do-and-if
  (check '[(do 1 2 3)
           (do)
           (if true :yes :no)
           (if nil :yes :no)
           (if (< 1 2) :yes :no)
           (if false :yes)]))

; DEPENDS ON [constants calls-and-interop do-and-if]
(deftest let-and-locals
  (check '[(let [a 1] a)
           (let [a 1 b (+ a 1)] [a b])
           (let [a 1 a (+ a 1)] a)
           (let [a 1] (let [b 2] (+ a b)))
           (let [x nil] (if x :yes :no))]))

; DEPENDS ON [constants let-and-locals]
(deftest case-family
  (check '[(case 2 1 :one 2 :two :other)
           (case 9 1 :one 2 :two :other)
           (case :b :a 1 :b 2 3)
           (case "x" "x" :str :none)
           (case 3 (1 2) :low (3 4) :high :none)
           (case "BB" "Aa" :a "BB" :b :none)]))

; DEPENDS ON [constants calls-and-interop do-and-if let-and-locals]
(deftest fns
  (check '[((fn [] 1))
           ((fn [x] x) 1)
           ((fn [x y] (+ x y)) 1 2)
           ((fn ([x] x) ([x y] (+ x y))) 1 2)
           ((fn [x & r] [x r]) 1 2 3)
           ((fn [& r] r))
           ((fn f [n] (if (zero? n) 0 (+ n (f (dec n))))) 4)
           (let [a 10] ((fn [x] (+ a x)) 1))]))

; DEPENDS ON [constants calls-and-interop collections do-and-if let-and-locals fns]
(deftest loop-and-recur
  (check '[(loop [i 0] (if (< i 3) (recur (inc i)) i))
           (loop [i 0 acc []] (if (< i 3) (recur (inc i) (conj acc i)) acc))
           (let [x 10] (loop [i 0] (if (< i x) (recur (+ i 5)) i)))
           ((fn [n] (if (zero? n) :done (recur (dec n)))) 3)]))

; DEPENDS ON [constants calls-and-interop do-and-if let-and-locals fns]
(deftest letfns
  (check '[(letfn [(f [x] x)] (f 1))
           (letfn [(ev? [n] (if (zero? n) true (od? (dec n))))
                   (od? [n] (if (zero? n) false (ev? (dec n))))]
             (ev? 4))]))

; DEPENDS ON [constants calls-and-interop let-and-locals]
(deftest try-catch
  (check '[(try 1)
           (try (throw (Exception. "boom")) (catch Exception e (.getMessage e)))
           (try (throw (Exception. "boom"))
                (catch IllegalStateException e :wrong)
                (catch Exception e :right))
           (try 1 (finally 2))]))

; I asked the LLM to separate the families in such way that more complex forms
; that depend on the primitive ones go last, that way one can guarantee that the
; problem is in the form itself instead of being propagated
(defn test-ns-hook
  " @llm "
  []
  (constants)
  (calls-and-interop)
  (collections)
  (do-and-if)
  (let-and-locals)
  (case-family)
  (fns)
  (loop-and-recur)
  (letfns)
  (try-catch))
