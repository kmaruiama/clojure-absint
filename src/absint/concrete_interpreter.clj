(ns absint.concrete-interpreter
  (:require [absint.domain-maps :as ast-fields])
  (:import (clojure.lang RT Reflector)))

(declare interpret)

; used only to interpret letfn, nowhere else

(defrecord LetFnPlace [place])

; custom nodes, done for ergonomics purposes and possible future hooks,
; don't actually exist in tools.analyzer

(defn indicate-exprs [e] {:op :CUSTOM/exprs, :content e})

(defn indicate-params [p] {:op :CUSTOM/params, :content p})

(defn indicate-bindings [b] {:op :CUSTOM/bindings, :content b})

(defn children
  [node-map father children-op]
  (let [xs (get node-map [(:id father) children-op])]
    (if (contains? ast-fields/vector-roles children-op) xs (first xs))))

(defn interpret-exprs
  [node-map exprs context]
  (mapv #(interpret node-map % context) (:content exprs)))

(defn interpret-static-call
  [node-map node context]
  (Reflector/invokeStaticMethod (:class node)
                                (name (:method node))
                                (as-> (children node-map node :args) args
                                  (indicate-exprs args)
                                  (interpret node-map args context)
                                  (object-array args))))

(defn interpret-quote [node-map node] (:val (children node-map node :expr)))

(defn recursively-unquote
  [form]
  (cond (and (seq? form) (= (first form) 'quote)) (second form)
        (map? form) (into (empty form)
                          (map (fn [[k x]] [(recursively-unquote k)
                                            (recursively-unquote x)]))
                          form)
        (vector? form) (mapv recursively-unquote form)
        (set? form) (into (empty form) (map recursively-unquote) form)
        :else form))

(defn interpret-const
  [node]
  (let [v (:val node)] (if (coll? v) (recursively-unquote v) v)))

(defn interpret-let
  [node-map node context]
  (let [bindings (as-> (children node-map node :bindings) bindings
                   (indicate-bindings bindings)
                   (interpret node-map bindings context))]
    (interpret node-map
               (children node-map node :body)
               (merge context bindings))))

(defn interpret-bindings
  [node-map node context]
  (loop [content (:content node)
         cur-ctx context]
    (if (empty? content)
      cur-ctx
      (let [current (first content)]
        (recur (rest content)
               (conj cur-ctx
                     {(:name current) (interpret
                                        node-map
                                        (children node-map current :init)
                                        cur-ctx)}))))))

(defn interpret-local
  [node context]
  (let [value (get context (:name node))]
    (if (instance? LetFnPlace value) (deref (:place value)) value)))

(defn interpret-if
  [node-map node context]
  (let [t (interpret node-map (children node-map node :test) context)]
    (if t
      (interpret node-map (children node-map node :then) context)
      (interpret node-map (children node-map node :else) context))))

(defn interpret-do
  [node-map node context]
  (as-> (children node-map node :statements) statements
    (indicate-exprs statements)
    (interpret node-map statements context))
  (interpret node-map (children node-map node :ret) context))

(defn interpret-map
  [node-map node context]
  (as-> (interleave (children node-map node :keys)
                    (children node-map node :vals))
    items
    (indicate-exprs items)
    (interpret node-map items context)
    (object-array items)
    (RT/map items)))

(defn interpret-set-and-vector
  [node-map node context constructor]
  (as-> (children node-map node :items) items
    (indicate-exprs items)
    (interpret node-map items context)
    (object-array items)
    (constructor items)))

(defn interpret-with-meta
  [node-map node context]
  (with-meta (interpret node-map (children node-map node :expr) context)
    (interpret node-map (children node-map node :meta) context)))

(defn interpret-var [node] (deref (:var node)))

(defn interpret-the-var [node] (:var node))

(defn interpret-keyword-invoke
  [node-map node context]
  (let [k (interpret node-map (children node-map node :keyword) context)
        target (interpret node-map (children node-map node :target) context)]
    (k target)))

(defn interpret-instance?
  [node-map node context]
  (let [c (:class node)
        target (interpret node-map (children node-map node :target) context)]
    (instance? c target)))

(defn interpret-instance-call
  "INTEROP: interprets an instance call.
   throws when the instance and the class are mismatched
   and waits until the interop is resolved before evaluating the args"
  [node-map node context]
  (let [c (:class node)
        method (:method node)
        instance
          (interpret node-map (children node-map node :instance) context)]
    (if (and c (not (nil? instance)) (not (instance? c instance)))
      (throw (Exception. "Instance does not match class!"))
      (Reflector/invokeInstanceMethod instance
                                      (str method)
                                      (as-> (children node-map node :args) args
                                        (indicate-exprs args)
                                        (interpret node-map args context)
                                        (object-array args))))))

(defn interpret-invoke
  [node-map node context]
  (let [f (interpret node-map (children node-map node :fn) context)
        args (as-> (children node-map node :args) args
               (indicate-exprs args)
               (interpret node-map args context))]
    (apply f args)))

(defn interpret-host-interop
  [node-map node context]
  (let [target (interpret node-map (children node-map node :target) context)]
    (Reflector/invokeNoArgInstanceMember target (str (:m-or-f node)) false)))

(defn interpret-new
  [node-map node context]
  (let [c (:val (children node-map node :class))]
    (Reflector/invokeConstructor c
                                 (as-> (children node-map node :args) args
                                   (indicate-exprs args)
                                   (interpret node-map args context)
                                   (object-array args)))))

(defn interpret-static-field
  [node]
  (let [c (:class node)
        field (:field node)]
    (Reflector/getStaticField c (str field))))

(defn interpret-instance-field
  [node-map node context]
  (let [instance (interpret node-map (children node-map node :instance) context)
        field (:field node)]
    (Reflector/getInstanceField instance (str field))))

(defn interpret-method-value
  "Does not return values per-se, but partially applied Reflector methods
   valid only for clojure >= 1.12.0"
  [node]
  (let [c (:class node)
        method (:method node)
        kind (:kind node)]
    (cond (= kind :instance) (fn [obj & args]
                               (Reflector/invokeInstanceMethod obj
                                                               (str method)
                                                               (object-array
                                                                 args)))
          (= kind :static)
            (fn [& args]
              (Reflector/invokeStaticMethod c (str method) (object-array args)))
          (= kind :ctor)
            (fn [& args] (Reflector/invokeConstructor c (object-array args))))))

(defn interpret-import
  [node]
  (let [c (RT/classForName (:class node))] (.importClass *ns* c)))

(defn interpret-params
  [node]
  (let [params (:content node)]
    (mapv #(select-keys % [:name :variadic? :my-position]) params)))

(defn interpret-fn-method
  [node-map node context]
  {:params (as-> (children node-map node :params) params
             (indicate-params params)
             (interpret node-map params context)),
   :body (children node-map node :body),
   :fixed-arity (:fixed-arity node),
   :variadic? (:variadic? node),
   :loop-id (:loop-id node)})

(defn choose-method
  [args my-methods]
  (let [number-of-arguments (count args)
        variadic-method (filter #(and (= (:variadic? %) true)
                                      (<= (:fixed-arity %) number-of-arguments))
                          my-methods)
        matches (filter #(and (not (:variadic? %))
                              (= (:fixed-arity %) number-of-arguments))
                  my-methods)
        number-of-matches (count matches)]
    (cond (and (= number-of-matches 0) (seq variadic-method)) (first
                                                                variadic-method)
          (not= 0 number-of-matches) (first matches)
          :else (throw (Exception.
                         "no arity matches the number of args you passed")))))

(defn build-context
  [params args context]
  (let [fixed-params (remove :variadic? params)
        rest-param (first (filter :variadic? params))
        number-of-fixed (count fixed-params)
        fixed-args (take number-of-fixed args)
        remaining-args (seq (drop number-of-fixed args))
        fixed-context (into context
                            (->> (interleave (map :name fixed-params)
                                             fixed-args)
                                 (partition 2)
                                 (mapv vec)))]
    (if (nil? rest-param)
      fixed-context
      (conj fixed-context {(:name rest-param) remaining-args}))))

(defn run-method
  [node-map method context]
  (loop [current-context context]
    (let [result (interpret node-map (:body method) current-context)]
      (if (= (:id result) (:loop-id method))
        (recur (into context
                     (zipmap (mapv :name (:params method))
                             (:next-bindings result))))
        result))))

; todo: document this behemoth
(defn interpret-fn
  [node-map node context]
  (let [my-name (:name (children node-map node :local))
        my-methods (as-> (children node-map node :methods) m
                     (indicate-exprs m)
                     (interpret node-map m context))]
    (fn closure [& args]
      (let [chosen-method (choose-method args my-methods)
            context-with-me
              (if (nil? my-name) context (conj context {my-name closure}))]
        (run-method
          node-map
          chosen-method
          (build-context (:params chosen-method) args context-with-me))))))

(defn interpret-loop
  [node-map node context]
  (let [id (:loop-id node)
        names (mapv :name (children node-map node :bindings))]
    (loop [bindings (as-> (children node-map node :bindings) bindings
                      (indicate-bindings bindings)
                      (interpret node-map bindings context))]
      (let [result (interpret node-map
                              (children node-map node :body)
                              (into context bindings))]
        (if (= (:id result) id)
          (recur (zipmap names (:next-bindings result)))
          result)))))

(defn interpret-recur
  [node-map node context]
  {:id (:loop-id node),
   :next-bindings (as-> (children node-map node :exprs) exprs
                    (indicate-exprs exprs)
                    (interpret node-map exprs context))})

(defn matched-catch
  [catches e]
  (first (filter #(instance? (:type %) e) catches)))

(defn interpret-try
  [node-map node context]
  (let [finally-block (children node-map node :finally)
        run (children node-map node :body)
        possible-catches-for-try (as-> (children node-map node :catches) c
                                   (indicate-exprs c)
                                   (interpret node-map c context))]
    (try (interpret node-map run context)
         (catch Throwable e
           (let [chosen-catch (matched-catch possible-catches-for-try e)]
             (if (or (nil? chosen-catch) (:interpreter/error (ex-data e)))
               (throw e)
               (interpret node-map
                          (:body chosen-catch)
                          (conj context {(:name chosen-catch) e})))))
         (finally (when (not (nil? finally-block))
                    (interpret node-map finally-block context))))))

(defn interpret-catch
  [node-map node]
  {:type (:val (children node-map node :class)),
   :name (:name (children node-map node :local)),
   :body (children node-map node :body)})

(defn interpret-throw
  [node-map node context]
  (let [exception
          (interpret node-map (children node-map node :exception) context)]
    (throw exception)))

(defn case-key
  " @llm "
  [{:keys [mask shift]} input]
  (let [input-hash (clojure.lang.Util/hash input)]
    (if (zero? mask)
      input-hash
      (bit-and (bit-shift-right input-hash shift) mask))))

(defn merged-then
  " @llm "
  [node-map node input thens]
  (let [input-key (case-key node input)
        thens-by-hash (zipmap (map :hash (children node-map node :tests))
                              thens)]
    (when (and (not= :int (:test-type node))
               (contains? (:skip-check? node) input-key))
      (get thens-by-hash input-key))))

(defn interpret-case
  [node-map node context]
  (let [x (interpret node-map (children node-map node :test) context)
        tests (as-> (children node-map node :tests) tests
                (indicate-exprs tests)
                (interpret node-map tests context))
        thens (as-> (children node-map node :thens) thens
                (indicate-exprs thens)
                (interpret node-map thens context))
        cond-map (zipmap tests thens)
        result (or (get cond-map x) (merged-then node-map node x thens))]
    (if (nil? result)
      (interpret node-map (children node-map node :default) context)
      (interpret node-map result context))))

(defn interpret-case-test
  [node-map node context]
  (interpret node-map (children node-map node :test) context))

(defn interpret-case-then [node-map node] (children node-map node :then))

(defn interpret-letfn
  [node-map node context]
  (let [uninterpreted-bindings (children node-map node :bindings)
        must-deliver (reduce #(assoc %1 (:name %2) (->LetFnPlace (promise)))
                       {}
                       uninterpreted-bindings)
        context-with-mocked-functions (into context must-deliver)
        context-with-real-functions
          (as-> uninterpreted-bindings bindings
            (indicate-bindings bindings)
            (interpret node-map bindings context-with-mocked-functions))]
    (doseq [[k v] must-deliver]
      (deliver (:place v) (k context-with-real-functions)))
    (interpret node-map
               (children node-map node :body)
               context-with-real-functions)))

; def is pointless to do concretely as I think the order in which the vars
; will be dispatched matters, and I cannot do a workaround in the abstract
; version
(defn interpret
  "Recurs over the s-expr tree starting by the root node"
  [node-map node context]
  (let [op (:op node)]
    (case op
      ; case
      :case (interpret-case node-map node context)
      :case-test (interpret-case-test node-map node context)
      :case-then (interpret-case-then node-map node)
      ; collections & metadata
      :vector (interpret-set-and-vector node-map node context vec)
      :map (interpret-map node-map node context)
      :set (interpret-set-and-vector node-map node context RT/set)
      :with-meta (interpret-with-meta node-map node context)
      ; constants & quote
      :const (interpret-const node)
      :quote (interpret-quote node-map node)
      ; control flow
      :if (interpret-if node-map node context)
      ; custom nodes
      :CUSTOM/exprs (interpret-exprs node-map node context)
      :CUSTOM/bindings (interpret-bindings node-map node context)
      :CUSTOM/params (interpret-params node)
      ; functions
      :fn (interpret-fn node-map node context)
      :fn-method (interpret-fn-method node-map node context)
      ; interop
      :static-call (interpret-static-call node-map node context)
      :instance-call (interpret-instance-call node-map node context)
      :host-interop (interpret-host-interop node-map node context)
      :new (interpret-new node-map node context)
      :static-field (interpret-static-field node)
      :instance-field (interpret-instance-field node-map node context)
      :method-value (interpret-method-value node)
      :import (interpret-import node)
      ; letfn
      :letfn (interpret-letfn node-map node context)
      ; locals, let, do
      :local (interpret-local node context)
      :let (interpret-let node-map node context)
      :do (interpret-do node-map node context)
      ; loop
      :loop (interpret-loop node-map node context)
      :recur (interpret-recur node-map node context)
      ; try / catch / finally / throw
      :try (interpret-try node-map node context)
      :catch (interpret-catch node-map node)
      :throw (interpret-throw node-map node context)
      ; vars & calls
      :var (interpret-var node)
      :the-var (interpret-the-var node)
      (:invoke :prim-invoke) (interpret-invoke node-map node context)
      :keyword-invoke (interpret-keyword-invoke node-map node context)
      :instance? (interpret-instance? node-map node context)
      (throw (ex-info (str "unidentified operator key (:op) " op)
                      {:interpreter/error true, :op op})))))