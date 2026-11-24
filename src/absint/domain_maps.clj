(ns absint.domain-maps)

; todo: def,
;       set!,
;       protocol-invoke,
;       deftype,
;       reify,
;       method
;       monitor-enter,
;       monitor-exit

(def ast-fields
  {:const [:val],
   :local [:name],
   :var [:var],
   :binding [:name :init :variadic?],
   :the-var [:var],
   :quote [:expr],
   :if [:test :then :else],
   :do [:statements :ret],
   :let [:bindings :body],
   :letfn [:bindings :body],
   :loop [:bindings :body :loop-id],
   :recur [:exprs :loop-id],
   :case [:test :tests :thens :default :mask :shift :test-type :skip-check?],
   :case-test [:test :hash],
   :case-then [:then],
   :fn [:methods :local],
   :fn-method [:params :body :fixed-arity :variadic? :loop-id],
   :throw [:exception],
   :try [:body :catches :finally],
   :catch [:class :local :body],
   :invoke [:fn :args],
   :prim-invoke [:fn :args],
   :keyword-invoke [:keyword :target],
   :static-call [:class :method :args],
   :instance-call [:instance :method :args :class],
   :static-field [:class :field],
   :instance-field [:instance :field],
   :new [:class :args],
   :instance? [:class :target],
   :method-value [:class :method :kind],
   :host-interop [:target :m-or-f],
   :map [:keys :vals],
   :vector [:items],
   :set [:items],
   :with-meta [:expr :meta],
   :import [:class]})

(def always-present [:op :id :parent :my-role :my-position :children])

(def vector-roles
  "slots of the ast that are not single items
   ex: the binding vector of a let function:
   (let [a 1 b 2 c 3]...)"
  #{:args :bindings :catches :exprs :items :keys :methods :params :statements
    :tests :thens :vals})
