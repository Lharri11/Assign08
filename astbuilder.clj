(ns minilang.codegen
  (:require [minilang.lexer :as lexer])
  (:require [minilang.node :as node])
  (:require [minilang.parser2 :as p])
  (:require [minilang.prettyprint :as pp])
  (:require [minilang.astbuilder :as ast])
  (:require [minilang.scope :as s])
  (:require [minilang.analyzer :as analyzer]))

(declare generate-code)

; Recursively generate code for all children of given augmented AST node.
(defn recur-on-children [aast]
  (loop [nodes (node/children aast)]
    (if (not (empty? nodes))
      (do
        (generate-code (first nodes) "")
        (recur (rest nodes))))))

(defn help-expression-statement [aast]
  (generate-code (node/get-child aast 0) "")
  (if (node/has-prop? aast :last)
    (do
    (println "\tsyscall $println")
    (println "\tpop"))
    (println "\tpop")))
  
(defn help-operator [aast ins]
  (recur-on-children aast)
  (println ins))

(defn help-op-assign [aast]
  (do
  (generate-code (node/get-child aast 1) "")
  (println (str "\tdup\n\tstlocal " (node/get-prop (node/get-child aast 0) :regnum)))))

(defn help-literal [aast]
  (let [literal-val (:value aast)]
    (println (str "\tldc_i " literal-val ))))  
  
(defn help-identifier [aast]
  (println (str "\tldlocal " (node/get-prop aast :regnum))))

(defn help-if-statement [aast]
  (new-label)
  (invert-comparison aast)
  (generate-code (node/get-child aast 0) "")
  (generate-code (node/get-child aast 1) ""))


(defn generate-code [aast ontrue]
  ;(println (str "; at " (:symbol aast)))
  (case (:symbol aast)
    :statement_list (recur-on-children aast)
    
    ; TODO: handle other kinds of AST nodes
    :expression_statement (help-expression-statement aast)
    :int_literal (help-literal aast)
    :str_literal (help-literal aast)
    :identifier (help-identifier aast)
   
    :op_assign (help-op-assign aast) 	
    :op_plus (help-operator aast "\tadd") 	
    :op_minus (help-operator aast "\tsub") 	
    :op_mul (help-operator aast "\tmul") 	
    :op_div (help-operator aast "\tdiv") 	
    :op_exp (help-operator aast "\texp") 	
    :op_lte (help-operator aast "\tjlte") 	
    :op_lt (help-operator aast "\tjlt") 	
    :op_gte (help-operator aast "\tjgte")
    :op_gt (help-operator aast "\tjgt")
    :op_eq (help-operator aast "\tje")
    :op_neq (help-operator aast "\tjne") 	
    
    ; Default case: do nothing
    ;(println (str "; ignored " (:symbol aast)))
    nil
    ))

; Generate a label
(defn new-label []
  (str (gensym)))

(def inverted-comparison-ops
  {:op_je :op_jne,
   :op_jne :op_je,
   :op_lt :op_gte,
   :op_gte :op_lt,
   :op_lte :op_gt,
   :op_gt :op_lte})

(defn invert-comparison [aast]
  ;(println "; Inverting comparison " (:symbol aast))
  (if (not (contains? inverted-comparison-ops (:symbol aast)))
    (throw (RuntimeException. (str (:symbol aast) " expression is not a condition?")))
    (node/make-node-with-props (get inverted-comparison-ops (:symbol aast)) (:value aast) (:props aast))))


(defn compile-unit [unit]
  (let [stmt-list (node/get-child unit 0)
        nlocals (node/get-prop stmt-list :nlocals)]
    (do
      ; This is the program entry point
      (println "main:")
      ; Reserve space for local variables
      (println (str "\tenter 0, " nlocals))
      ; Generate code for the top-level statement list
      (generate-code stmt-list "")
      ; Emit code to return cleanly (to exit the program)
      (println "\tldc_i 0")
      (println "\tret"))))


; ----------------------------------------------------------------------
; Testing:
; Edit testprog, then reload this file, then execute
;
;   (compile-unit aast)
;
; in a REPL.
; ----------------------------------------------------------------------

;(def testprog "var a; a := 4 * 5; a;")
(def testprog "var a; var b; var c; b := 6; c := 3; a := b*c;")
(def parse-tree (p/parse (lexer/token-sequence (lexer/create-lexer (java.io.StringReader. testprog)))))
(def ast (ast/build-ast parse-tree))
(def aast (analyzer/augment-ast ast (s/create-scope)))
