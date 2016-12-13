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
        (generate-code (first nodes))
        (recur (rest nodes))))))

(defn help-expression-statement [aast]
  (generate-code (node/get-child aast 0))
  (if (node/has-prop? aast :last)
    (do
    (println "\tsyscall $println")
    (println "\tpop"))
    (println "\tpop")))
  
(defn help-operator [aast ins]
  (recur-on-children aast)
  (println "\t" ins))

(defn help-op-assign [aast]
  (do
  (generate-code (node/get-child aast 1))
  (println (str "\tdup" (node/get-prop (node/get-child aast 1) :regnum)))
  (println (str "\tstlocal" (node/get-prop (node/get-child aast 1) :regnum)))))

(defn help-literal [aast]
  (let [literal-val (:value aast)]
    (println (str "%tldc_i " literal-val ))))  
  
(defn help-identifier [aast]
  (println (str "\tldlocal " (node/get-prop aast :regnum))))

(defn generate-code [aast]
  ;(println (str "; at " (:symbol aast)))
  (case (:symbol aast)
    :statement_list (recur-on-children aast)
    
    ; TODO: handle other kinds of AST nodes
    :expression_statement (help-expression-statement aast)
    :int_literal (help-literal aast)
    :str_literal (help-literal aast)
    :identifier (help-identifier aast)
   
    :op_assign (help-op-assign aast) 	
    :op_plus (help-operator aast "add") 	
    :op_minus (help-operator aast "sub") 	
    :op_mul (help-operator aast "mul") 	
    :op_div (help-operator aast "div") 	
    :op_exp (help-operator aast "exp") 	
    :op_lte (help-operator aast "jlte") 	
    :op_lt (help-operator aast "jlt") 	
    :op_gte (help-operator aast "jgte")
    :op_gt (help-operator aast "jgt")
    :op_eq (help-operator aast "je")
    :op_neq (help-operator aast "jne") 	
    
    ; Default case: do nothing
    ;(println (str "; ignored " (:symbol aast)))
    nil
    ))

(defn compile-unit [unit]
  (let [stmt-list (node/get-child unit 0)
        nlocals (node/get-prop stmt-list :nlocals)]
    (do
      ; This is the program entry point
      (println "main:")
      ; Reserve space for local variables
      (println (str "\tenter 0, " nlocals))
      ; Generate code for the top-level statement list
      (generate-code stmt-list)
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

(def testprog "var a; a := 4 * 5; a;")
;(def testprog "var a; var b; var c; b := 6; c := 3; a := b*c;")
(def parse-tree (p/parse (lexer/token-sequence (lexer/create-lexer (java.io.StringReader. testprog)))))
(def ast (ast/build-ast parse-tree))
(def aast (analyzer/augment-ast ast (s/create-scope)))
