(ns interprete.core-test
  (:require [clojure.test :refer :all]
            [interprete.core :refer :all]))

(deftest test-palabra-reservada?
  (testing "Should be reserved word"
    (is (= true (palabra-reservada? 'io)))
    (is (= true (palabra-reservada? 'Write)))
    (is (= true (palabra-reservada? 'process)))
    (is (= true (palabra-reservada? 'mut)))
    (is (= true (palabra-reservada? 'exit)))
    (is (= true (palabra-reservada? 'flush)))
    (is (= true (palabra-reservada? 'read_line)))
    (is (= true (palabra-reservada? 'chars)))
    (is (= true (palabra-reservada? 'nth)))
    (is (= true (palabra-reservada? 'unwrap)))
    (is (= true (palabra-reservada? 'fn)))
    )

  (testing "Shouldn't be reserved word"
    (is (= false (palabra-reservada? 'while)))
    (is (= false (palabra-reservada? 'until)))
    (is (= false (palabra-reservada? '13)))
    (is (= false (palabra-reservada? 'hola)))
    )
)

; TODO: ver que hay que validar
(deftest test-identificador?
  (testing "Should be identificador"
    (is (= true (identificador? 'boolean)))
    (is (= true (identificador? 'e120)))
    )

  (testing "Shouldn't be identificador"
    (is (= false (identificador? 'bool)))
    (is (= false (identificador? '12e0)))
    )
)

(deftest test-dividir
  (testing "Dividir are correct"
    (is (= (dividir 12 3) 4 ))
    (is (= (dividir 12.0 3) 4.0 ))
    (is (= (dividir 12 3.0) 4.0 ))
    (is (= (dividir 12.0 3.0) 4.0 ))
    (is (= (dividir 1 2) 0 ))
    (is (= (dividir 1 2.0) 0.5 ))
    )
  )

(deftest test-pasar-a-int
  (testing "Pasar-a-int can parse"
    (is (= (pasar-a-int "10") 10 ))
    (is (= (pasar-a-int 10.0) 10 ))
    (is (= (pasar-a-int 10) 10 ))
    (is (= (pasar-a-int '4) 4 ))
    )

  (testing "Pasar-a-int can't parse"
    (is (= (pasar-a-int "123r") "123r" ))
    (is (= (pasar-a-int 'a) 'a ))
    (is (= (pasar-a-int '[10.0]) [10.0])))
  )

(deftest test-pasar-a-float
  (testing "Pasar-a-float can parse"
    (is (= (pasar-a-float "10") 10.0 ))
    (is (= (pasar-a-float 10.0) 10.0 ))
    (is (= (pasar-a-float 10) 10.0 ))
    (is (= (pasar-a-float '4) 4.0 ))
    )

  (testing "Pasar-a-float can't parse"
    (is (= (pasar-a-float "123r") "123r" ))
    (is (= (pasar-a-float 'a) 'a ))
    (is (= (pasar-a-float '[10]) [10])))
  )

(deftest test-ya-declarado-localmente?
  (testing "procesar contexto"
    (is (= (procesar-contexto 0 [['io ['lib '()] 0] ['Write ['lib '()] 0]]) ['io, 'Write]))
    (is (= (procesar-contexto 1 [['io ['lib '()] 0] ['Write ['lib '()] 0]]) ['Write]))
    )

  (testing "buscar indetificador"
    (is (= (buscar-identificador 'io ['io, 'Write]) true))
    (is (= (buscar-identificador 'Read ['io, 'Write]) false))
    )

  (testing "ya-declarada-localmente?"
    (is (= (ya-declarado-localmente? 'Write [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) true))
    (is (= (ya-declarado-localmente? 'Read [[0] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) false))
    (is (= (ya-declarado-localmente? 'Write [[0 1] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) true))
    (is (= (ya-declarado-localmente? 'Write [[0 2] [['io ['lib '()] 0] ['Write ['lib '()] 0] ['entero_a_hexa ['fn [(list ['n (symbol ":") 'i64]) 'String]] 2]]]) false))
    )
  )

(deftest test-reemplazar-en-coleccion
  (testing "reemplazar en coleccion"
    (is (= (reemplazar-en-coleccion 1 9 [1, 2, 3, 4]) [1, 9, 3, 4]))
    (is (= (reemplazar-en-coleccion 1 9 []) nil))
    (is (= (reemplazar-en-coleccion 5 9 [1, 2, 3, 4]) nil))
    )
  )

(deftest test-cargar-en-ult-reg
  (testing "cargar en ultimo registro"
    (is (= (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 nil]]] 1 'i64 0)
           [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]]
           ))
    (is (= (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['i64 nil] ['i64 0]]] 0 'f64 3)
           [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]] [['f64 3] ['i64 0]]]
           ))
    (is (= (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]]] 2 'f64 5)
           [[['String "2"] ['i64 6] ['f64 5] ['i64 3] ['i64 0]]]
           ))
    (is (= (cargar-en-ult-reg [[['String "2"] ['i64 6] ['i64 2] ['i64 3] ['i64 0]]] 5 'f64 5)
           nil))
    )
  )

(deftest test-cargar-en-reg-dest
  (testing "cargar en ultimo registro"
    (is (= (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 2]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 4] 'i64 0)
           [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]]
           ))
    (is (= (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [0 3] 'f64 3)
           [[['String "2"] ['i64 6] ['i64 2] ['f64 3] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]]
           ))
    (is (= (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [1 2] 'f64 4)
           [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['f64 4] ['i64 [0 4]] ['i64 2] ['i64 2]]]
           ))
    (is (= (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [1 9] 'f64 4)
           nil
           ))
    (is (= (cargar-en-reg-dest [[['String "2"] ['i64 6] ['i64 2] ['i64 2] ['i64 0]] [['i64 6] ['i64 2] ['i64 [0 3]] ['i64 [0 4]] ['i64 2] ['i64 2]]] [9 1] 'f64 4)
           nil
           ))
    )
  )

(deftest test-inicializar-contexto-local
  (testing "iniciar contexto local aux"
    (is (= (iniciar-contexto-local-aux [[0] [['cont]]]) [[0, 1] [['cont]]]))
    (is (= (iniciar-contexto-local-aux [[0] []]) [[0, 0] []]))
    )

  (testing "inicializar contexto local"
    (is (= (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
           [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] 8 [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]))
    (is (= (inicializar-contexto-local [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []])
           [(symbol "{") (list 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")")] :sin-errores [[0 1] [['main ['fn [() ()]] 2]]] 0 [['CAL 2] 'HLT] []]))
    )
  )

(deftest test-cargar-const-en-tabla
  (testing "armar-terna-const"
    (is (= (armar-terna-const ['const 'UNO (symbol ":") 'i64 (symbol "=") 1])
           ['UNO ['const 'i64] 1]))
    (is (= (armar-terna-const [2 'io (symbol "}") 'const 'DOS (symbol ":") 'i32 (symbol "=") 2])
           ['DOS ['const 'i32] 2]))
    )

  (testing "modificar-contexto-con-const"
    (is (= (modificar-contexto-con-const [[0] [['Write ['lib '()] "h"]]] ['DOS ['const 'i32] 2])
           [[0] [['Write ['lib '()] "h"] ['DOS ['const 'i32] 2]]]))
    )

  (testing "cargar-const-en-tabla"
    (is (= (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
           [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] 8 [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []]))
    (is (= (cargar-const-en-tabla [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0]]] 0 [['CAL 0] 'HLT] []])
           [(symbol ";") (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "{}" (symbol ",") 'TRES (symbol ")") (symbol "}")) ['use 'std (symbol "::") 'io (symbol ";") 'const 'TRES (symbol ":") 'i64 (symbol "=") 3] :sin-errores [[0] [['io ['lib '()] 0] ['TRES ['const 'i64] 3]]] 0 [['CAL 0] 'HLT] []]))
    )
  )

(deftest test-restaurar-contexto-anterior
  (testing "frontera"
    (is (= (frontera [[0 1 2] [['io 'Write]]]) 2))
    )

  (testing "eliminar-ultimo"
    (is (= (eliminar-ultimo [1 2 3 4 5]) [1 2 3 4]))
    (is (= (eliminar-ultimo []) []))
    )

  (testing "eliminar-desde"
    (is (= (eliminar-desde 2 [1 2 3 4 5]) [1 2]))
    (is (= (eliminar-desde 0 [1 2 3 4 5]) []))
    (is (= (eliminar-desde 6 [1 2 3 4 5]) [1 2 3 4 5]))
    )

  (testing "restaurar-contexto-anterior"
    (is (= (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
           ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] 8 [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]))
    (is (= (restaurar-contexto-anterior ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0 1] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0] ['y ['var-inmut 'i64] 1]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]])
           ['EOF () ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol "=") 10 (symbol ";") 'let 'y (symbol ":") 'i64 (symbol "=") 20 (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x '+ 'y (symbol ")") (symbol "}")] :sin-errores [[0] [['main ['fn [() ()]] 2]]] 2 [['CAL 2] 'HLT ['PUSHFI 10] ['POP 0] ['PUSHFI 20] ['POP 1] ['PUSHFI "{}"] ['PUSHFM 0] ['PUSHFM 1] 'ADD ['PUSHFI 2] 'OUT 'NL] [[2 ['i64 nil] ['i64 nil]]]]))
    )

  )

(deftest test-buscar-tipo-de-retorno
  (testing "buscar-terna"
    (is (= (buscar-terna [['io [] 2] ['TRES ['const 'i64] 8]] 8)
           ['TRES ['const 'i64] 8]))
    (is (= (buscar-terna [['io [] 2] ['TRES ['const 'i64] 3] ['FN [] 8]] 2)
           ['io [] 2]))
    (is (= (buscar-terna [['io [] 2] ['TRES ['const 'i64] 3] ['FN [] 8]] 4)
           nil))
    )

  (testing "buscar-tipo-de-retorno"
    (is (= (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 2)
          'i64))
    (is (= (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 8)
           '()))
    (is (= (buscar-tipo-de-retorno [(symbol ";") (list 'println! (symbol "(") "La suma de 5 mas 7 es {}" (symbol ",") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")") (symbol ")") (symbol ";") (symbol "}")) ['fn 'suma (symbol "(") 'x (symbol ":") 'i64 (symbol ",") 'y (symbol ":") 'i64 (symbol ")") (symbol "->") 'i64 (symbol "{") 'x '+ 'y (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'suma (symbol "(") 5 (symbol ",") 7 (symbol ")")] :sin-errores [[0 2] [['suma ['fn [(list ['x (symbol ":") 'i64] ['y (symbol ":") 'i64]) 'i64]] 2] ['main ['fn [() ()]] 8]]] 0 [['CAL 8] 'HLT ['POPARG 1] ['POPARG 0] ['PUSHFM 0] ['PUSHFM 1] 'ADD 'RET ['PUSHFI 5] ['PUSHFI 7] ['CAL 2]] [[2 ['i64 nil] ['i64 nil]] [8]]] 1)
           nil))
    )
  )
