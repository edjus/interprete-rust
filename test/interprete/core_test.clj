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
    (is (= true (palabra-reservada? 'break)))
    (is (= true (palabra-reservada? 'atan)))
    (is (= true (palabra-reservada? 'sqrt)))
    (is (= true (palabra-reservada? 'break)))

    )

  (testing "Shouldn't be reserved word"
    (is (= false (palabra-reservada? 'until)))
    (is (= false (palabra-reservada? '13)))
    (is (= false (palabra-reservada? 'hola)))
    )
)

(deftest test-identificador?
  (testing "Should be identificador"
    (is (= true (identificador? 'boolean)))
    (is (= true (identificador? 'e120)))
    )

  (testing "Shouldn't be identificador"
    (is (= false (identificador? 'bool)))
    (is (= false (identificador? '12e0)))
    (is (= false (identificador? 'sqrt)))
    (is (= false (identificador? 'atan)))
    (is (= false (identificador? 'abs)))
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

(deftest test-funciones-auxiliares
  (testing "cambiar signo"
    (is (= (cambiar-signo -1) 1))
    (is (= (cambiar-signo 1) -1))
    (is (= (cambiar-signo 0) 0))
    )

  (testing "negar boolenao"
    (is (= (negar-booleano false) true))
    (is (= (negar-booleano true) false))
    (is (thrown-with-msg? Exception #"Tipo invalido" (negar-booleano 0)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (negar-booleano "a")))
    (is (thrown-with-msg? Exception #"Tipo invalido" (negar-booleano 'i)))
    )

  (testing "parse a entero"
    (is (= (numero-a-entero "5") 5))
    (is (= (numero-a-entero "-3") -3))
    (is (thrown-with-msg? Exception #"Tipo invalido" (numero-a-entero false)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (numero-a-entero 'e2)))
    )

  (testing "parse a float"
    (is (= (numero-a-float 4) 4.0))
    (is (= (numero-a-float -1.0) -1.0))
    (is (= (numero-a-float "-1.2") -1.2))
    (is (= (numero-a-float "0.5") 0.5))
    (is (thrown-with-msg? Exception #"Tipo invalido" (numero-a-float false)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (numero-a-float 'e2)))
    )

  (testing "raiz-cuadrada"
    (is (= (raiz-cuadrada 9) 3.0))
    (is (= (raiz-cuadrada 144.0) 12.0))
    (is (= (raiz-cuadrada 0) 0.0))
    (is (thrown-with-msg? Exception #"Valor invalido" (raiz-cuadrada -4)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (raiz-cuadrada "0")))
    )

  (testing "calcular-seno"
    (is (= (calcular-seno 0) 0.0))
    (is (= (calcular-seno (/ Math/PI 2)) 1.0))
    (is (thrown-with-msg? Exception #"Tipo invalido" (calcular-seno false)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (calcular-seno "0")))
    )

  (testing "calcular-arcotangente"
    (is (= (calcular-arcotangente 0) 0.0))
    (is (= (calcular-arcotangente 1) (/ Math/PI 4)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (calcular-arcotangente false)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (calcular-arcotangente "0")))
    )

  (testing "calcular-valor-absoluto"
    (is (= (calcular-valor-absoluto -1) 1))
    (is (= (calcular-valor-absoluto 3) 3))
    (is (= (calcular-valor-absoluto 0) 0))
    (is (thrown-with-msg? Exception #"Tipo invalido" (calcular-valor-absoluto false)))
    (is (thrown-with-msg? Exception #"Tipo invalido" (calcular-valor-absoluto "0")))
    )

  (testing "get char from string"
    (is (= (get-char-from-string "1023F2" 2) \2))
    )
  )

(deftest test-listar
  (testing "tokens-a-string"
    (is (= (tokens-a-string ['fn (symbol "(") 0] 0 false) "fn ( 0 "))
    (is (= (tokens-a-string ['fn (symbol "{")] 0 false) "fn \n{\n"))
    (is (= (tokens-a-string [(symbol ";")] 0 false) ";\n"))
    (is (= (tokens-a-string [(symbol "}")] 1 false) "\n}\n"))
    (is (= (tokens-a-string ["hola, mundo"] 1 false) "\"hola, mundo\" "))
    (is (= (tokens-a-string ["f\t\tf\n"] 1 false) "\"f\\t\\tf\\n\" "))
    (is (= (tokens-a-string ['fn (symbol "(") 0] 1 true) "  fn ( 0 "))
    (is (= (tokens-a-string ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'println! (symbol "(") "Hola, mundo!" (symbol ")") (symbol "}")] 0 false)
           "fn main ( ) \n{\n  println! ( \"Hola, mundo!\" ) \n}\n"))
    )
  )

(deftest test-agregar-ptcoma
  (testing "procesar-tokens"
    (is (= (procesar-tokens '(1 2)) '(1 2)))
    (is (= (procesar-tokens '('fn 'io)) '('fn 'io)))
    (is (= (procesar-tokens (list 'fn (symbol "}"))) (list 'fn (symbol "}"))))
    (is (= (procesar-tokens (list (symbol "}") 'print)) (list (symbol "}") (symbol ";") 'print)))
    (is (= (procesar-tokens (list (symbol "}") 'else)) (list (symbol "}") 'else)))
    (is (= (procesar-tokens (list (symbol "}") (symbol ")"))) (list (symbol "}") (symbol ")"))))
    (is (= (procesar-tokens (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}")))
           (list 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'if 'x '< '0 (symbol "{") 'x '= '- 'x (symbol ";") (symbol "}") (symbol ";") 'renglon '= 'x (symbol ";") 'if 'z '< '0 (symbol "{") 'z '= '- 'z (symbol ";") (symbol "}") (symbol "}") 'fn 'foo (symbol "(") (symbol ")") (symbol "{") 'if 'y '> '0 (symbol "{") 'y '= '- 'y (symbol ";") (symbol "}") 'else (symbol "{") 'x '= '- 'y (symbol ";") (symbol "}") (symbol "}"))))
    )
  )

(deftest test-fixup
  (testing "fix-uo"
    (is (= (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
           [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] 8 [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]))
    (is (= (fixup [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP '?] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]] 4)
           [(symbol "{") (list 'x '= 20 (symbol ";") (symbol "}") (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'x (symbol ")") (symbol "}")) ['fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'x (symbol ":") 'i64 (symbol ";") 'if false (symbol "{") 'x '= 10 (symbol ";") (symbol "}") 'else] :sin-errores [[0 1 2] [['main ['fn [() ()]] 2] ['x ['var-inmut 'i64] 0]]] 1 [['CAL 2] 'HLT ['PUSHFI false] ['JC 5] ['JMP 8] ['PUSHFI 10] ['POP 0] ['JMP '?]] [[2 ['i64 nil]]]]
           ))
    )
  )

(deftest test-generar-ref
  (testing "econtrar terna desde el final"
    (is (= (encontrar-ultima-terna 'v [['v 1 0] ['g 2 3] ['v 3 0] ['w 0 10]])
           ['v 3 0]))
    (is (= (encontrar-ultima-terna 'v [['z 1 0] ['g 2 3] ['r 3 0] ['w 0 10]])
           nil))
    )

  (testing "encontra direccion"
    (is (= (encontrar-direccion ['fn 'v] [[1 2] [['v 1 0] ['g 2 3] ['v 3 0] ['w 0 10]]])
           0))
    (is (= (encontrar-direccion ['fn 'Y] [[1 2] [['v 1 0] ['g 2 3] ['v 3 0] ['w 0 10]]])
           nil))
    )

  (testing "generar  ref"
    (is (= (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
           [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] 8 [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]))
    (is (= (generar-ref [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]])
           [(symbol ")") (list (symbol ";") 'println! (symbol "(") "{}" (symbol ",") 'v (symbol ")") (symbol ";") (symbol "}")) ['fn 'inc (symbol "(") 'v (symbol ":") (symbol "&") 'mut 'i64 (symbol ")") (symbol "{") '* 'v (symbol "+=") 1 (symbol ";") (symbol "}") 'fn 'main (symbol "(") (symbol ")") (symbol "{") 'let 'mut 'v (symbol ":") 'i64 (symbol "=") 5 (symbol ";") 'inc (symbol "(") (symbol "&") 'mut 'v] :sin-errores [[0 2] [['inc ['fn [(list ['v (symbol ":") (symbol "&") 'mut 'i64]) ()]] 2] ['main ['fn [() ()]] 6] ['v ['var-mut 'i64] 0]]] 1 [['CAL 6] 'HLT ['POPARG 0] ['PUSHFI 1] ['POPADDREF 0] 'RETN ['PUSHFI 5] ['POP 0] ['PUSHADDR 0]] [[2 ['i64 nil]] [6 ['i64 nil]]]]))

    )
  )

(deftest test-convertir-formato-impresion
  (testing "cantidad-formatos"
    (is (= (cantidad-fmt "{}") 1))
    (is (= (cantidad-fmt "{:.8}\t{}") 2))
    (is (= (cantidad-fmt "io") 0))
    )

  (testing "procesar-formato"
    (is (= (procesar-formato '("{}-f3" "," "\n") '(2.0) 0) '("%.6f-f3" "," "\n")))
    (is (= (procesar-formato '("{:.8}-\t" "," "\n") '(2.0) 0) '("%.8f-\t" "," "\n")))
    (is (= (procesar-formato '(" {}") '(3) 0) '(" %d")))
    (is (= (procesar-formato '(" {}") '(\a) 0) '(" %s")))

    )

  (testing "convertir-formato-impresion"
    (is (= (convertir-formato-impresion '("Hola, mundo!"))
           '("Hola, mundo!")))
    (is (= (convertir-formato-impresion '("- My name is {}, James {}.\n- Hello, {}{}{}!" "Bond" "Bond" 0 0 7))
           '("- My name is %s, James %s.\n- Hello, %d%d%d!" "Bond" "Bond" 0 0 7)))
    (is (= (convertir-formato-impresion '("{}\t{}" 0 "0")) '("%d\t%s" 0 "0")))
    (is (= (convertir-formato-impresion '("Las raices cuadradas de {} son +{:.8} y -{:.8}" 4.0 1.999999999985448 1.999999999985448))
           '("Las raices cuadradas de %.6f son +%.8f y -%.8f" 4.0 1.999999999985448 1.999999999985448)))
    )

  (deftest test-compatibles?
    (testing "compatible"
      (is (= (compatibles? 'i64 5) true))
      (is (= (compatibles? 'i64 [5.0]) true))
      (is (= (compatibles? 'String "Hola") true))
      (is (= (compatibles? 'bool true) true))
      (is (= (compatibles? 'usize 1) true))
      (is (= (compatibles? 'char \a) true))
      (is (= (compatibles? 'char ['a]) true))
      )

    (testing "no compatible"
      (is (= (compatibles? 'i64 5.0) false))
      (is (= (compatibles? 'bool 1) false))
      (is (= (compatibles? 'String "Hola")))
      (is (= (compatibles? 'usize "1") false))
      (is (= (compatibles? 'char 'a)))
      )
    ))
