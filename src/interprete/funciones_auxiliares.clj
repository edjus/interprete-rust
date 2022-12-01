(ns interprete.funciones_auxiliares)

(declare dividir-segun-tipo)
(declare cambiar-signo)
(declare raiz-cuadrada)
(declare numero?)
(declare buscar-mensaje)
(declare negar-booleano)
(declare booleano?)
(declare numero-a-entero)
(declare numero-a-float)
(declare calcular-seno)
(declare calcular-arcotangente)
(declare calcular-valor-absoluto)
(declare get-char-from-string)

(defn calcular-arcotangente [val]
  (if (numero? val)
    (Math/atan val)
    (do (print "ERROR: ") (println (buscar-mensaje 39)) (throw (Exception. "Tipo invalido")))
    )
  )

(defn calcular-valor-absoluto [val]
  (if (numero? val)
    (abs val)
    (do (print "ERROR: ") (println (buscar-mensaje 39)) (throw (Exception. "Tipo invalido")))
    )
  )

(defn get-char-from-string [txt, pos]
  (get txt pos)
  )

(defn calcular-seno [val]
  (if (numero? val)
    (Math/sin val)
    (do (print "ERROR: ") (println (buscar-mensaje 39)) (throw (Exception. "Tipo invalido")))
    )
  )

(defn numero-a-float [val]
  (cond
    (numero? val) (float val)
    (string? val) (Double/parseDouble val)
    :else (do (print "ERROR: ") (println (buscar-mensaje 39)) (throw (Exception. "Tipo invalido")))
    )
  )

(defn numero-a-entero [val]
  (cond
    (numero? val) (int val)
    (string? val) (Integer/parseInt val)
    :else (do (print "ERROR: ") (println (buscar-mensaje 39)) (throw (Exception. "Tipo invalido")))
    )
  )

(defn booleano? [x]
  (contains? #{"true" "false"} (str x))
  )

(defn negar-booleano [val]
  (if (booleano? val)
    (not val)
    (do (print "ERROR: ") (println (buscar-mensaje 58)) (throw (Exception. "Tipo invalido")))
    )
  )

(defn cambiar-signo [n]
  (* -1 n)
  )

(defn raiz-cuadrada [val]
  (cond
    (not (numero? val))  (do (print "ERROR: ") (println (buscar-mensaje 39)) (throw (Exception. "Tipo invalido")))
    (neg? val) (do (println "ERROR: SE ESPERABA UN NUMERO NO NEGATIVO") (throw (Exception. "Valor invalido")))
    :else (Math/sqrt val)
    )
  )

(defn dividir-segun-tipo [num, div]
  (cond
    (int? num) (int (/ num div))
    (float? num) (float (/ num div))
    :else nil
    )
  )

(defn numero? [x]
  (number? x)
  )

(defn buscar-mensaje [cod]
  (case cod
    1 "COMANDO DESCONOCIDO"
    2 "ARCHIVO NO ENCONTRADO"
    3 "SE ENCONTRO PREMATURAMENTE EL FIN DEL ARCHIVO:  EOF"
    4 "SE ESPERABA std"
    5 "SE ESPERABA UN OPERADOR DE RESOLUCION DE AMBITO:  ::"
    6 "SE ESPERABA LA PALABRA RESERVADA io"
    7 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS Write O process"
    8 "SE ESPERABA UN PUNTO Y COMA:  ;"
    9 "SE ESPERABA UN IGUAL:  ="
    10 "SE ESPERABA UN IDENTIFICADOR"
    11 "SE ESPERABA ABRIR UN PARENTESIS:  ("
    12 "SE ESPERABA CERRAR UN PARENTESIS:  )"
    13 "SE ESPERABA UN TIPO DE RETORNO:  i64, f64, bool O String"
    14 "SE ESPERABAN DOS PUNTOS:  :"
    15 "SE ESPERABA UN TIPO DE PARAMETRO:  i64 O f64"
    16 "SE ESPERABA LA PALABRA RESERVADA mut"
    17 "SE ESPERABA ABRIR UNA LLAVE:  {"
    18 "SE ESPERABA UN PUNTO Y COMA O CERRAR UNA LLAVE:  ; O }"
    19 "SE ESPERABA UN TIPO DE VARIABLE:  i64, f64, bool, char O String"
    20 "SE ESPERABA UN TIPO DE CONSTANTE:  i64 O f64"
    21 "SE ESPERABA UN INICIO DE EXPRESION"
    22 "SE ESPERABA LA PALABRA RESERVADA exit"
    23 "SE ESPERABA UN PUNTO:  ."
    24 "SE ESPERABA LA PALABRA RESERVADA expect"
    25 "SE ESPERABA UNA CADENA"
    26 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS stdin O stdout"
    27 "SE ESPERABA LA PALABRA RESERVADA flush"
    28 "SE ESPERABA LA PALABRA RESERVADA read_line"
    29 "SE ESPERABA UN AMPERSAND:  &"
    30 "SE ESPERABA UN IDENTIFICADOR O LA PALABRA RESERVADA mut"
    31 "SE ESPERABA LA PALABRA RESERVADA chars"
    32 "SE ESPERABA LA PALABRA RESERVADA nth"
    33 "SE ESPERABA LA PALABRA RESERVADA unwrap"
    34 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS new O from"
    35 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS as_str O trim"
    36 "SE ESPERABA UNA DE LAS PALABRAS RESERVADAS to_string O parse"
    37 "SE ESPERABA UN TIPO NUMERICO:  i64, f64 O usize"
    38 "SE ESPERABA UNA FUNCION MATEMATICA:  sqrt, sin, atan O abs"
    39 "SE ESPERABA UN NUMERO"
    40 "SE ESPERABA LA PALABRA RESERVADA fn"
    41 "DECLARACION DUPLICADA DE IDENTIFICADOR DE BIBLIOTECA, CONSTANTE O FUNCION"
    42 "IDENTIFICADOR NO DECLARADO"
    43 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE"
    44 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE MUTABLE"
    45 "SE ESPERABA UN IDENTIFICADOR DE FUNCION"
    46 "SE ESPERABA UN IDENTIFICADOR DE CONSTANTE O VARIABLE"
    47 "SE ESPERABA QUE LA FUNCION RETORNE UN VALOR"
    48 "SE ESPERABA QUE LA FUNCION NO RETORNE NINGUN VALOR"
    49 "SE ESPERABA UNA COMA O CERRAR UN PARENTESIS:  , O )"
    50 "TIPOS INCOMPATIBLES"
    51 "SE ESPERABA UN IDENTIFICADOR DE VARIABLE DE TIPO String"
    52 "SE ESPERABA ABRIR UN CORCHETE ANGULAR:  <"
    53 "SE ESPERABA CERRAR UN CORCHETE ANGULAR:  >"
    54 "SE ESPERABA UN IDENTIFICADOR DE PUNTERO"
    55 "FALLO EN UNA OPERACION MONADICA"
    56 "FALLO EN UNA OPERACION DIADICA"
    57 "FALLO INDICE INVALIDO EN LA COLECCION"
    58 "SE ESPERABA UN BOOLEANO"
    59 "NO SE ENCONTRO VARIABLE"
    cod)
  )