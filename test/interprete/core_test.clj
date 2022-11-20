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