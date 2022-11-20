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