(ns vrs.spec-test
  (:require [clojure.test    :refer [deftest is testing]]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [vrs.spec        :as vrs-spec]))

(def model-objects
  (-> "models.edn" io/resource slurp edn/read-string))

(deftest spec-test
  (testing "testing validation of model objects"
    (run! #(is (vrs-spec/valid? (:in %))) (map first (vals model-objects)))))

(clojure.test/test-all-vars *ns*)
