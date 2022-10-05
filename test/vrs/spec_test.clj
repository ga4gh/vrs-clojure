(ns vrs.spec-test
  "Test the spec model of VRS objects."
  (:require [clojure.test    :refer [deftest is testing]]
            [clojure.edn     :as edn]
            [clojure.java.io :as io]
            [vrs.spec        :as spec]))

(def model-objects
  "A seq of seqs of verified VRS objects."
  (-> "models.edn" io/resource slurp edn/read-string))

(deftest spec-test
  (testing "testing validation of model objects"
    (run! #(is (spec/valid? (:in %))) (map first (vals model-objects)))))

(clojure.test/test-all-vars *ns*)
