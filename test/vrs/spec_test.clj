(ns vrs.spec-test
  (:require [vrs.spec :as vrs-spec]
            [clojure.test :refer :all]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(def model-objects
  (-> (io/resource "models.edn") slurp edn/read-string))

(deftest spec-test
  (testing "testing validation of model objects"
    (run! #(is vrs-spec/valid? (:in %)) (vals model-objects))))
