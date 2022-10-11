(ns vrs.spec-test
  "Test the spec model of VRS objects."
  (:require [clojure.test    :refer [deftest is testing]]
            [clj-http.client :as http]
            [clj-yaml.core   :as yaml]
            [clojure.string  :as str]
            [vrs.digest      :as digest]
            [vrs.spec        :as spec]))

(defn ^:private ednify
  "Return EDN from the YAML file on GitHub."
  [yaml]
  (->> (str yaml ".yaml")
       (conj ["https:" "" "raw.githubusercontent.com"
              "ga4gh" "vrs" "main" "validation"])
       (str/join "/")
       http/get :body yaml/parse-string))

(defn ^:private model-example-out-from-in?
  "Assert OUT is validly derived from IN from some example."
  [in {:keys [ga4gh_digest ga4gh_identify ga4gh_serialize] :as out}]
  (is (every? map? [in out]))
  (is (not (every? nil? [ga4gh_digest ga4gh_identify ga4gh_serialize])))
  (let [serialized (#'digest/ga4gh_serialize in)]
    (is (= ga4gh_serialize (#'digest/ga4gh_serialize in)))
    (when ga4gh_digest
      (is (= ga4gh_digest (#'digest/sha512t24u serialized))))
    (when ga4gh_identify
      (let [id (-> in (#'digest/ga4gh_identify) :_id)]
        (is (spec/curie? id))
        (is (= ga4gh_identify id))))))

(defn ^:private model-example-valid?
  "Assert IN has the type KIND and OUT is validly derived from IN."
  [kind {:keys [in out] :as example}]
  (is (map? example))
  (is (= kind (:type in)))
  (is (spec/valid? in))
  (model-example-out-from-in? in out))

(defn ^:private model-valid?
  "Assert that each of EXAMPLES is a valid instance of KIND."
  [[kind examples]]
  (is (keyword? kind))
  (is (seq? examples))
  (run! (partial model-example-valid? (name kind)) examples))

(deftest models
  (testing "examples in models.yaml"
    (run! model-valid? (ednify "models"))))

(defn ^:private function-example-valid?
  [function {:keys [in out] :as example}]
  (is fn? function)
  (is (map? example))
  (is (map? in))
  (is (= out (-> in :blob function))))

(defn ^:private function-valid?
  [[function examples]]
  (is (keyword? function))
  (is (seq? examples))
  (let [f (ns-resolve 'vrs.digest (symbol (name function)))]
    (run! (partial function-example-valid? f) examples)))

(deftest functions
  (testing "examples in functions.yaml"
    (run! function-valid? (ednify "functions"))))

(clojure.test/test-all-vars *ns*)
