(ns vrs.digest
  "Digest a VRS according to this specification.
  https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html"
  (:require [clojure.data.json :as json]
            [clojure.walk      :as walk]
            [clj-yaml.core     :as yaml]
            [vrs.spec          :as spec])
  (:import [clojure.lang Keyword]
           [java.security MessageDigest]
           [java.util Arrays Base64]))

(defn ^:private jsonify
  "Return a string containing the JSON projection of EDN."
  [edn]
  (json/write-str edn :escape-slash false))

(defn ^:private keyword->codepoint-seq
  "Return a codepoint (integer) iterator on the name of KW."
  [^Keyword kw]
  (-> kw name .codePoints .iterator iterator-seq))

(defn ^:private codepoints
  "Compare keys LEFT and RIGHT codepoint by codepoint in UTF-8."
  [left right]
  (loop [seqL (keyword->codepoint-seq left)
         seqR (keyword->codepoint-seq right)]
    (let [cpL (first seqL) cpR (first seqR)]
      (cond (and (nil? cpL) (nil? cpR))  0
            (nil? cpL)                  -1
            (nil? cpR)                   1
            (< cpL cpR)                 -1
            (> cpL cpR)                  1
            :else (recur (rest seqL) (rest seqR))))))

(defn ^:private sha512t24u
  "Base64-encode the truncated SHA-512 digest of string S."
  [s]
  (-> (MessageDigest/getInstance "SHA-512")
      (.digest (.getBytes s))
      (Arrays/copyOf 24)
      (->> (.encodeToString (Base64/getUrlEncoder)))))

(defn ^:private digest
  "Return IT or ITs digest when ITs TYPE is digestible."
  [{:keys [type] :as it}]
  (if (spec/digestible? type)
    (-> it spec/trace jsonify sha512t24u)
    it))

(defn ^:private dictify
  "Adjust IT according to vrs-python's dictify(:sigh:)."
  [it]
  (letfn [(each [m [k v]] (if (-> k name first (= \_)) m
                              (assoc m k (dictify v))))]
    (cond (map?        it) (reduce each {} it)
          (sequential? it) (into [] (if (every? spec/curie? it)
                                      (sort it)
                                      (map dictify it)))
          (string?     it) (-> it spec/curie? second (or it))
          :else        it)))

(defn ga4gh_serialize
  [vrs]
  (->> vrs (walk/postwalk dictify) jsonify))
