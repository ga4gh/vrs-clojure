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

(declare ga4gh_digest)                  ; for parenthetical Python

(defn ^:private dictify
  "Frob IT like vrs-python's dictify(:sigh:), and DIGEST? when true."
  [digest? it]
  (letfn [(each [m [k v]] (if (-> k name first (= \_)) m
                              (assoc m k (dictify true v))))
          (digestible? [it] (and digest? (-> it :type spec/digestible?)))
          (uncurieify  [it] (-> it spec/curie? second (or it)))]
    (cond (boolean?    it)  it
          (digestible? it)  (ga4gh_digest it)
          (map?        it)  (reduce each {} it)
          (number?     it)  it
          (sequential? it)  (into [] (if (every? spec/curie? it)
                                       (->> it
                                            (map uncurieify)
                                            (sort-by identity codepoints))
                                       (map (partial dictify true) it)))
          (string?     it)  (uncurieify it)
          :else             (throw (ex-info "Cannot serialize" {:it it})))))

(defn ^:private canonicalize
  "Return a canonical JSON string for the map M."
  [m]
  (if (map? m)
    (-> codepoints sorted-map-by (into m) jsonify)
    m))

(defn ga4gh_serialize
  [vrs]
  (->> vrs (dictify false) canonicalize))

(defn ga4gh_digest
  [vrs]
  (-> vrs ga4gh_serialize sha512t24u))
