(ns vrs.digest
  "Digest a VRS according to its specification.
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

(defn ^:private canonicalize
  "Return a canonical JSON string for the map M."
  [m]
  (if (map? m)
    (-> codepoints sorted-map-by (into m) jsonify)
    m))

(defn ^:private sha512t24u
  "Base64-encode the truncated SHA-512 digest of string S."
  [s]
  (-> (MessageDigest/getInstance "SHA-512")
      (.digest (.getBytes s))
      (Arrays/copyOf 24)
      (->> (.encodeToString (Base64/getUrlEncoder)))))

(declare ga4gh_digest)                  ; for the parenthetical Python below

;; This implementation mirrors the vrs-python code so we can use the
;; validation suite implemented there.

(defn ^:private dictify
  "Frob VRO like vrs-python's DICTIFY, and digest VRO when ENREF?."
  [enref? vro]
  (letfn [(digestible? [vro] (and enref? (-> vro :type spec/digestible?)))
          (strings?    [vro] (and (sequential? vro) (every? string? vro)))
          (each        [m [k v]] (if (-> k name first (= \_)) m
                                     (assoc m k (dictify true v))))
          (uncurieify  [vro] (-> vro spec/curie? second (or vro)))]
    (cond (strings?    vro)  (->> vro
                                  (map uncurieify)
                                  (sort-by identity codepoints)
                                  (into []))
          (boolean?    vro)  vro
          (digestible? vro)  (ga4gh_digest vro)
          (map?        vro)  (reduce each {} vro)
          (number?     vro)  vro
          (sequential? vro)  (into [] (map (partial dictify true) vro))
          (string?     vro)  (uncurieify vro)
          :else              (throw (ex-info "Cannot serialize" {:vro vro})))))

(defn ga4gh_serialize
  "Implement vrs-python's GA4GH_SERIALIZE for VRO."
  [vro]
  (->> vro (dictify false) canonicalize))

(defn ga4gh_digest
  "Implement vrs-python's GA4GH_DIGEST for VRO."
  [vro]
  (-> vro ga4gh_serialize sha512t24u))

(defn ga4gh_identify
  "Implement vrs-python's GA4GH_IDENTIFY for VRO."
  [{:keys [type] :as vro}]
  (let [digest (-> vro ga4gh_digest)]
    (if-let [prefix (spec/digestible type)]
      (str "ga4gh" prefix \. digest)
      digest)))
