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

(defn ^:private frob
  "Do whatever the GA4GH VRS says to do to a [K V] field."
  [[k v]]
  (spec/trace ['frob k v])
  (if (string? v)
    (let [[curie? _ga4gh _type digest] (re-matches spec/curie-regex v)]
      (spec/trace ['frob curie? digest])
      (if curie? [k digest] [k v]))
    [k v]))

(defn ^:private serialize
  "Return a canonical JSON string for the map M."
  [m]
  (spec/trace ['serialize m])
  (if (map? m)
    (-> codepoints sorted-map-by
        (into (->> m
                   (remove #(-> % first name first (= \_)))
                   (map frob))))
    m))

(defn ga4gh_serialize
  "Satisfy the ga4gh_serialize validation on VRS for testing."
  [vrs]
  (spec/trace ['ga4gh_serialize vrs])
  (json/write-str (walk/postwalk serialize vrs) :escape-slash false))

(defn ^:private sha512t24u
  "Base64-encode the SHA-512 digest of string S."
  [s]
  (-> (MessageDigest/getInstance "SHA-512")
      (.digest (.getBytes s))
      (Arrays/copyOf 24)
      (->> (.encodeToString (Base64/getUrlEncoder)))))

(defn ^:private ga4gh_identify
  "Add an _ID field to THING mapped to its digest when digestible."
  [{:keys [type] :as thing}]
  (let [prefix (spec/digestible type)]
    (cond (keyword? prefix)         (-> thing serialize
                                        (json/write-str :escape-slash false)
                                        sha512t24u
                                        (->> (str "ga4gh" prefix \.)
                                             (assoc thing :_id)))
          (spec/indigestible? type) thing
          (map? thing)              (spec/trace thing)
          :else                     thing)))

;; https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html#identify
;;
(defn identify
  "Digest a VRS object."
  [vrs]
  (walk/postwalk ga4gh_identify vrs))
