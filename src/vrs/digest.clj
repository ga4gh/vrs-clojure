(ns vrs.digest
  "Digest a VRS according to this specification.
  https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html"
  (:require [clojure.data.json :as json]
            [clojure.walk      :as walk]
            [clj-yaml.core     :as yaml])
  (:import [clojure.lang Keyword]
           [java.security MessageDigest]
           [java.util Arrays Base64]))

(defmacro trace
  "Like DUMP but map location metadata."
  [expression]
  (let [{:keys [line column]} (meta &form)]
    `(let [x# ~expression]
       (do
         (clojure.pprint/pprint
          {:column ~column :file ~*file* :line ~line '~expression x#})
         x#))))

(def digestible
  "Map a digestible type to an identifier prefix as a keyword."
  {"Allele"             :VA
   "ChromosomeLocation" :VCL
   "CopyNumber"         :VCN
   "Genotype"           :VGT
   "Haplotype"          :VH
   "SequenceLocation"   :VSL
   "Text"               :VT
   "VariationSet"       :VS})

(def digestible?
  "Set of the types that can be digested."
  (set (keys digestible)))

(def indigestible?
  "Set of the types that cannot be digested."
  #{"CURIE"
    "CompositeSequenceExpression"
    "CytobandInterval"
    "DefiniteRange"
    "DerivedSequenceExpression"
    "Gene"
    "HumanCytoband"
    "IndefiniteRange"
    "LiteralSequenceExpression"
    "Number"
    "RepeatedSequenceExpression"
    "Residue"
    "Sequence"
    "SequenceInterval"})

(def obsolete?
  "Other now obsolete indigestible types."
  #{"AbsoluteCopyNumber"
    "Abundance"                         ; :VAB not anymore ...
    "GenotypeMember"
    "IndefiniteRange"
    "RelativeCopyNumber"
    "SequenceState"
    "SimpleInterval"
    "State"})

(def type?
  "The set of all type names -- even obsolete ones."
  (-> obsolete?
      (into indigestible?)
      (into digestible?)))

(defn keyword->codepoint-seq
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

(defn ^:private jsonify
  "Return a canonical JSON string for the map M."
  [m]
  (-> codepoints sorted-map-by
      (into (remove #(-> % first name first (= \_)) m))
      (json/write-str :escape-slash false)))

(jsonify allele)

(defn ^:private digest
  "Base64-encode the SHA-512 digest of string S."
  [s]
  (-> (MessageDigest/getInstance "SHA-512")
      (.digest (.getBytes s))
      (Arrays/copyOf 24)
      (->> (.encodeToString (Base64/getUrlEncoder)))))

(defn ^:private idify
  "Add an _ID field to THING mapped to its digest when digestible."
  [{:keys [type] :as thing}]
  (let [prefix (digestible type)]
    (cond (keyword? prefix)    (-> thing jsonify digest
                                   (->> (str "ga4gh" prefix \.)
                                        (assoc thing :_id)))
          (indigestible? type) thing
          (map? thing)         (trace thing)
          :else                thing)))

;; https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html#identify
;;
(defn identify
  "Digest a VRS object."
  [vrs]
  (walk/postwalk idify vrs))
