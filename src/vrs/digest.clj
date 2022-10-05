(ns vrs.digest
  "Digest a VRS according to this specification.
  https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html"
  (:require [clojure.data.json :as json]
            [clojure.pprint    :refer [pprint]]
            [clojure.walk      :as walk]
            [clj-yaml.core     :as yaml])
  (:import [clojure.lang Keyword]
           [java.security MessageDigest]
           [java.util Arrays Base64 Base64$Encoder]))

(-> (java.util.Base64/getUrlEncoder)
    class
    (. getDeclaredField "toBase64URL"))

(-> (java.util.Base64/getUrlEncoder)
    clojure.reflect/reflect
    :members
    (->> (filter #(= 'toBase64URL (:name %)))))

#_java.util.Base64$Encoder/toBase64URL

;; => #object[java.lang.reflect.Field 0x44c125c7 "private static final char[] java.util.Base64$Encoder.toBase64URL"]

(def ^:private haplotype
  {:_id "TODO:replacewithvrsid"
   :members
   [{:_id "TODO:replacewithvrsid"
     :location
     {:_id "TODO:replacewithvrsid"
      :interval
      {:end {:type "Number" :value 44908822}
       :start {:type "Number" :value 44908821}
       :type "SequenceInterval"}
      :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl"
      :type "SequenceLocation"}
     :state {:sequence "C" :type "LiteralSequenceExpression"}
     :type "Allele"}
    {:_id "TODO:replacewithvrsid"
     :location
     {:_id "TODO:replacewithvrsid"
      :interval
      {:end {:type "Number" :value 44908684}
       :start {:type "Number" :value 44908683}
       :type "SequenceInterval"}
      :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl"
      :type "SequenceLocation"}
     :state {:sequence "C" :type "LiteralSequenceExpression"}
     :type "Allele"}]
   :type "Haplotype"})

(defmacro trace
  "Like DUMP but map location metadata."
  [expression]
  (let [{:keys [line column]} (meta &form)]
    `(let [x# ~expression]
       (do
         (pprint {:column ~column :file ~*file* :line ~line '~expression x#})
         x#))))

(def digestible
  "Map a digestible type to an identifier prefix as a keyword."
  {"Abundance"          :VAB
   "Allele"             :VA
   "ChromosomeLocation" :VCL
   "CopyNumber"         :VCN
   "Genotype"           :VGT
   "Haplotype"          :VH
   "SequenceLocation"   :VSL
   "Text"               :VT
   "VariationSet"       :VS})


(def indigestible?
  "Set of the types that cannot be digested."
  #{"CURIE"
    "CompositeSequenceExpression"
    "CytobandInterval"
    "DefiniteRange"
    "DerivedSequenceExpression"
    "Gene"
    "GenotypeMember"
    "HumanCytoband"
    "IndefiniteRange"
    "LiteralSequenceExpression"
    "Number"
    "RepeatedSequenceExpression"
    "Residue"
    "Sequence"
    "SequenceInterval"})

(def obsolete
  "Other now obsolete indigestible types."
  #{"SequenceState"
    "SimpleInterval"
    "State"})

(def allele
  "An example Allele VRS from the spec."
  {:_id "ga4gh:VA._YNe5V9kyydfkGU0NRyCMHDSKHL4YNvc"
   :location
   {:interval
    {:end {:type "Number" :value 44908822}
     :start {:type "Number" :value 44908821}
     :type "SequenceInterval"}
    :sequence_id "refseq:NC_000019.10"
    :type "SequenceLocation"}
   :state {:sequence "T" :type "LiteralSequenceExpression"}
   :type "Allele"})

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
          (map? thing)         (dump thing)
          :else                thing)))

;; https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html#identify

(defn identify
  "Digest a VRS object."
  [vrs]
  (walk/postwalk idify vrs))

{:_id "ga4gh:VA._YNe5V9kyydfkGU0NRyCMHDSKHL4YNvc"
 :location
 {:interval
  {:end {:type "Number" :value 44908822}
   :start {:type "Number" :value 44908821}
   :type "SequenceInterval"}
  :sequence_id "refseq:NC_000019.10"
  :type "SequenceLocation"}
 :state {:sequence "T" :type "LiteralSequenceExpression"}
 :type "Allele"}

(jsonify allele)
(identify allele)
(identify haplotype)

(def DkZLLMnwoH6zIncSRh2c05nzCNLdTqHl
  "JSON VRS with ID ga4gh:VA.DkZLLMnwoH6zIncSRh2c05nzCNLdTqHl."
  "{
    \"_id\": \"ga4gh:VA.DkZLLMnwoH6zIncSRh2c05nzCNLdTqHl\",
    \"type\": \"Allele\",
    \"location\": {
        \"type\": \"SequenceLocation\",
        \"sequence_id\": \"ga4gh:SQ._0wi-qoDrvram155UmcSC-zA5ZK4fpLT\",
        \"interval\": {
            \"type\": \"SequenceInterval\",
            \"start\": {
                \"type\": \"Number\",
                \"value\": 32936731
            },
            \"end\": {
                \"type\": \"Number\",
                \"value\": 32936732
            }
        }
    },
    \"state\": {
        \"type\": \"LiteralSequenceExpression\",
        \"sequence\": \"C\"
    }
   }")

(def _YNe5V9kyydfkGU0NRyCMHDSKHL4YNvc
  "JSON VRS with ID ga4gh:VA._YNe5V9kyydfkGU0NRyCMHDSKHL4YNvc."
  "{
    \"_id\": \"ga4gh:VA._YNe5V9kyydfkGU0NRyCMHDSKHL4YNvc\",
    \"location\": {
      \"interval\": {
        \"end\": {
          \"type\": \"Number\",
          \"value\": 44908822
        },
        \"start\": {
          \"type\": \"Number\",
          \"value\": 44908821
        },
        \"type\": \"SequenceInterval\"
      },
      \"sequence_id\": \"refseq:NC_000019.10\",
      \"type\": \"SequenceLocation\"
    },
    \"state\": {
      \"sequence\": \"T\",
      \"type\": \"LiteralSequenceExpression\"
    },
    \"type\": \"Allele\"
   }")

(-> "./validation/functions.yaml" slurp yaml/parse-string)
(-> "./validation/models.yaml" slurp yaml/parse-string)
