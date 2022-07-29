(ns vrs.digest
  "Digest a VRS according to this specification.
  https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html"
  (:require [clojure.data.json       :as json]
            [clojure.edn             :as edn]
            [clojure.java.io         :as io]
            [clojure.pprint          :refer [pprint]]
            [clojure.string          :as str]
            [clojure.zip             :as zip])
  (:import [java.security MessageDigest]
           [java.util Arrays Base64]))

(defmacro dump
  "Dump [EXPRESSION VALUE] where VALUE is EXPRESSION's value."
  [expression]
  `(let [x# ~expression]
     (do
       (pprint ['~expression x#])
       x#)))

;; Q: Is SequenceInterval a "leaf" map in the schema?
;; A "leaf" map is a map that cannot be replaced with an :_id?
;; SequenceInterval does not have a type identifier prefix?
;; If so, what other types are there?

(def kind
  "Map an object type name to a type identifier prefix as a keyword."
  {"Abundance"          :VAB
   "Allele"             :VA
   "ChromosomeLocation" :VCL
   "CopyNumber"         :VCN
   "Haplotype"          :VH
   "Sequence"           :SQ
   "SequenceInterval"   nil             ; HACK
   "SequenceLocation"   :VSL
   "Text"               :VT
   "VariationSet"       :VS})

(def allele
  "An example Allele VRS from the spec."
  {:type     "Allele"
   :location {:interval    {:end   {:type  "Number"
                                    :value 44908822}
                            :start {:type  "Number"
                                    :value 44908821}
                            :type  "SequenceInterval"}
              :sequence_id "refseq:NC_000019.10"
              :type        "SequenceLocation"}
   :state    {:sequence "T"
              :type     "LiteralSequenceExpression"}})

(defn codepoints
  "Compare keys LEFT and RIGHT codepoint by codepoint in UTF-8."
  [left right]
  (loop [seqL (-> left  name .codePoints .iterator iterator-seq)
         seqR (-> right name .codePoints .iterator iterator-seq)]
    (let [cpL (first seqL) cpR (first seqR)]
      (cond (and (nil? cpL) (nil? cpR))  0
            (nil? cpL)                  -1
            (nil? cpR)                   1
            (< cpL cpR)                 -1
            (> cpL cpR)                  1
            :else (recur (rest seqL) (rest seqR))))))

(defn jsonify
  "Return a canonical JSON string for the map M."
  [m]
  (-> codepoints sorted-map-by
      (into (remove #(-> % first name first (= \_)) m))
      (json/write-str :escape-slash false)))

(jsonify allele)

(def the-allele
  {:_id "TODO:replacewithvrsid"
   :members
   [{:_id "TODO:replacewithvrsid"
     :location
     {:_id "TODO:replacewithvrsid"
      :interval
      {:end {:type "Number", :value 44908822},
       :start {:type "Number", :value 44908821},
       :type "SequenceInterval"},
      :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl",
      :type "SequenceLocation"},
     :state {:sequence "C", :type "LiteralSequenceExpression"},
     :type "Allele"}
    {:_id "TODO:replacewithvrsid"
     :location
     {:_id "TODO:replacewithvrsid"
      :interval
      {:end {:type "Number", :value 44908684},
       :start {:type "Number", :value 44908683},
       :type "SequenceInterval"},
      :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl",
      :type "SequenceLocation"},
     :state {:sequence "C", :type "LiteralSequenceExpression"},
     :type "Allele"}],
   :type "Haplotype"})

(defn- serialize-object [o]
  (reduce (fn [s v] (str s v)) o))

(defn digest
  "Base64-encode the SHA-512 digest of string S."
  [s]
  (.encodeToString (Base64/getEncoder)
                   (-> (MessageDigest/getInstance "SHA-512")
                       (.digest (.getBytes s))
                       (Arrays/copyOf 24))))

(defn- object->vrs-id [o]
  (str "ga4gh" (kind (:type o)) "."
       (-> o (dissoc :_id)
           (->> (sort-by key)
                serialize-object
                digest))))

(object->vrs-id the-allele)

;; https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html#identify

(defn ^:private digest
  "Digest a VRS object."
  [vrs]
  (letfn [(branch? [node] (or   (map? node) (vector? node)))
          (leaf?   [node] (nil? (-> node :type kind)))
          (make    [node children] (into (empty node) children))]
    (loop [loc (zip/zipper branch? seq make vrs)]
      (if (zip/end? loc) (zip/root loc)
          (let [node (zip/node loc)]
            (recur (zip/next
                    (if (leaf? node)
                      loc
                      loc))))))))
