(ns vrs.digest
  "Digest a VRS according to this specification.
  https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html"
  (:require [clojure.edn     :as edn]
            [clojure.java.io :as io])
  (:import [java.security MessageDigest]
           [java.util Base64 Arrays]))

;; HACK: Use ":" when there is no type identifier prefix.
;;
(def kinds
  "Map an object type name to a type identifier prefix as a keyword."
  {"Abundance"          :VAB
   "Allele"             :VA
   "ChromosomeLocation" :VCL
   "CopyNumber"         :VCN
   "Haplotype"          :VH
   "Sequence"           :SQ
   "SequenceInterval"   ":"
   "SequenceLocation"   :VSL
   "Text"               :VT
   "VariationSet"       :VS})

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

(defn- str->digest [s]
  (.encodeToString (Base64/getEncoder)
                   (-> (MessageDigest/getInstance "SHA-512")
                       (.digest (.getBytes s))
                       (Arrays/copyOf 24))))

(defn- object->vrs-id [o]
  (str "ga4gh" (kinds (:type o)) "."
       (-> o (dissoc :_id)
           (->> (sort-by key)
                serialize-object
                str->digest))))

(object->vrs-id the-allele)
