(ns vrs.digest
  "Create computable digest for an entity, per the VRS spec:
  
  https://vrs.ga4gh.org/en/stable/impl-guide/computed_identifiers.html"
  (:require [clojure.walk :as walk]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.security MessageDigest]
           [java.util Base64 Arrays]))

(def identifiable-objects
  #{"Allele"
    "Haplotype"
    "CopyNumber"
    "Text"
    "VariationSet"
    "ChromosomeLocation"
    "SequenceLocation"
    "SequenceInterval"})

(def type-prefixes
  {"Sequence" "SQ"
   "Allele" "VA"
   "Haplotype" "VH"
   "Abundance" "VAB"
   "VariationSet" "VS"
   "SequenceLocation" "VSL"
   "ChromosomeLocation" "VCL"
   "Text" "VT"})

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

(def model-objects
  (-> (io/resource "models.edn") slurp edn/read-string))

(defn- serialize-object [o]
  (reduce (fn [s v] (str s v)) o))

(defn- str->digest [s]
  (.encodeToString (Base64/getEncoder)
                   (-> (MessageDigest/getInstance "SHA-512")
                       (.digest (.getBytes s))
                       (Arrays/copyOf 24))))

(defn- object->vrs-id [o]
  (str
   "ga4gh:"
   (get type-prefixes (get o "type"))
   "."
   (->> (dissoc o "_id")
        seq
        (sort-by key)
        serialize-object
        str->digest)))

(->> the-allele
     walk/stringify-keys
     object->vrs-id)

