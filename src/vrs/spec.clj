(ns vrs.spec
  "Clojure spec implementation of VRS data model
  https://vrs.ga4gh.org/en/stable/terms_and_model.html"
  (:require [clojure.spec.alpha :as s]))

;; https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry#Amino_acid_and_nucleotide_base_codes

(def nucleics
  "The IUPAC nucleic acid codes."
  {:A "Adenine"
   :C "Cytosine"
   :G "Guanine"
   :T "Thymine"
   :U "Uracil"
   :R "A or G (purine)"
   :Y "C or T (pyrimidine)"
   :K "G or T (ketone bases)"
   :M "A or C (amino groups)"
   :S "C or G (strong interaction)"
   :W "A or T (or U) (weak interaction)"
   :B "Not A (C or G or T or U)"
   :D "Not C (A or G or T or U)"
   :H "Not G (A or C or T or U)"
   :V "Not T nor U (A or C or G)"
   :N "Any (A or C or G or T or U)"
   :X "Masked"
   :- "Gap"})

(def aminos
  "The IUPAC amino acid codes."
  {:A "Alanine"
   :B "Aspartic acid or Asparagine"
   :C "Cysteine"
   :D "Aspartic acid"
   :E "Glutamic acid"
   :F "Phenylalanine"
   :G "Glycine"
   :H "Histidine"
   :I "Isoleucine"
   :K "Lysine"
   :L "Leucine"
   :M "Methionine"
   :N "Asparagine"
   :O "Pyrrolysine"
   :P "Proline"
   :Q "Glutamine"
   :R "Arginine"
   :S "Serine"
   :T "Threonine"
   :U "Selenocysteine"
   :V "Valine"
   :W "Tryptophan"
   :Y "Tyrosine"
   :Z "Glutamic acid or Glutamine"
   :J "Leucine or isoleucine"
   :X "Any (unknown)"
   :* "Translation stop"
   :- "Gap"})

(s/def ::string (s/and string? seq))

(s/def ::curie string?) ; Should be a regex matching a curie

(s/def ::human-cytoband
  #(re-matches #"^cen|[pq](ter|([1-9][0-9]*(\.[1-9][0-9]*)?))$" %))

(s/def ::residue
  (-> (concat aminos nucleics)
      keys set
      (->> (map (comp first name)))))

(s/def ::sequence #(re-matches #"^[A-Z*\-]*$" %))

(s/def ::type string?) ; Consider limiting to valid vrs types
(s/def ::value int?)   ; Consider limiting to valid vrs types

(s/def ::Number (s/keys :req-un [::type ::value]))

(s/def ::min nat-int?)

(s/def ::max nat-int?)

(s/def ::DefiniteRange
  (s/keys :req-un [::max ::min ::type]))

(s/def ::comparator #{"<=" ">="})
(s/def ::IndefiniteRange (s/keys :req-un [::comparator ::type ::value]))

(s/def ::LiteralSequenceExpression
  (s/keys :req-un [::sequence ::type]))

(s/def ::SequenceInterval
  (s/keys :req-un [::type ::start ::end]))

(s/def ::interval ::SequenceInterval)

(s/def ::_id string?)

(s/def ::sequence_id string?)

(s/def ::SequenceLocation
  (s/keys :opt-un [::_id]
          :req-un [::type ::sequence_id ::interval]))

;; TODO, uncomment after sequence location
(s/def ::reverse_complement boolean?)

(s/def ::location ::SequenceLocation)

(s/def ::DerivedSequenceExpression
  (s/keys :req-un [#_::location ::reverse_complement ::type]))

(s/def ::seq_expr
  (s/or ::derived-sequence-expression ::DerivedSequenceExpression
           ::literal-sequence-expression ::LiteralSequenceExpression))

(s/def ::count (s/or ::definite-range   ::DefiniteRange
                           ::indefinite-range ::IndefiniteRange
                           ::number           ::Number))

(s/def ::RepeatedSequenceExpression
  (s/keys :req-un [::count #_ ::seq_expr ::type]))

(s/def ::component
  (s/or ::literal-sequence-expression ::LiteralSequenceExpression
        ::repeated-sequence-expression ::RepeatedSequenceExpression
        ::derived-sequence-expression ::DerivedSequenceExpression))
(s/def ::components (s/coll-of ::component))
(s/def ::ComposedSequenceExpression
  (s/keys :req-un [::type ::components]))

(s/def ::SequenceExpression
  (s/or ::composed-sequence-expression ::ComposedSequenceExpression
        ::derived-sequence-expression ::DerivedSequenceExpression
        ::literal-sequence-expression ::LiteralSequenceExpression))

(s/def :vrs.spec.allele/location
  (s/or ::curie ::curie
           ::location ::location))

(s/def ::state ::SequenceExpression)

(s/def ::Allele
  (s/keys :opt-un [::_id]
          :req-un [::state ::type :vrs.spec.allele/location]))

(s/def ::haplotype-member (s/or ::allele ::Allele
                                ::curie ::curie))
(s/def ::members (s/coll-of ::haplotype-member))

(s/def ::Haplotype
  (s/keys :opt-un [::_id]
          :req-un [::type ::members]))

(s/def ::molecular-variation
  (s/or ::allele ::Allele
        ::haplotype ::Haplotype))

(s/def ::gene_id ::curie)
(s/def ::Gene
  (s/keys :req-un [::type ::gene_id]))

(s/def ::feature ::Gene)

(s/def ::copies
  (s/or ::number ::Number
           ::indefinite-range ::IndefiniteRange
           ::definite-range ::DefiniteRange))

(s/def ::subject
  (s/or ::curie ::curie
           ::feature ::feature
           ::sequence-expression ::SequenceExpression
           ::molecular-variation ::molecular-variation))

(s/def ::CopyNumber
  (s/keys :req-un [::_id ::type ::subject ::copies]))

(s/def ::RelativeCopyNumber
  (s/keys :req-un [::type]))

(s/def ::chr ::string)

(s/def ::ChromosomeLocation
  (s/keys :req-un [::chr ::type]))

(s/def ::definition string?)

(s/def ::Text
  (s/keys :req_un [::_id ::type ::definition]))

(s/def ::systemic-variation ::CopyNumber)

(s/def ::utility-variation
  (s/or ::text ::Text
           ::variation-set ::VariationSet))

(s/def ::variation
  (s/or ::molecular-variation ::molecular-variation
           ::systemic-variation ::systemic-variation
           ::utility-variation ::utility-variation))

(s/def ::member
  (s/or ::curie ::curie
           ::variation ::variation))

(s/def ::members
  (s/coll-of ::member))

(s/def ::VariationSet
  (s/keys :req_un [::_id ::type ::members]))


(s/def ::AbsoluteCopyNumber
  (s/keys :opt-un [::_id]
          :req-un [::type ::subject ::copies]))

(s/def ::SimpleInterval
  (s/keys :opt-un [::_id]
          :req-un [::type ::start ::end]))

(s/def ::CytobandInterval
  (s/keys :req-un [::type ::start ::end]))

(def ^:private the-namespace-name
  "The name of this namespace as a string."
  (name (ns-name *ns*)))

(defn valid? [o]
  (s/valid? (keyword the-namespace-name (:type o)) o))

(comment (s/valid? ::Number {:type "Number" :value 3}))

(comment
  (s/valid? ::DefiniteRange
            {:type "DefiniteRange"
             :min {:type "Number" :value 3}
             :max {:type "Number" :value 5}}))

(comment
  (s/valid? ::IndefiniteRange
            {:type "IndefiniteRange"
             :comparator "<="
             :value 22}))

(comment
  (s/valid? ::LiteralSequenceExpression
            {:sequence "ACGT"
             :type "LiteralSequenceExpression"}))
(comment
  (s/valid? ::SequenceInterval
            {:type "SequenceInterval"
             :start {:type "Number" :value 44908821}
             :end {:type "Number" :value 44908822}}))
(comment
  (s/valid? ::SequenceLocation
            {:_id "TODO:replacewithvrsid"
             :type "SequenceLocation"
             :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl"
             :interval {:type "SimpleInterval"
                        :start 44908821
                        :end 44908822}}))

(comment
  (s/valid? ::RepeatedSequenceExpression
            {:count {:comparator ">=", :type "IndefiniteRange", :value 6},
             :seq_expr
             {:location
              {:interval
               {:end {:type "Number", :value 44908822},
                :start {:type "Number", :value 44908821},
                :type "SequenceInterval"},
               :_id "TODO:replacewithvrsid",
               :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl",
               :type "SequenceLocation"},
              :reverse_complement false,
              :type "DerivedSequenceExpression"},
             :type "RepeatedSequenceExpression"}))

(comment
  (s/valid? ::ComposedSequenceExpression
            {:type "ComposedSequenceExpression",
             :components
             [{:type "RepeatedSequenceExpression",
               :seq_expr {:type "LiteralSequenceExpression",
                          :sequence "GCG"},
               :count {:type "Number", :value 11}}
              {:type "RepeatedSequenceExpression",
               :seq_expr {:type "LiteralSequenceExpression",
                          :sequence "GCA"},
               :count {:type "Number", :value 3}}
              {:type "RepeatedSequenceExpression",
               :seq_expr {:type "LiteralSequenceExpression",
                          :sequence "GCG"},
               :count {:type "Number", :value 1}}]}))

(comment
  (s/valid? ::Allele
            {:_id "TODO:replacewithvrsid"
             :location
             {:interval
              {:end {:type "Number", :value 44908822},
               :start {:type "Number", :value 44908821},
               :type "SequenceInterval"},
              :_id "TODO:replacewithvrsid"
              :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl",
              :type "SequenceLocation"},
             :state {:sequence "T", :type "SequenceState"},
             :type "Allele"}))
(comment
  (s/valid? ::Haplotype
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

  (s/valid? ::Haplotype
            {:_id "TODO:replacewithvrsid"
             :members
             ["ga4gh:VA.-kUJh47Pu24Y3Wdsk1rXEDKsXWNY-68x"
              "ga4gh:VA.Z_rYRxpUvwqCLsCBO3YLl70o2uf9_Op1"]
             :type "Haplotype"}))
(comment
  (s/valid? ::CopyNumber
            {:_id "TODO:replacewithvrsid"
             :copies
             {:comparator ">="
              :type "IndefiniteRange"
              :value 3}
             :subject
             {:gene_id "ncbigene:348"
              :type "Gene"}
             :type "CopyNumber"}))

(comment
  (s/valid? ::VariationSet
            {:members
             [{:location
               {:interval
                {:end {:type "Number", :value 44908822},
                 :start {:type "Number", :value 44908821},
                 :type "SequenceInterval"},
                :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl",
                :type "SequenceLocation"},
               :state {:sequence "C", :type "LiteralSequenceExpression"},
               :type "Allele"}
              {:location
               {:interval
                {:end {:type "Number", :value 44908684},
                 :start {:type "Number", :value 44908683},
                 :type "SequenceInterval"},
                :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl",
                :type "SequenceLocation"},
               :state {:sequence "C", :type "LiteralSequenceExpression"},
               :type "Allele"}],
             :type "VariationSet"}))
