(ns vrs.spec
  "Clojure spec implementation of VRS data model
  https://vrs.ga4gh.org/en/stable/terms_and_model.html"
  (:require [clojure.spec.alpha :as spec]))

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

(spec/def ::string (spec/and string? seq))

(spec/def ::curie string?) ; Should be a regex matching a curie

(spec/def ::human-cytoband
  #(re-matches #"^cen|[pq](ter|([1-9][0-9]*(\.[1-9][0-9]*)?))$" %))

(spec/def ::residue
  (-> (concat aminos nucleics)
      keys set
      (->> (map (comp first name)))))

(spec/def ::sequence #(re-matches #"^[A-Z*\-]*$" %))

(spec/def ::type string?) ; Consider limiting to valid vrs types
(spec/def ::value int?)   ; Consider limiting to valid vrs types

(spec/def ::Number (spec/keys :req-un [::type ::value]))

(comment (spec/valid? ::Number {:type "Number" :value 3}))

(spec/def ::min ::Number)
(spec/def ::max ::Number)
(spec/def ::DefiniteRange (spec/keys :req-un [::type ::min ::max]))

(comment
  (spec/valid? ::DefiniteRange
               {:type "DefiniteRange"
                :min {:type "Number" :value 3}
                :max {:type "Number" :value 5}}))

(spec/def ::comparator #{"<=" ">="})
(spec/def ::IndefiniteRange (spec/keys :req-un [::comparator ::type ::value]))

(comment
  (spec/valid? ::IndefiniteRange
               {:type "IndefiniteRange"
                :comparator "<="
                :value 22}))

(spec/def ::LiteralSequenceExpression
  (spec/keys :req-un [::type ::sequence]))

(comment
  (spec/valid? ::LiteralSequenceExpression
               {:sequence "ACGT"
                :type "LiteralSequenceExpression"}))

(spec/def ::SequenceInterval
  (spec/keys :req-un [::type ::start ::end]))

(comment
  (spec/valid? ::SequenceInterval
               {:type "SequenceInterval"
                :start {:type "Number" :value 44908821}
                :end {:type "Number" :value 44908822}}))

;; TODO implememnt simple interval eventually...
(spec/def ::interval ::SequenceInterval)
(spec/def ::_id string?)
(spec/def ::sequence_id string?)
(spec/def ::sequence-location
  (spec/keys :req-un [::_id ::type ::sequence_id ::interval]))

(comment
  (spec/valid? ::sequence-location
               {:_id "TODO:replacewithvrsid"
                :type "SequenceLocation"
                :sequence_id "ga4gh:SQ.IIB53T8CNeJJdUqzn9V_JnRtQadwWCbl"
                :interval {:type "SimpleInterval"
                           :start 44908821
                           :end 44908822}}))

;; TODO, uncomment after sequence location
(spec/def ::reverse_complement boolean?)
(spec/def ::location ::sequence-location)
(spec/def ::DerivedSequenceExpression
  (spec/keys :req-un [::type #_ ::location ::reverse_complement]))

(spec/def ::seq_expr (spec/or ::literal-sequence-expression ::LiteralSequenceExpression
                              ::derived-sequence-expression
                              ::DerivedSequenceExpression))
(spec/def ::count (spec/or ::number ::Number
                           ::indefinite-range ::IndefiniteRange
                           ::definite-range ::DefiniteRange))

(spec/def ::RepeatedSequenceExpression
  (spec/keys :req-un [::type ::seq_expr ::count]))

(comment
  (spec/valid? ::RepeatedSequenceExpression
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

(spec/def ::component
  (spec/or ::literal-sequence-expression ::LiteralSequenceExpression
           ::repeated-sequence-expression ::RepeatedSequenceExpression
           ::derived-sequence-expression ::DerivedSequenceExpression))
(spec/def ::components (spec/coll-of ::component))
(spec/def ::ComposedSequenceExpression
  (spec/keys :req-un [::type ::components]))

(comment
  (spec/valid? ::ComposedSequenceExpression
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

(spec/def ::SequenceExpression
  (spec/or ::composed-sequence-expression ::ComposedSequenceExpression
           ::derived-sequence-expression ::DerivedSequenceExpression
           ::literal-sequence-expression ::LiteralSequenceExpression))
(spec/def :vrs.spec.allele/location
  (spec/or ::curie ::curie
           ::location ::location))
(spec/def ::state ::SequenceExpression)
(spec/def ::Allele
  (spec/keys :req-un [::_id ::type :vrs.spec.allele/location ::state]))

(comment
  (spec/valid? ::Allele
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

(spec/def ::haplotype-member (spec/or ::allele ::Allele
                                      ::curie ::curie))
(spec/def ::members (spec/coll-of ::haplotype-member))
(spec/def ::Haplotype
  (spec/keys :req-un [::_id ::type ::members]))

(comment
  (spec/valid? ::Haplotype
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

  (spec/valid? ::Haplotype
               {:_id "TODO:replacewithvrsid"
                :members
                ["ga4gh:VA.-kUJh47Pu24Y3Wdsk1rXEDKsXWNY-68x"
                 "ga4gh:VA.Z_rYRxpUvwqCLsCBO3YLl70o2uf9_Op1"]
                :type "Haplotype"}))
(spec/def ::molecular-variation
  (spec/or ::allele ::Allele
           ::haplotype ::Haplotype))

(spec/def ::gene_id ::curie)
(spec/def ::Gene
  (spec/keys :req-un [::type ::gene_id]))

(spec/def ::feature ::Gene)
(spec/def ::copies
  (spec/or ::number ::Number
           ::indefinite-range ::IndefiniteRange
           ::definite-range ::DefiniteRange))
(spec/def ::subject
  (spec/or ::curie ::curie
           ::feature ::feature
           ::sequence-expression ::SequenceExpression
           ::molecular-variation ::molecular-variation))
(spec/def ::CopyNumber
  (spec/keys :req-un [::_id ::type ::subject ::copies]))

(spec/def ::RelativeCopyNumber
  (spec/keys :req-un [::type]))

(spec/def ::chr ::string)

(spec/def ::ChromosomeLocation
  (spec/keys :req-un [::chr ::type]))

(comment
  (spec/valid?
   ::CopyNumber
   {:_id "TODO:replacewithvrsid"
    :copies
    {:comparator ">="
     :type "IndefiniteRange"
     :value 3}
    :subject
    {:gene_id "ncbigene:348"
     :type "Gene"}
    :type "CopyNumber"}))

(spec/def ::definition string?)
(spec/def ::Text
  (spec/keys :req_un [::_id ::type ::definition]))

(spec/def ::systemic-variation ::CopyNumber)
(spec/def ::utility-variation
  (spec/or ::text ::Text
           ::variation-set ::VariationSet))
(spec/def ::variation
  (spec/or ::molecular-variation ::molecular-variation
           ::systemic-variation ::systemic-variation
           ::utility-variation ::utility-variation))
(spec/def :vrs.spec/member
  (spec/or ::curie ::curie
           ::variation ::variation))
(spec/def :vrs.spec/members
  (spec/coll-of :vrs.spec/member))
(spec/def ::VariationSet
  (spec/keys :req_un [::_id ::type :vrs.spec/members]))

(comment
  (spec/valid? ::VariationSet
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

(def ^:private the-namespace-name
  "The name of this namespace as a string."
  (name (ns-name *ns*)))

(defn valid? [o]
  (trace o)
  (spec/valid? (keyword the-namespace-name (:type o)) o))
