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
   :X "Maksed"
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

(spec/def ::number (spec/keys :req-un [::type ::value]))

(comment (spec/valid? ::number {:type "Number" :value 3}))

(spec/def ::min ::number)
(spec/def ::max ::number)
(spec/def ::definite-range (spec/keys :req-un [::type ::min ::max]))

(comment
  (spec/valid? ::definite-range
               {:type "DefiniteRange"
                :min {:type "Number" :value 3}
                :max {:type "Number" :value 5}}))

(spec/def ::comparator #{"<=" ">="})
(spec/def ::indefinite-range (spec/keys :req-un [::comparator ::type ::value]))

(comment
  (spec/valid? ::indefinite-range
               {:type "IndefiniteRange"
                :comparator "<="
                :value 22}))

(spec/def ::literal-sequence-expression
  (spec/keys :req-un [::type ::sequence]))

(comment
  (spec/valid? ::literal-sequence-expression
               {:sequence "ACGT"
                :type "LiteralSequenceExpression"}))

(spec/def ::sequence-interval
  (spec/keys :req-un [::type ::start ::end]))

(comment
  (spec/valid? ::sequence-interval
               {:type "SequenceInterval"
                :start {:type "Number" :value 44908821}
                :end {:type "Number" :value 44908822}}))

;; TODO implememnt simple interval eventually...
(spec/def ::interval ::sequence-interval)
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
(spec/def ::derived-sequence-expression
  (spec/keys :req-un [::type ::location ::reverse_complement]))

(spec/def ::seq_expr (spec/or ::literal-sequence-expression ::literal-sequence-expression
                              ::derived-sequence-expression
                              ::derived-sequence-expression))
(spec/def ::count (spec/or ::number ::number
                           ::indefinite-range ::indefinite-range
                           ::definite-range ::definite-range))

(spec/def ::repeated-sequence-expression
  (spec/keys :req-un [::type ::seq_expr ::count]))

(comment
  (spec/valid? ::repeated-sequence-expression
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
  (spec/or ::literal-sequence-expression ::literal-sequence-expression
           ::repeated-sequence-expression ::repeated-sequence-expression
           ::derived-sequence-expression ::derived-sequence-expression))
(spec/def ::components (spec/coll-of ::component))
(spec/def ::composed-sequence-expression
  (spec/keys :req-un [::type ::components]))

(comment
  (spec/valid? ::composed-sequence-expression
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

(spec/def ::sequence-expression
  (spec/or ::composed-sequence-expression ::composed-sequence-expression
           ::derived-sequence-expression ::derived-sequence-expression
           ::literal-sequence-expression ::literal-sequence-expression))
(spec/def :vrs.spec.allele/location
  (spec/or ::curie ::curie
           ::location ::location))
(spec/def ::state ::sequence-expression)
(spec/def ::allele
  (spec/keys :req-un [::_id ::type :vrs.spec.allele/location ::state]))

(comment
  (spec/valid? ::allele
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

(spec/def ::haplotype-member (spec/or ::allele ::allele
                                      ::curie ::curie))
(spec/def ::members (spec/coll-of ::haplotype-member))
(spec/def ::haplotype
  (spec/keys :req-un [::_id ::type ::members]))

(comment
  (spec/valid? ::haplotype
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

  (spec/valid? ::haplotype
               {:_id "TODO:replacewithvrsid"
                :members
                ["ga4gh:VA.-kUJh47Pu24Y3Wdsk1rXEDKsXWNY-68x"
                 "ga4gh:VA.Z_rYRxpUvwqCLsCBO3YLl70o2uf9_Op1"]
                :type "Haplotype"}))
(spec/def ::molecular-variation
  (spec/or ::allele ::allele
           ::haplotype ::haplotype))

(spec/def ::gene_id ::curie)
(spec/def ::gene
  (spec/keys :req-un [::type ::gene_id]))

(spec/def ::feature ::gene)
(spec/def ::copies
  (spec/or ::number ::number
           ::indefinite-range ::indefinite-range
           ::definite-range ::definite-range))
(spec/def ::subject
  (spec/or ::curie ::curie
           ::feature ::feature
           ::sequence-expression ::sequence-expression
           ::molecular-variation ::molecular-variation))
(spec/def ::copy-number
  (spec/keys :req-un [::_id ::type ::subject ::copies]))

(comment
  (spec/valid?
   ::copy-number
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
(spec/def ::text
  (spec/keys :req_un [::_id ::type ::definition]))

(spec/def ::systemic-variation ::copy-number)
(spec/def ::utility-variation
  (spec/or ::text ::text
           ::variation-set ::variation-set))
(spec/def ::variation
  (spec/or ::molecular-variation ::molecular-variation
           ::systemic-variation ::systemic-variation
           ::utility-variation ::utility-variation))
(spec/def :vrs.spec/member
  (spec/or ::curie ::curie
           ::variation ::variation))
(spec/def :vrs.spec/members
  (spec/coll-of :vrs.spec/member))
(spec/def ::variation-set
  (spec/keys :req_un [::_id ::type :vrs.spec/members]))

(comment
  (spec/valid? ::variation-set
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

(def type->spec
  {"Number" ::number
   "DefiniteRange" ::definite-range
   "IndefiniteRange" ::indefinite-range
   "LiteralSequenceExpression" ::literal-sequence-expression
   "SequenceInterval" ::sequence-interval
   "SequenceExpression" ::sequence-expression
   "DerivedSequenceExpression" ::derived-sequence-expression
   "RepeatedSequenceExpression" ::repeated-sequence-expression
   "ComposedSequenceExpression" ::composed-sequence-expression
   "Allele" ::allele
   "Haplotype" ::haplotype
   "Gene" ::gene
   "CopyNumber" ::copy-number
   "Text" ::text
   "VariationSet" ::variation-set})

(defn valid? [o]
  (spec/valid? (type->spec (:type o)) o))
