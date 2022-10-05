(ns vrs.spec
  "Clojure spec implementation of VRS data model
  https://vrs.ga4gh.org/en/stable/terms_and_model.html"
  (:require [clojure.spec.alpha :as s]
            [clojure.string     :as str]
            [vrs.digest         :as digest]))

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

(def sequence-regex
  "Match the characters in the aminos and nucleics keys."
  (re-pattern (str "^[" (-> (concat (keys aminos) (keys nucleics))
                            set
                            (->> (map name) (apply str))
                            (str/escape {\- "\\-" \* "\\*"})) "]*$")))

(def curie-regex
  (-> digest/digestible vals
      (->> (map name)
           sort
           (interpose "|")
           (apply str)))
  )

(s/def ::min nat-int?)

(s/def ::max nat-int?)

(s/def ::string (s/and string? seq))

(s/def ::curie ::string)          ; Should be a regex matching a curie

(s/def ::sequence (partial re-matches sequence-regex))

(s/def ::type ::string)         ; Consider limiting to valid vrs types

(s/def ::value int?)            ; Consider limiting to valid vrs types

(s/def ::Number (s/keys :req-un [::type ::value]))

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
