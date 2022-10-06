(ns vrs.spec
  "Clojure spec implementation of VRS data model
  https://vrs.ga4gh.org/en/stable/terms_and_model.html"
  (:require [clojure.spec.alpha :as s]
            [clojure.string     :as str]
            [vrs.digest         :as digest]))

(def ^:private the-namespace-name
  "The name of this namespace as a string for `valid?` below."
  (name (ns-name *ns*)))

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
  (-> (concat (keys aminos) (keys nucleics)) set
      (->> (map name) (apply str))
      (str/escape {\- "\\-" \* "\\*"})
      (interpose ["^[" "]*$"])
      (->> (apply str))
      re-pattern))

;; Recent JDKs put java.util.Base64$Encoder/toBase64URL off limits!
;;
(def curie-regex
  "Match a CURIE to 3 groups: 'ga4gh':'type'.'digest'."
  (let [ga4gh           "(ga4gh)"
        url-safe-base64 "[a-zA-Z_-]"
        hash-size       24
        hash-group      (str "(" url-safe-base64 "{" hash-size "})")]
    (-> digest/digestible vals
        (->> (map name) sort (interpose "|") (apply str)) ; type
        (interpose [(str "^" ga4gh ":(") (str ")\\." hash-group "$")])
        (->> (apply str))
        re-pattern)))

(defn curie?
  "True when OBJECT is a CURIE."
  [object]
  (try (re-matches curie-regex object)
       (catch Throwable _)))

;; https://vrs.ga4gh.org/en/latest/terms_and_model.html#humancytoband
;;
(defn iscn?
  [object]
  (try (re-matches #"^cen|[pq](ter|([1-9][0-9]*(\.[1-9][0-9]*)?))$" object)
       (catch Throwable _)))

(defn sequence?
  "True when OBJECT is a sequence."
  [object]
  (try (re-matches sequence-regex object)
       (catch Throwable _)))

(s/def ::min nat-int?)

(s/def ::max nat-int?)

(s/def ::value int?)

(s/def ::reverse_complement boolean?)

(s/def ::comparator #{"<=" ">="})

(s/def ::string (s/and string? seq))

(s/def ::chr ::string)

(s/def ::definition ::string)

(s/def ::gene_id ::string)

(s/def ::relative_copy_class ::string)

(s/def ::sequence_id ::string)

(s/def ::species_id ::string)

(s/def ::CURIE curie?)

(s/def ::sequence sequence?)

(s/def ::type digest/type?)

(s/def ::LiteralSequenceExpression
  (s/keys :req-un [::sequence ::type]))

(s/def ::Number
  (s/keys :req-un [::type ::value]))

(s/def ::_id ::CURIE)

(s/def ::DefiniteRange
  (s/keys :req-un [::max ::min ::type]))

(s/def ::IndefiniteRange
  (s/keys :req-un [::comparator ::type ::value]))

(s/def ::range
  (s/or  ::definite-range   ::DefiniteRange
         ::indefinite-range ::IndefiniteRange
         ::number           ::Number))

(s/def :vrs.spec.iscn/end       iscn?)
(s/def :vrs.spec.iscn/start     iscn?)

(s/def :vrs.spec.sequence/end   ::range)
(s/def :vrs.spec.sequence/start ::range)

(s/def :vrs.spec.simple/end     nat-int?)
(s/def :vrs.spec.simple/start   nat-int?)

(s/def ::copies ::range)

(s/def ::CytobandInterval
  (s/keys :req-un [::type :vrs.spec.iscn/end :vrs.spec.iscn/start]))

(s/def ::SimpleInterval
  (s/keys :opt-un [::_id]
          :req-un [::type :vrs.spec.simple/end :vrs.spec.simple/start]))

(s/def ::count
  (s/or ::definite-range   ::DefiniteRange
        ::indefinite-range ::IndefiniteRange
        ::number           ::Number))

(s/def ::LiteralSequenceExpression
  (s/keys :req-un [::sequence ::type]))

(s/def ::SequenceInterval
  (s/keys :req-un [::type :vrs.spec.sequence/end :vrs.spec.sequence/start]))

(s/def ::interval
  (s/or ::cytoband-interval ::CytobandInterval
        ::sequence-interval ::SequenceInterval))

(s/def ::SequenceLocation
  (s/keys :opt-un [::_id]
          :req-un [::interval ::sequence_id ::type]))

(s/def ::location ::SequenceLocation)

(s/def :vrs.spec.allele/location
  (s/or ::curie    ::CURIE
        ::location ::location))

(s/def ::DerivedSequenceExpression
  (s/keys :req-un [::location ::reverse_complement ::type]))

(s/def ::seq_expr
  (s/or ::derived-sequence-expression ::DerivedSequenceExpression
        ::literal-sequence-expression ::LiteralSequenceExpression))

(s/def ::RepeatedSequenceExpression
  (s/keys :req-un [::count ::seq_expr ::type]))

(s/def ::component
  (s/or ::derived-sequence-expression  ::DerivedSequenceExpression
        ::literal-sequence-expression  ::LiteralSequenceExpression
        ::repeated-sequence-expression ::RepeatedSequenceExpression))

(s/def ::components (s/coll-of ::component))

(s/def ::ComposedSequenceExpression
  (s/keys :req-un [::components ::type]))

(s/def ::SequenceExpression
  (s/or ::composed-sequence-expression ::ComposedSequenceExpression
        ::derived-sequence-expression  ::DerivedSequenceExpression
        ::literal-sequence-expression  ::LiteralSequenceExpression))

(s/def ::state ::SequenceExpression)

(s/def ::Allele
  (s/keys :opt-un [::_id]
          :req-un [::state ::type :vrs.spec.allele/location]))

(s/def ::haplotype-member
  (s/or ::allele ::Allele
        ::curie  ::CURIE))

(s/def :vrs.spec.haplotype/members
  (s/and seq
         (s/coll-of ::haplotype-member)))

(s/def ::Haplotype
  (s/keys :opt-un [::_id]
          :req-un [::type :vrs.spec.haplotype/members]))

(s/def ::molecular-variation
  (s/or ::allele    ::Allele
        ::haplotype ::Haplotype))

(s/def ::Gene
  (s/keys :req-un [::gene_id ::type]))

(s/def ::feature ::Gene)

(s/def ::subject
  (s/or ::curie               ::CURIE
        ::feature             ::feature
        ::molecular-variation ::molecular-variation
        ::sequence-expression ::SequenceExpression))

(s/def ::RelativeCopyNumber
  (s/keys :req-un [::relative_copy_class ::subject ::type]))

(s/def ::CopyNumber
  (s/keys :opt-un [::_id]
          :req-un [::copies ::subject ::type]))

(s/def ::ChromosomeLocation
  (s/keys :opt-un [::_id]
          :req-un [::chr ::interval ::species_id ::type]))

(s/def ::Text
  (s/keys :opt-un [::_id]
          :req-un [::definition ::type]))

(s/def ::systemic-variation ::CopyNumber)

(s/def ::utility-variation
  (s/or ::text          ::Text
        ::variation-set ::VariationSet))

(s/def ::variation
  (s/or ::molecular-variation ::molecular-variation
        ::systemic-variation  ::systemic-variation
        ::utility-variation   ::utility-variation))

(s/def ::variation-member
  (s/or ::curie     ::CURIE
        ::variation ::variation))

(s/def :vrs.spec.variation/members
  (s/coll-of ::variation-member))

(s/def ::VariationSet
  (s/keys :opt-un [::_id]
          :req-un [::type :vrs.spec.variation/members]))

(s/def ::AbsoluteCopyNumber
  (s/keys :opt-un [::_id]
          :req-un [::copies ::subject ::type]))

(defn valid?
  "True when the VRS object O is valid according to spec."
  [o]
  (s/valid? (keyword the-namespace-name (:type o)) o))
