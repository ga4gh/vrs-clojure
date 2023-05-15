(ns vrs.spec
  "Model VRS. https://vrs.ga4gh.org/en/stable/terms_and_model.html"
  (:require [clojure.spec.alpha :as s]
            [clojure.string     :as str]))

(def ^:private the-namespace-name
  "The name of this namespace as a string for `valid?` below."
  (-> *ns* ns-name name))

;; https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry#Amino_acid_and_nucleotide_base_codes

(def ^:private nucleics
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

(def ^:private aminos
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

(def digestible
  "Map a digestible type to an identifier prefix as a keyword."
  {"AbsoluteCopyNumber" :VAC
   "Abundance"          :VAB            ; not in validation suite
   "Allele"             :VA
   "ChromosomeLocation" :VCL
   "CopyNumber"         :VCN            ; not in validation suite
   "Genotype"           :VG             ; not in validation suite
   "Haplotype"          :VH
   "RelativeCopyNumber" :VRC
   "SequenceLocation"   :VSL
   "Text"               :VT
   "VariationSet"       :VS})

(def digestible?
  "Set of the types that can be digested."
  (-> digestible keys set))

(def indigestible?
  "Set of the types that cannot be digested."
  #{"CURIE"
    "CytobandInterval"
    "DefiniteRange"
    "DerivedSequenceExpression"
    "Gene"
    "IndefiniteRange"
    "LiteralSequenceExpression"
    "Number"
    "RepeatedSequenceExpression"
    "SequenceInterval"})

(def ^:private obsolete?
  "Obsolete types that still show up in validation suites."
  #{"SequenceState"
    "SimpleInterval"})

(def ^:private type?
  "The set of all type names -- even obsolete ones."
  (-> obsolete?
      (into indigestible?)
      (into digestible?)))

;; Recent JDKs put java.util.Base64$Encoder/toBase64URL off limits!
;;
(def ^:private digest-regex
  "32 character from the Base64 alphabet safe for URLs by RFC#4648§5."
  (let [url-safe-base64 "[a-z0-9A-Z_-]"
        size            32]
    (re-pattern (str "(" url-safe-base64 "{" size "})"))))

(def ^:private sequence-regex
  "Match the characters in the aminos and nucleics keys."
  (-> (concat (keys aminos) (keys nucleics)) set
      (->> (map name) (apply str))
      (str/escape {\- "\\-" \* "\\*"})
      (interpose ["^[" "]*$"])
      (->> (apply str))
      re-pattern))

;; Evidently "SQ" is also a valid type prefix for a VRS CURIE.¯\_(ツ)_/¯
;; https://github.com/ga4gh/vrs/blob/5d7f29cbdfac15619f9f39b38c370be416828fe6/schema/ga4gh.yaml#L20
;; https://github.com/ga4gh/vrs/blob/5d7f29cbdfac15619f9f39b38c370be416828fe6/validation/models.yaml#L95
;;
(def curie-regex
  "Match a VRS CURIE to 3 groups: 'ga4gh':'type'.'digest'."
  (-> digestible vals (conj :SQ)
      (->> (map name) sort (interpose "|") (apply str))
      (interpose ["^(ga4gh):(" (str ")\\." digest-regex "$")])
      (->> (apply str))
      re-pattern))

(defn ^:private digest?
  "Nil or the OBJECT string when it is the digest part of a VRS CURIE."
  [object]
  (try (re-matches digest-regex object)
       (catch Throwable _)))

(defn ^:private sequence?
  "Nil or the OBJECT string when it is a sequence."
  [object]
  (try (re-matches sequence-regex object)
       (catch Throwable _)))

(defn curie?
  "Nil or [TYPE DIGEST] from OBJECT when it is a VRS CURIE string.

   (spec/curie? \"ga4gh:VT.01234567890123456789012345678901\")

   [\"VT\" \"01234567890123456789012345678901\"]"
  [object]
  (try (let [[curie? _ga4gh type digest] (re-matches curie-regex object)]
         (when curie? [type digest]))
       (catch Throwable _)))

;; https://vrs.ga4gh.org/en/latest/terms_and_model.html#humancytoband
;;
(defn ^:private iscn?
  [object]
  (try (re-matches #"^cen|[pq](ter|([1-9][0-9]*(\.[1-9][0-9]*)?))$" object)
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

(s/def ::name ::string)

(s/def ::relative_copy_class ::string)

(s/def ::sequence_id ::string)

(s/def ::species_id ::string)

(s/def ::CURIE curie?)

(s/def ::sequence sequence?)

(s/def ::type type?)

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
  (s/keys :req-un [::type :vrs.spec.simple/end :vrs.spec.simple/start]))

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
  (s/keys :opt-un [::_id ::name]
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

(s/def ::AbsoluteCopyNumber
  (s/keys :opt-un [::_id]
          :req-un [::copies ::subject ::type]))

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

(defn valid?
  "True when the OBJECT is valid according to the VRS spec."
  [{:keys [type] :as object}]
  (s/valid? (keyword the-namespace-name type) object))
