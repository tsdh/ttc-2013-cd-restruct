(ns ttc-2013-cd-restruct.core
  (:use funnyqt.protocols funnyqt.emf funnyqt.query
        funnyqt.query.emf funnyqt.in-place))

(load-metamodel "metamodel/newmm.ecore")

(defn add-prop! [mo e pn t]
  (let [p (ecreate! 'Property)]
    (eadd! mo :propertys p)
    (eset! p :name pn)
    (eset! p :type t)
    (eadd! e :ownedAttribute p)))

(defn delete-prop! [e pn]
  (let [p (first (filter #(= pn (eget % :name))
                         (eget e :ownedAttribute)))]
    (edelete! p)
    (eremove! e :ownedAttribute p)))

(defn pull-up [mo pnts froms to]
  (doseq [[pn t] pnts]
    (add-prop! mo to pn t)
    (doseq [s froms]
      (delete-prop! s pn)))
  true)

(defn make-generalization! [mo sub super]
  (let [gen (ecreate! 'Generalization)]
    (eadd! mo :generalizations gen)
    (eset! gen :general super)
    (eset! gen :specific sub)))

(defn make-entity! [mo]
  (let [e (ecreate! 'Entity)]
    (eadd! mo :entitys e)
    (eset! e :name (str (gensym "NewClass")))))

(defn prop-type-set [e]
  (set (map (fn [p] [(eget p :name) (eget p :type)])
            (eget e :ownedAttribute))))

(defn filter-by-properties [pnts entities]
  (set (filter (fn [e]
                 (forall? #(member? % (prop-type-set e)) pnts))
               entities)))

(defn common-props [classes]
  (let [pes (set (map (fn [pnt]
                        [pnt (filter-by-properties [pnt] classes)])
                      (set (mapcat prop-type-set classes))))
        freq-map (apply hash-map
                        (mapcat (fn [[_ ents]]
                                  [ents (count (filter #(= ents (second %))
                                                       pes))])
                                pes))
        collapse (fn collapse [aes]
                   (when-let [[pnt entities] (first aes)]
                     (let [[s r] (split-with (fn [[_ ents]]
                                               (= entities ents)) aes)]
                       (cons [(map first s) entities]
                             (lazy-seq (collapse r))))))]
    (collapse (into (sorted-set-by
                     (fn [[_ aes :as a] [_ bes :as b]]
                       (let [x (- (count bes) (count aes))]
                         (if (zero? x)
                           (let [x (- (freq-map bes) (freq-map aes))]
                             (if (zero? x)
                               (compare a b)
                               x))
                           x))))
                    pes))))

(defn pull-up-helper [mo super classes]
  (when (seq classes)
    (when-let [[pnts entities] (first (common-props classes))]
      (if (and super (= classes entities))
        (pull-up mo pnts entities super)  ;; rule 1
        (when (> (count entities) 1)
          (let [nc (make-entity! mo)]     ;; if super rule 2, else rule 3
            (pull-up mo pnts entities nc)
            (doseq [s entities]
              (doseq [oldgen (eget s :generalization)
                      :when (= super (adj oldgen :general))]
                (edelete! oldgen))
              (make-generalization! mo s nc))
            (when super (make-generalization! mo nc super))
            true))))))

(defn exploit-multiple-inheritance [mo]
  (doseq [[pnts entities] (common-props (eget mo :entitys))
          :while (> (count entities) 1)]
    (let [[nc reuse]
          (if-let [top (first (filter
                               #(and (empty? (eget % :generalization))
                                     (re-matches #"NewClass.*" (eget % :name)))
                               entities))]
            [top true]
            [(make-entity! mo) false])]
      (doseq [[pn t] pnts]
        (when-not reuse
          (add-prop! mo nc pn t))
        (doseq [e (remove #(= nc %) entities)]
          (delete-prop! e pn)
          (make-generalization! mo e nc))))))

(defn pull-up-1-2 [mo]
  (loop [classes (eget mo :entitys), applied false]
    (if (seq classes)
      (let [super (first classes)
            result (pull-up-helper
                    mo super (set (adjs super :specialization :specific)))]
        (recur (rest classes) (or result applied)))
      applied)))

(defn pull-up-3 [mo]
  (pull-up-helper mo nil (set (remove #(seq (eget % :generalization))
                                      (eget mo :entitys)))))

(defn pull-up-attributes [model multi-inheritance]
  (let [mo (the (eallobjects model 'model))]
    (iteratively #(let [r (pull-up-1-2 mo)]
                    (or (pull-up-3 mo) r)))
    (when multi-inheritance (exploit-multiple-inheritance mo))
    model))
