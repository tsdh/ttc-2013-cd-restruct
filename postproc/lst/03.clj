(defn common-props [classes]
  (let [pes (set (map (fn [pnt] [pnt (filter-by-properties [pnt] classes)])
                      (set (mapcat prop-type-set classes))))
        freq-map (apply hash-map
                        (mapcat (fn [[_ ents]] [ents (count (filter #(= ents (second %))
                                                                    pes))])
                                pes))
        collapse (fn collapse [aes]
                   (when-let [[pnt entities] (first aes)]
                     (let [[s r] (split-with (fn [[_ ents]] (= entities ents)) aes)]
                       (cons [(map first s) entities]
                             (lazy-seq (collapse r))))))]
    (collapse (into (sorted-set-by
                     (fn [[_ aes :as a] [_ bes :as b]]
                       (let [x (- (count bes) (count aes))]
                         (if (zero? x)
                           (let [x (- (freq-map bes) (freq-map aes))]
                             (if (zero? x) (compare a b) x))
                           x))))
                    pes))))
