(defn filter-by-properties [pnts entities]
  (set (filter (fn [e] (forall? #(member? % (prop-type-set e)) pnts))
               entities)))
