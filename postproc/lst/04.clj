(defn pull-up-helper [mo super classes]
  (when (seq classes)
    (when-let [[pnts entities] (first (common-props classes))]
      (if (and super (= classes entities))
        (pull-up mo pnts entities super)  ;; rule 1
        (when (> (count entities) 1)
          (let [nc (make-entity! mo)]     ;; rule 2 if super is given, else rule 3
            (pull-up mo pnts entities nc)
            (doseq [s entities]
              (doseq [oldgen (eget s :generalization)
                      :when (= super (adj oldgen :general))]
                (edelete! oldgen))
              (make-generalization! mo s nc))
            (when super (make-generalization! mo nc super))
            true))))))
