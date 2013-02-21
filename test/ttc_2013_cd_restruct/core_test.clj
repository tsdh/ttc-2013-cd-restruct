(ns ttc-2013-cd-restruct.core-test
  (:require [clojure.string :as str])
  (:use clojure.test
        ttc-2013-cd-restruct.core
        funnyqt.emf
        funnyqt.query))

;;* Utils

(defn everythings-contained? [m]
  (let [model-obj (the (eallobjects m 'model))]
    (forall? #(= model-obj (econtainer %))
             (eallobjects m '!model))))

(defn make-test-name [file multi]
  (symbol (str "test-" (-> (.getName (java.io.File. file))
                           (str/replace ".xmi" "")
                           (str/replace "_" "-"))
               (if multi "-mi" "-si"))))

(defn make-result-file-name [file multi]
  (str "results/"
       (-> (.getName (java.io.File. file))
           (str/replace ".xmi" "")
           (str "result" (if multi "-mi" "-si") ".xmi"))))

(defn make-result-viz-file-name [file multi]
  (str "results/"
       (-> (.getName (java.io.File. file))
           (str/replace ".xmi" "")
           (str "result" (if multi "-mi" "-si") ".pdf"))))

(defmacro make-test-case [file multi entities-num props-num gens-num & selectors]
  (let [test-name (make-test-name file multi)
        viz-file (str/replace file ".xmi" ".pdf")
        result-file (make-result-file-name file multi)
        result-viz-file (make-result-viz-file-name file multi)]
    `(deftest ~(with-meta test-name
                 (apply hash-map (mapcat (fn [sel] [sel true]) selectors)))
       (System/gc)
       (println "===========================================================")
       (if ~multi
         (println "Testing Multiple Inheritance")
         (println "Testing Single Inheritance"))
       (println "Model:" ~file)
       (print "Loading Time: ")
       (let [m# (time (load-model ~file))]
         (when (< (count (eallobjects m#)) 30)
           (print-model m# ~viz-file :exclude (eallobjects m# ~''model)))
         (print "Transformation Time: ")
         (time (pull-up-attributes m# ~multi))
         (is (= 1 (count (eallobjects m# ~''model))))
         (is (everythings-contained? m#))
         (is (= ~entities-num (count (eallobjects m# ~''Entity))))
         (is (= ~props-num    (count (eallobjects m# ~''Property))))
         (is (= ~gens-num     (count (eallobjects m# ~''Generalization))))
         ;; Don't save the very large models.
         (when (< ~entities-num 10000)
           (save-model m# ~result-file))
         (when (< (count (eallobjects m#)) 30)
           (print-model m# ~result-viz-file :exclude (eallobjects m# ~''model)))))))

;; Perform a warmup

(do
  (print "Performing a warmup...")
  (flush)
  (let [m1 (load-model "models/model/testcase2_5000.xmi")
        m2 (load-model "models/model/testcase3.xmi")]
    (pull-up-attributes m1 true)
    (pull-up-attributes m2 true))
  (println " Done! Now running the tests.")
  (System/gc))

;;* Core task: Single Inheritance

;; Single inheritance models, use only single inheritance rules

(make-test-case "models/model/testcase1.xmi"        false      6    3       5)
(make-test-case "models/model/testcase2.xmi"        false      9    2       8)
(make-test-case "models/model/testcase2_1000.xmi"   false   4002    2    4001)
(make-test-case "models/model/testcase2_5000.xmi"   false  20002    2   20001)
(make-test-case "models/model/testcase2_10000.xmi"  false  40002    2   40001)
(make-test-case "models/model/testcase2_50000.xmi"  false 200002    2  200001 :stress)
(make-test-case "models/model/testcase2_100000.xmi" false 400002    2  400001 :stress)
(make-test-case "models/model/testcase2_200000.xmi" false 800002    2  800001 :stress :maximum)
(make-test-case "models/model/testcase3.xmi"        false    501   10     500)

;; Multi-inheritance models, but just normal pulling up

(make-test-case "models/extension/mitest1.xmi"     false     4    1      4)
(make-test-case "models/extension/mitest2.xmi"     false     8    2      8)
(make-test-case "models/extension/mitest3.xmi"     false     6    3      4)

;;* Extension task: Exploit multiple inheritance

;; Single inheritance models, but also use multi inheritance rules.  With the
;; testcase1 model, the doubly-declared attribute a:T1 can be refactored into a
;; new class that both A and B inherit from (in addition to their normal
;; superclasses).
(make-test-case "models/model/testcase1.xmi"        true      7    2       7)
(make-test-case "models/model/testcase2.xmi"        true      9    2       8)
(make-test-case "models/model/testcase2_1000.xmi"   true   4002    2    4001)
(make-test-case "models/model/testcase2_5000.xmi"   true  20002    2   20001)
(make-test-case "models/model/testcase2_10000.xmi"  true  40002    2   40001)
(make-test-case "models/model/testcase2_50000.xmi"  true 200002    2  200001 :stress)
(make-test-case "models/model/testcase2_100000.xmi" true 400002    2  400001 :stress)
(make-test-case "models/model/testcase2_200000.xmi" true 800002    2  800001 :stress :maximum)
(make-test-case "models/model/testcase3.xmi"        true    501   10     500)

;; Multi-inheritance models with extension rule

(make-test-case "models/extension/mitest1.xmi"     true     4    1      4)
(make-test-case "models/extension/mitest2.xmi"     true     8    2      8)
(make-test-case "models/extension/mitest3.xmi"     true     7    2      6)

;;* Some additional test cases

(defn find-or-make-type [mo tn]
  (if-let [ts (seq (filter #(= tn (eget % :name))
                           (eget mo :types)))]
    (first ts)
    (let [t (ecreate! 'Type)]
      (eadd! mo :types t)
      (eset! t :name tn))))

(defn mk-entity [mo n & atts]
  (let [e (ecreate! 'Entity)]
    (eadd! mo :entitys e)
    (eset! e :name n)
    (doseq [[an at] atts
            :let [p (ecreate! 'Property)]]
      (eadd! mo :propertys p)
      (eset! p :name an)
      (eadd! e :ownedAttribute p)
      (eset! p :type (find-or-make-type mo at)))
    e))

(defn find-entity [mo n]
  (the #(= n (eget % :name))
       (eget mo :entitys)))

(defn mk-model [n ents gens]
  (let [model (new-model)
        mo (ecreate! model 'model)]
    (doseq [[n & ats] ents]
      (apply mk-entity mo n ats))
    (doseq [[sub super] gens]
      (make-generalization! mo (find-entity mo sub) (find-entity mo super)))
    (save-model model (str "models/extension/" n ".xmi"))
    model))

(mk-model "extra-test1"
          [["A" ["x" "a"]]
           ["B" ["y" "b"]]
           ["C" ["x" "a"]]
           ["D" ["y" "b"]]
           ["E" ["x" "a"] ["y" "b"]]
           ["F" ["x" "a"] ["y" "b"]]]
          [])

(make-test-case "models/extension/extra-test1.xmi" false 9 3 7)
(make-test-case "models/extension/extra-test1.xmi" true  9 2 8)

(mk-model "extra-test2"
          [["A"]
           ["B"]
           ["C" ["x" "a"] ["z" "c"]]
           ["D" ["x" "a"] ["y" "b"]]
           ["E" ["x" "a"] ["y" "b"]]
           ["F" ["x" "a"] ["z" "c"]]]
          [["C" "A"]
           ["D" "A"]
           ["E" "B"]
           ["F" "B"]])

(make-test-case "models/extension/extra-test2.xmi" false 7 5 6)
(make-test-case "models/extension/extra-test2.xmi" true  9 3 10)

(mk-model "extra-test3"
          [["S"]
           ["A" ["a" "I"] ["b" "I"]]
           ["B" ["a" "I"] ["c" "I"]]
           ["C" ["c" "I"] ["d" "I"]]
           ["D" ["c" "I"] ["b" "I"]]]
          [["A" "S"]
           ["B" "S"]
           ["C" "S"]
           ["D" "S"]])

(make-test-case "models/extension/extra-test3.xmi" false 6 6 5)
(make-test-case "models/extension/extra-test3.xmi" true  8 4 9)

(mk-model "extra-test4"
          [["S"]
           ["A" ["a" "I"] ["b" "I"] ["c" "I"]]
           ["B" ["a" "I"] ["b" "I"] ["d" "I"]]
           ["C" ["d" "I"]]]
          [["A" "S"]
           ["B" "S"]
           ["C" "S"]])

(make-test-case "models/extension/extra-test4.xmi" false 5 5 4)
(make-test-case "models/extension/extra-test4.xmi" true  6 4 6)

(defn stats [m]
  {:entities (count (eallobjects m 'Entity))
   :properties (count (eallobjects m 'Property))
   :generalization (count (eallobjects m 'Generalization))
   :types (count (eallobjects m 'Type))})
