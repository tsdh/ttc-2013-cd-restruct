(defproject ttc-2013-cd-restruct "0.1.0"
  :description "Solution to the TTC Class Diagram Restructuring Case"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [funnyqt "0.4.23"]]
  :test-selectors {:default (fn [m] (not (or (:stress m) (:maximum m))))
                   :maximum :maximum
                   :stress  :stress
                   :all     (constantly true)}
  :jvm-opts ["-Xms800m" "-Xmx800m"])
