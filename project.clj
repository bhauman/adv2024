(defproject adv2024 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [dev.weavejester/medley "1.7.0"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/data.priority-map "1.1.0"]
                 #_[com.phronemophobic/clj-graphviz "0.6.4"]
                 [incanter "1.9.3"]
                 #_[uncomplicate/neanderthal "0.46.0"]]
  :repl-options {:init-ns adv2024.core})
