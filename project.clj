(defproject leap-overtone-sandbox "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [ohpauleez/clojure_leap "0.1.1-SNAPSHOT"]
                 [overtone "0.8.1" :exclusions [org.clojure/clojure]]
                 [leipzig "0.6.0"]]
  :repl-options {:init-ns leap-overtone-sandbox.core})
