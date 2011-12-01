(defproject tappan/util "0.0.2-SNAPSHOT"
  :description "Utility functions for Tappan."
  :warn-on-reflection true
  :dependencies [[org.clojure/clojure "1.2.1"]]
  :repl-options [:init nil :caught clj-stacktrace.repl/pst+]
  :clj-stacktrace {:test-color true})