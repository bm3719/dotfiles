{:user {:plugins [[lein-ancient "1.0.0-RC3"]
                  [lein-try "0.4.3"]
                  [lein-bikeshed "0.5.2"]
                  [cider/cider-nrepl "0.28.1"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.13"]]
        :injections []
        :eastwood {:add-linters [:unused-locals :unused-namespaces]}}}
