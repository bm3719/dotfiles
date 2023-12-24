{:user {:plugins [[lein-ancient "1.0.0-RC4-SNAPSHOT"]
                  [lein-try "0.4.3"]
                  [lein-bikeshed "0.5.2"]
                  [cider/cider-nrepl "0.44.0"]
                  [lein-pprint "1.3.2"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.13"]]
        :injections []
        :eastwood {:add-linters [:unused-locals :unused-namespaces]}}}
