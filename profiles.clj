{:user {:plugins [[lein-kibit "0.1.6"]
                  [lein-ancient "0.6.15"]
                  [lein-try "0.4.3"]
                  [lein-bikeshed "0.5.1"]
                  [jonase/eastwood "0.2.9"]
                  [cider/cider-nrepl "0.17.0"]]
        :dependencies [; [spyscope "0.1.5"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :injections [;(require 'spyscope.core)
                     ]
        :eastwood {:add-linters [:unused-locals :unused-namespaces]}}}
