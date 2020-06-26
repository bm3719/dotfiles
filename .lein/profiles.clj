{:user {:plugins [[lein-ancient "0.6.15"]
                  [lein-try "0.4.3"]
                  [lein-bikeshed "0.5.2"]
                  [cider/cider-nrepl "0.25.1"]]
        :dependencies [;; [spyscope "0.1.6"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :injections [;; (require 'spyscope.core)
                     ]
        :eastwood {:add-linters [:unused-locals :unused-namespaces]}}}
