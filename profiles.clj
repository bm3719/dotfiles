{:user {:plugins [[lein-kibit "0.1.2"]
                  [lein-ancient "0.6.8"]
                  [lein-try "0.4.3"]
                  [cider/cider-nrepl "0.13.0"]]
        :dependencies [[spyscope "0.1.5"]
                       [org.clojure/tools.nrepl "0.2.12"]]
        :injections [(require 'spyscope.core)]}}
