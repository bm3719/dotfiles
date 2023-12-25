{:user {:plugins [;; [cider/cider-nrepl "0.22.0-beta6"]
                  [cider/cider-nrepl "0.28.6"] ; latest
                  [lein-ancient "1.0.0-RC4-SNAPSHOT"]
                  ;; [lein-try "0.4.3"]
                  [lein-bikeshed "0.5.2"]
                  [lein-pprint "1.3.2"]
                  [chestnut/lein-template "0.18.0"]
                  [s3-wagon-private "1.3.4"]
                  [lein-exec "0.3.7"]
                  [clj-http "3.10.0"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       [nrepl "1.0.0"]]
        :injections []
        :eastwood {:add-linters [:unused-locals :unused-namespaces]}}}
