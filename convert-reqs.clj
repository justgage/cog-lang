(ns convert-reqs.core 
  (require [clojure.edn :as edn])
  (require [clojure.pprint :as p])
  (require [clojure.string :as str])
  )
(defn tag [tag-name contents] 
  (str "<" tag-name ">\n" contents "\n</" tag-name ">\n")
  )

(def reqs (read-string (slurp "requirements.edn")))
(def language-name (reqs :lang-name))
(def header (tag "style"
                  (slurp "style.css")))


(defn print-headers "does what it says" []
  (println "<table>")
  (println "<tr> <th> id </td><th> functional-requirement </td><th> demonstration scenarios </td><th> success measure </td><th></tr>")
  )

(defn print-footer [] (println "</table>"))
(defn print-out-req "This does the basic work of printing out requirements" [id content]
  (println 
    "<tr>"
    "<td>" (name id) "</td>"
    "<td>" language-name (-> content :type name) (content :functional-requirement)"</td>"
    "<td> <ul><li>" (str/join "</li><br /><li>" (map #(str/replace %1 "\n" "<br />\n") (content :demonstration-scenarios))) "</li></ul></td>"
    "<td>" (str/join "" (map #(str/replace %1 "\n" "<br />\n") (content :success-measure)))
    "</td>"
    "</tr>"
    )
  
  )


(println header)
(print-headers)
(as-> reqs r
  (:reqs r) 
  (doseq [ [k v] r] 
    (print-out-req k v)
    )
  )

(print-footer)
