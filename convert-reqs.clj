(ns convert-reqs.core 
  (require [clojure.edn :as edn])
  (require [clojure.pprint :as p])
  (require [clojure.string :as str])
  )
(defn tag [tag-name contents] 
  (str "<" tag-name ">\n" contents "\n</" tag-name ">\n")
  )

(def reqs-doc (read-string (slurp "requirements.edn")))
(def language-name (reqs-doc :lang-name))
(def header
  (tag "header" (str (tag "style" (slurp "style.css"))
                     (tag "title" (:title reqs-doc)))))
(def table-headers 
  (str "<table>
       <tr> 
         <th> id </td>
         <th> functional-requirement </td>
         <th> demonstration scenarios </td>
         <th> success measure </td><th>
       </tr>"))
(def table-footer "</table>")

(defn req-to-str [id content]
  (str
    "<tr>"
    "<td>" (name id) "</td>"
    "<td>" language-name " " (-> content :type name) " " (content :functional-requirement)"</td>"
    "<td> <ol><li>" (str/join "</li><br /><li>" (map #(str/replace %1 "\n" "<br />\n") (content :demonstration-scenarios))) "</li></ol></td>"
    "<td><ol><li>  " (str/join "</li><br /><li>" (map #(str/replace %1 "\n" "<br />\n") (content :success-measure))) "</li></ol></td>"
    "</td>"
    "</tr>"
    )
  
  )

(defn def-to-string [[name disc]]
  (str (tag "h4" name) "\n" (tag "div" disc)))

;;;; print them out

(println header)

(println (tag "h1" (:title reqs-doc)))
(println (tag "p" (:author reqs-doc)))
(println (tag "p" (:date reqs-doc)))
(println (tag "h2" "Definitions"))

(doseq [defi (:definitions reqs-doc)] 
  (println (def-to-string defi)))

(println (tag "h2" "Requirements"))

(println table-headers)

(doseq [[key value] (:reqs reqs-doc)] 
  (println (req-to-str key value)))

(println table-footer)
