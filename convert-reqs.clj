(ns convert-reqs.core 
  (require [clojure.edn :as edn])
  (require [clojure.pprint :as p])
  (require [clojure.string :as str])
  )

;; HTML stuff

(defn tag "Makes an html tag string"
  [tag-name & contents] 
  (str "<" (name tag-name) ">\n" 
       (apply str contents) 
       "\n</" (name tag-name) ">\n"))

(defn args "takes a map and makes it into an html argument string" [arg-list]
  (reduce-kv "" (fn [init key val] 
                  (str init (name key) "=\"" val"\""))))

(defn tag-arg "this is a tag with arguments, the second argument is a sequence of argumetns"
  [tag-name arg-map & contents]
  (str "<" (name tag-name) " " (args arg-map)   ">\n"
       (apply str contents) 
       "\n</" (name tag-name) ">\n"))


;; vars

(def reqs-doc (read-string (slurp "requirements.edn")))
(def language-name (reqs-doc :lang-name))
(def header
  (tag :header 
       (str (tag :style (slurp "style.css"))
            (tag :title (:title reqs-doc)))))

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
  (tag :tr 
  "<td>" id "</td>"
  "<td>" language-name " " (-> content :type name) " " (content :functional-requirement)"</td>"
  "<td> <ol><li>" (str/join "</li><br /><li>" (map #(str/replace %1 "\n" "<br />\n") (content :demonstration-scenarios))) "</li></ol></td>"
  "<td>
  <ol><li>  " (str/join "</li><br /><li>" (map #(str/replace %1 "\n" "<br />\n") (content :success-measure))) "</li></ol></td>"
  "</td>"
       ))

(defn def-to-string [[name disc]]
  (str (tag :h4 name) "\n" (tag :div disc)))

;;;; print them out

(println header)

(println (tag :h1 (:title reqs-doc)))
(println (tag :p (:author reqs-doc)))
(println (tag :p (:date reqs-doc)))
(println (tag :h2 "Definitions"))

(doseq [defi (:definitions reqs-doc)] 
  (println (def-to-string defi)))

(println (tag :h2 "Requirements"))

(println table-headers)

(println
  (reduce-kv #(str %1 (req-to-str %2 %3 )) ""
             (:reqs reqs-doc) ))

(println table-footer)
