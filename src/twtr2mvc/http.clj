(ns twtr2mvc.http
  (use [clojure.contrib.duck-streams :only (with-out-writer slurp*)]
       [clojure.contrib.def :only (defnk)]
       [clojure.contrib.str-utils :only (str-join re-split)])
  (import [java.util Date]
	  [java.io InputStreamReader OutputStreamWriter]
	  [java.net URL URLEncoder]))

(defstruct cookie :key :val :path :due)

(def *cookie-table* (ref {}))

(defn parse-cookie-header [header]
  (let [[kv path due] (re-split #"; " header)
	[_ key val] (re-find #"^(.+?)=(.+)$" kv)
	[_ path] (re-find #"^path=(.+)$" path)
	[_ due] (re-find #"^expires=(.+)$" due)]
    (struct cookie key val path (Date. due))))

(defn serialize-cookies [cookies]
  (str-join "; "
    (for [cookie cookies]
      (str (:key cookie) "=" (:val cookie)))))

(defn put-cookie [host cookie]
  (dosync
   (let [cookies (@*cookie-table* host)]
     (if cookies
       (swap! cookies assoc (:key cookie) cookie)
       (alter *cookie-table*
	      assoc host (atom {(:key cookie) cookie})))))
  nil)

(defn get-cookies [host path]
  (let [cookies (@*cookie-table* host)]
    (when cookies
      (let [now (Date.)
	    ret (for [[key cookie] @cookies
		      :when (and (.before now (:due cookie))
				 (.startsWith path (:path cookie)))]
		  cookie)]
	(if (empty? ret) nil ret)))))

(defnk query-string [kv-map encoding :encoder #(URLEncoder/encode %1 %2)]
  (str-join \&
    (for [[key value] kv-map]
      (let [key (if (keyword? key) (name key) (str key))]
	(str key \= (encoder (str value) encoding))))))

(defnk http-request [uri :method "GET" :headers {}
		     :body nil :encoding "UTF-8"
		     :reader (fn [in headers] (slurp* in))
		     :options {}]
  (letfn [(request [uri opts]
	    (let [uri (URL. uri)
		  host (.getHost uri)
		  path (.getPath uri)
		  conn (.openConnection uri)]
	      (.setRequestMethod conn method)
	      (doseq [[header value] headers]
		(.setRequestProperty conn (str header) (str value)))
	      (when-let [cookies (get-cookies host path)]
		(.setRequestProperty conn "Cookie" (serialize-cookies cookies)))
	      (when (or (= method "POST") (= method "PUT"))
		(let [body (or body
			       (and (string? opts) opts)
			       (query-string opts encoding))]
		  (.setDoOutput conn true)
		  (with-out-writer
		      (OutputStreamWriter. (.getOutputStream conn) encoding)
		    (print body))))
	      (let [headers (.getHeaderFields conn)]
		(when-let [cookies (.get headers "Set-Cookie")]
		  (doseq [cookie cookies]
		    (put-cookie host (parse-cookie-header cookie))))
		(with-open [in (InputStreamReader.
				(.getInputStream conn)
				encoding)]
		  [(.getResponseCode conn) headers (reader in headers)]))))]
      (let [[status _ :as result] (request uri options)]
	(when-not (= status "200") 'error)
	result)))

;; (defn http-request [uri & args]
;;   (ensure-enabling-cookies)
;;   (let [args (apply hash-map args)
;; 	options (args :options)
;; 	encoding (or (args :encoding) "UTF-8")
;; 	rest-args (dissoc args :options :encoding)]
;;     (letfn [(request [uri & args]
;; 	      (apply agent/http-agent uri
;; 		     :headers {"Connection" "close"}
;; 		     :read-timeout 3000
;; 		     ;:handler #(convert-stream-encoding (agent/stream %) encoding)
;; 		     (concat args (apply concat rest-args))))]
;;       (let [method (or (args :method) "GET")
;; 	    a (if options
;; 		(let [qs (query-string options encoding)]
;; 		  (if (or (= method "POST")
;; 			  (= method "PUT"))
;; 		    (request uri :body qs)
;; 		    (request (str uri \? qs))))
;; 		(request uri))]
;; 	a))))
