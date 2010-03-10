(ns twtr2mvc.http
  (use [clojure.contrib.duck-streams :only (with-out-writer slurp*)]
       [clojure.contrib.def :only (defnk)]
       [clojure.contrib.str-utils :only (str-join)])
  (import [java.io InputStreamReader OutputStreamWriter]
	  [java.net URL URLEncoder CookieHandler CookieManager CookiePolicy]))

(defnk query-string [kv-map encoding :encoder #(URLEncoder/encode %1 %2)]
  (str-join \&
    (for [[key value] kv-map]
      (let [key (if (keyword? key) (name key) (str key))]
	(str key \= (encoder (str value) encoding))))))

(defn enable-cookies []
  (let [manager (CookieManager.)]
    (.setCookiePolicy manager CookiePolicy/ACCEPT_ALL)
    (CookieHandler/setDefault manager)))

(def ensure-enabling-cookies
  (let [already-enabled? (atom false)]
    (fn []
      (when-not @already-enabled?
	(swap! already-enabled? (constantly true))
	(enable-cookies)))))

(defnk http-request [uri :method "GET" :headers {}
		     :body nil :encoding "UTF-8"
		     :reader (fn [in headers] (slurp* in))
		     :options {}]
  (ensure-enabling-cookies)
  (letfn [(request [uri opts]
	    (let [conn (.openConnection (URL. uri))]
	      (.setRequestMethod conn method)
	      (doseq [[header value] headers]
		(.setRequestProperty conn (str header) (str value)))
	      (when (or (= method "POST") (= method "PUT"))
		(let [body (or body
			       (and (string? opts) opts)
			       (query-string opts encoding))]
		  (.setDoOutput conn true)
		  (with-out-writer
		      (OutputStreamWriter. (.getOutputStream conn) encoding)
		    (print body))))
	      (with-open [in (InputStreamReader.
			       (.getInputStream conn)
			       encoding)]
		[(.getResponseCode conn)
		 (.getHeaderFields conn)
		 (reader in (.getHeaderFields conn))])))]
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
