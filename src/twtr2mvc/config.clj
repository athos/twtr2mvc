(ns twtr2mvc.config)

(def *configuration-filename* (atom "twtr2mvc.conf"))

(def *configurations* (atom {}))

(defmacro defconfig [name & key-values]
  (let [configs (map (fn [[key value]]
		       [(str name "." key) value])
		     key-values)]
    `(do (swap! *configurations*
		(fn [~'x] (into ~'x '~configs)))
	 nil)))

(def already-loaded? (ref false))

(defn config [key]
  (dosync
   (when-not @already-loaded?
     (load-file @*configuration-filename*)
     (ref-set already-loaded? true)))
  (@*configurations* key))

(defn reload []
  (dosync
   (ref-set already-loaded? false))
  (load-file @*configuration-filename*))
