(ns twtr2mvc
  (:gen-class)
  (use [twtr2mvc.config :only (config set-config-file!)]
       [clojure.contrib.str-utils :only (re-sub)])
  (require [twtr2mvc.mixi :as mixi]
	   [twtr2mvc.twitter :as twitter]
	   [clojure.contrib.logging :as log]))

(defn forward-from-twitter [since-id]
  (let [ms (twitter/mentions since-id)]
    (if (empty? ms)
      since-id
      (let [follower? (apply hash-set (twitter/followers))
	    rx (re-pattern (str "^@" (config "twitter.username") "\\s*"))]
	(doseq [[id text user user-id] (reverse ms) :when (follower? user-id)]
	  (log/info  (format "forward from Twitter: %s" text))
	  (mixi/post-echo (re-sub rx "" text)))
	(twitter/max-status-id ms)))))

(defn forward-from-mixi [last-time]
  (let [echoes (mixi/recent-echoes last-time)]
    (if (empty? echoes)
      last-time
      (do
	(doseq [[time message user id] (reverse echoes)
		:when (not (= id (config "mixi.id")))]
	  (log/info (format "forward from Mixi: %s" message))
	  (twitter/feed-to-twitter user id time message))
	(mixi/last-time echoes)))))

(defn kick-watcher! [name proc seed]
  (let [thread (Thread. (fn []
			  (try
			   (loop [since (proc (seed))]
			     (Thread/sleep 60000)
			     (log/info (format "[%s] watcher polling" name))
			     (recur (proc since)))
			   (catch Exception e
			     (log/error (format "[%s] watcher error: %s"
						name
						(.getMessage e)))))
			  (recur)))]
    (.start thread)))

(defn -main [& args]
  (when (> (count args) 0)
    (set-config-file! (first args)))
  (log/info "starting ...")
  (kick-watcher! "twitter" forward-from-twitter
		 #(twitter/max-status-id (twitter/mentions false)))
  (kick-watcher! "mixi" forward-from-mixi
		 #(mixi/last-time (mixi/recent-echoes false)))
  (while true (.suspend (Thread/currentThread))))
