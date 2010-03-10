(ns twtr2mvc
  (use twtr2mvc.config
       [clojure.contrib.str-utils :only (re-sub)])
  (require [twtr2mvc.mixi :as mixi]
	   [twtr2mvc.twitter :as twitter]))

(defn forward-from-twitter [since-id]
  (let [ms (twitter/mentions since-id)]
    (if (empty? ms)
      since-id
      (let [follower? (apply hash-set (twitter/followers))
	    rx (re-pattern (str "^@" (config "twitter.username") "\\s*"))]
	(doseq [[id text user user-id] (reverse ms) :when (follower? user-id)]
	  (println "from Twitter:" text)
	  (mixi/post-echo (re-sub rx "" text)))
	(twitter/max-status-id ms)))))

(defn forward-from-mixi [last-time]
  (let [echoes (mixi/recent-echoes last-time)]
    (if (empty? echoes)
      last-time
      (do
	(doseq [[_ message user id] (reverse echoes)
		:when (not (= id (config "mixi.id")))]
	  (println "from Mixi:" message)
	  (twitter/update-status (str user "@mixi " message)))
	(mixi/last-time echoes)))))

(defn kick-watcher! [proc seed]
  (let [thread (Thread. #(loop [since (proc (seed))]
			   (println since)
			   (Thread/sleep 60000)
			   (recur (proc since))))]
    (.start thread)))

(defn main- []
  (kick-watcher! forward-from-twitter
		 #(twitter/max-status-id (twitter/mentions false)))
  (kick-watcher! forward-from-mixi
		 #(mixi/last-time (mixi/recent-echoes false)))
  (while true (.suspend (Thread/currentThread))))
