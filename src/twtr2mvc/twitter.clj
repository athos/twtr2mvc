(ns twtr2mvc.twitter
  (use twtr2mvc.config)
  (import [twitter4j Twitter Paging]
	  [twitter4j.http AccessToken]))

(def *oauth-consumer-key* (config "twitter.consumer-key"))
(def *oauth-consumer-secret* (config "twitter.consumer-secret"))

(def *oauth-access-token* (config "twitter.access-token"))
(def *oauth-access-token-secret* (config "twitter.access-token-secret"))

(def twitter
  (doto (Twitter.)
    (.setOAuthConsumer *oauth-consumer-key* *oauth-consumer-secret*)
    (.setOAuthAccessToken (AccessToken. *oauth-access-token*
					*oauth-access-token-secret*))))

(defn update-status [msg]
  (.updateStatus twitter msg))

(defn in-reply-to [status]
  (let [reply-id (.getInReplyToStatusId status)]
    (if (= reply-id -1)
      nil
      (-> twitter (.showStatus reply-id) (.getText)))))

(defn mentions
  ([] (mentions nil))
  ([since-id]
   (let [ms (if since-id
	      (.getMentions twitter (Paging. (long since-id)))
	      (.getMentions twitter))]
     (sort #(>= (first %1) (first %2))
	   (for [m ms]
	     (let [u (.getUser m)]
	       [(.getId m)
		(.getText m)
		(.getScreenName u)
		(.getId u)
		(in-reply-to m)]))))))

(defn followers []
  (-> (.getFriendsIDs twitter) (.getIDs) seq))

(defn max-status-id [ms]
  (if (empty? ms) false (first (first ms))))

(defn feed-to-twitter [name id time msg]
  (let [content (format "%s: %s" name msg)
	id:time (format "(%s:%s)" id time)]
    (update-status
     (if (> (+ (count content) (count id:time)) 139)
       (format "%s\u2026 %s" (subs content 0 (- 138 (count id:time))) id:time)
       (format "%s %s" content id:time)))))
