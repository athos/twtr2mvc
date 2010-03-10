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

(defn mentions
  ([] (mentions nil))
  ([since-id]
   (let [ms (if since-id
	      (.getMentions twitter (Paging. (long since-id)))
	      (.getMentions twitter))]
     (sort #(>= (first %1) (first %2))
	   (for [m ms]
	     (let [u (.getUser m)]
	       [(.getId m) (.getText m) (.getScreenName u) (.getId u)]))))))

(defn followers []
  (-> (.getFriendsIDs twitter) (.getIDs) seq))

(defn max-status-id [ms]
  (if (empty? ms) false (first (first ms))))
