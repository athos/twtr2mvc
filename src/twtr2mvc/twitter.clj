(ns twtr2mvc.twitter
  (use twtr2mvc.config)
  (import [twitter4j Twitter]
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

(defn mentions []
  (.getMentions twitter))
