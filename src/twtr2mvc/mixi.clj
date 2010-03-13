(ns twtr2mvc.mixi
  (use twtr2mvc.http twtr2mvc.html twtr2mvc.config
       [clojure.contrib.str-utils :only (re-sub)]))

(def *user-email* (config "mixi.email"))
(def *user-password* (config "mixi.password"))

(defn mixi-request [path & rest]
  (apply http-request (str "http://mixi.jp" path ".pl")
	 :encoding "EUC_JP"
	 rest))

(defn omit->> [parent]
  (re-sub #"^&gt;&gt;" "" (str parent)))

(defn scrape-echoes [html]
  (let [echoes (extract-html html "//table/tr/td[3]")
	result (for [echo echoes]
		 (let [[id time name message] (extract-html echo "div/text()")
		       parent (extract-html echo
				"a[contains(@href,'view_echo.pl')]/text()")]
		   [(Long/parseLong (str time))
		    (if parent
		      (format ">>%s %s" (omit->> parent) message)
		      (str message))
		    (str name)
		    (str id)]))]
    (when-not (empty? result)
      result)))
      
(defn scrape-post-key [html]
  (let [xpath "//form[@id='EchoPost']//input[2]/@value"
	post-key (extract-html html xpath)]
    (when-not (empty? post-key)
      (.getStringValue post-key))))

(defn login []
  (let [options {"email" *user-email*,
		 "password" *user-password*,
		 "sticky" "on",
		 "next_url" "/home.pl"}]
    (mixi-request "/login" :method "POST" :options options)))

(defn try-until-success [path scraper]
  (loop [retry 5]
    (if (= retry 0)
      (throw (Exception. "exceeded retry upper bound"))
      (let [[status headers body]
	    (mixi-request path :reader (fn [in _] (parse-html in)))]
	(if-let [result (scraper body)]
	  result
	  (do (login)
	      (recur (dec retry))))))))

(defn recent-echoes [last-time]
  (sort #(>= (first %1) (first %2))
	(filter (fn [[time]] (or (not last-time) (> time last-time)))
		(try-until-success "/recent_echo" scrape-echoes))))

(defn post-key []
  (try-until-success "/recent_echo" scrape-post-key))

(defn post-echo [msg & [parent-member-id parent-post-time :as parent]]
  (let [post_key (post-key)
	options (merge (and parent
			    {"parent_member_id" parent-member-id,
			     "parent_post_time" parent-post-time})
		       {"body" msg,
			"post_key" post_key,
			"redirect" "recent_echo"})]
    (mixi-request "/add_echo" :method "POST" :options options)))

(defn last-time [echoes]
  (if (empty? echoes) false (first (first echoes))))

(defn feed-to-mixi [msg in-reply-to]
  (if in-reply-to
    (let [[id time] (re-find #"\(([0-9]+):([0-9]+)\)$" in-reply-to)]
      (post-echo msg id time))
    (post-echo msg)))
