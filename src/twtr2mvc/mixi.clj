(ns twtr2mvc.mixi
  (use twtr2mvc.http twtr2mvc.html twtr2mvc.config))

(def *user-email* (config "mixi.email"))
(def *user-password* (config "mixi.password"))

(defn mixi-request [path & rest]
  (apply http-request (str "http://mixi.jp" path ".pl")
	 :encoding "EUC_JP"
	 rest))

(defn scrape-echoes [html]
  (let [echoes (extract-html html "//table/tr/td[3]")
	result (for [echo echoes]
		 (let [[id time name message] (extract-html echo "div/text()")]
		   [(Long/parseLong (str time))
		    (str message)
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

(defn post-echo [msg]
  (let [post_key (post-key)
	options {"body" msg,
		 "post_key" post_key,
		 "redirect" "recent_echo"}]
    (mixi-request "/add_echo" :method "POST" :options options)))

(defn last-time [echoes]
  (if (empty? echoes) false (first (first echoes))))
