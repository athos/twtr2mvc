(ns twtr2mvc.html
  (use [clojure.contrib.duck-streams :only (reader)]
       [saxon :only (compile-file compile-xpath)])
  (import [java.io Reader InputStream PipedWriter PipedReader]
	  [org.xml.sax InputSource]
	  [org.ccil.cowan.tagsoup HTMLSchema Parser XMLWriter]))

(defn parse-html [r]
  (let [parser (Parser.)
	input (InputSource.)
	output (PipedWriter.)
	writer (XMLWriter. output)
	result (PipedReader. output)]
    (.setCharacterStream input (reader r))
    (doto parser
      (.setProperty Parser/schemaProperty (HTMLSchema.))
      (.setFeature Parser/CDATAElementsFeature false)
      (.setFeature Parser/namespacesFeature false)
      (.setContentHandler writer))
    (let [ret (future (compile-file result))]
      (.parse parser input)
      (.close output)
      @ret)))

(defn extract-html [html xpath]
  ((compile-xpath xpath) html))
