(require '[discljord.connections :as c])
(require '[discljord.messaging :as m])
(require '[discljord.events :as e])
(require '[clojure.core.async :as a])
(require '[loudmoauth.core :as lm])
(require '[ring.adapter.jetty :as ringj])
(require '[ring.util.response :as ringr])
(require '[ring.middleware.params :as ringp])
(require '[ring.middleware.keyword-params :as ringkp])
(require '[clj-spotify.core :as sptfy])
(require '[clj-spotify.util :as util])
(require '[clojure.string :as str])
(require '[clojure.core.match :refer [match]])
(require '[dotenv :refer [env]])

(def state (atom nil))
(def auth-message
  (str "Follow link to authenticate your spotify account: " (env :BASE_URL) "/interact"))
(def token
  (env :DISCORD_AUTH_TOKEN))
(def spotify-client-id (env :SPOTIFY_OAUTH2_CLIENT_ID))
(def spotify-client-secret (env :SPOTIFY_OAUTH2_CLIENT_SECRET))
(def intents #{:guilds :guild-messages})

;; TODO: handle albums
(defn extract-songs [msg]
  (map (fn [match]
         (clojure.string/join ":" (cons "spotify" (rest match))))
       (re-seq #"https://open.spotify.com/(track)/([-a-zA-Z0-9]*)(?:\?.*)?" msg)))

(defn has-spotify-tracks? [msg] (re-seq #"https://open.spotify.com/(track)/([-a-zA-Z0-9]*)(?:\?.*)?" msg))

(defn create-playlist []
  (sptfy/create-a-playlist {:user_id (get (sptfy/get-current-users-profile {} (lm/oauth-token :spotify)) :id) :name
                            "Music is mid recs" :public true} (lm/oauth-token :spotify)))

;; TODO: don't hardcode the playlist ID
(defn add-to-playlist [uris]
  (match [(sptfy/add-tracks-to-a-playlist
           {:user_id (get (sptfy/get-current-users-profile {} (lm/oauth-token :spotify)) :id)
            :playlist_id (get @state :playlist_id) :uris uris}
           (lm/oauth-token :spotify))]
    [{:snapshot_id _}] "Added to playlist"
    :else "Error"))

(defn handler [request]
  (condp = (:uri request)
    "/oauth2" (lm/parse-params request)
    "/interact"  (ringr/redirect (lm/user-interaction))
    {:status 200
     :body (:uri request)}))

(defn run-auth-server
  "Starts our test web server."
  []
  (future (ringj/run-jetty (ringp/wrap-params (ringkp/wrap-keyword-params handler))  {:port 3000}))
  (lm/add-provider {:base-url "https://accounts.spotify.com"
                    :auth-endpoint "/authorize"
                    :token-endpoint "/api/token"
                    :client-id spotify-client-id
                    :redirect-uri (str (env :BASE_URL) "/oauth2")
                    :scope "playlist-read-private playlist-modify-public playlist-modify-private ugc-image-upload"
                    :custom-query-params {:show-dialog "true"}
                    :client-secret spotify-client-secret
                    :provider :spotify}))

(run-auth-server)

(defmulti handle-event
  (fn [event-type _event-data]
    event-type))

(defmethod handle-event :default
  [_event-type _event-data])
(defn handle-new-playlist [channel-id]
  (let [playlist (create-playlist)]
    (swap! state assoc :playlist_id (:id playlist))
    (m/create-message! (:messaging @state) channel-id :content
                       (str "<" (get-in playlist [:external_urls :spotify]) ">"))))

;; TODO: error handling
;; TODO: limit to one channel
(defn set-playlist? [a] (str/starts-with? a "!set-playlist"))
(defmethod handle-event :message-create
  [_event-type {{bot :bot} :author :keys [channel-id content id]}]
  (when-not bot (match [content]
                  ["!disconnect"] (c/disconnect-bot! (:connection @state))
                  ["!auth"] (m/create-message! (:messaging @state) channel-id :content auth-message)
                  ;; TODO: register commands properly
                  [_ :guard set-playlist?] (swap! state assoc :playlist_id
                                                  (nth (re-find #"https://open.spotify.com/playlist/(\w*)" content) 1))
                  ;; TODO: proper error handling and replies
                  ["!new-playlist"] (handle-new-playlist channel-id)
                  :else (if (has-spotify-tracks? content)
                          (m/create-message! (:messaging @state) channel-id :content (add-to-playlist (extract-songs content)))
                          ;; TODO: don't error if we can't do anything with the message
                          (m/create-reaction! (:messaging @state) channel-id id "🆘")))))

(a/go
  (let [event-ch (a/chan 100)
        connection-ch (c/connect-bot! token event-ch :intents intents)
        messaging-ch (m/start-connection! token)
        init-state {:connection connection-ch
                    :event event-ch
                    :messaging messaging-ch}]
    (reset! state init-state)
    (try (e/message-pump! event-ch handle-event)
         (finally
           (m/stop-connection! messaging-ch)
           (a/close!           event-ch)))))
