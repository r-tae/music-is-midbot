(ns mim.spotify
  (:require [clj-http.client :as http]
            [clj-http.util :refer [url-encode]]
            [clj-spotify.core :as sptfy]
            [com.brunobonacci.mulog :as u]
            [dotenv :refer [env]]
            [mim.state :refer [get-state update-state]]
            [ring.adapter.jetty :as ringj]
            [ring.util.response :as ringr]
            [ring.middleware.params :as ringp]
            [ring.middleware.keyword-params :as ringkp]
            [slingshot.slingshot :refer [try+]]))

(def ^:private track-re #"https://open.spotify.com/(track)/(\w*)")
(def ^:private playlist-re #"https://open.spotify.com/(playlist)/(\w*)")
(def ^:private album-re #"https://open.spotify.com/(album)/(\w*)")

(declare refresh-tokens)
;; (defn refresh-token [] (auth/refresh-token :spotify))
(defn log-out [] (update-state :auth nil))

; TODO: somehow reply on persistant error
(defn- retry-on-error
  "Retry after refreshing the spotify token if `call` evaluates to `{:error _}`
       (retry-on-error '(sptfy/get-current-users-profile {} (lm/oauth-token :spotify))"
  [call]
  (let [result (call)]
    (if (:error result)
      (do (u/log ::retrying :call call) (refresh-tokens (get-state :auth :access_token))
          (call))
      result)))

(defn extract-tracks
  "Returns the track IDs of any spotify URLs in `msg`
   ```clojure
      (extract-tracks \"https://open.spotify.com/track/7oJgLeTIETiDEqRFaWbS3k?si=b606403c7cb34870\")
      => [\"spotify:track:7oJgLeTIETiDEqRFaWbS3k\"]
   ```"
  [message]
  (map (fn [match]
         (clojure.string/join ":" (cons "spotify" (rest match))))
       (re-seq #"https://open.spotify.com/(track)/([-a-zA-Z0-9]*)(?:\?.*)?" message)))
(defn extract-playlists
  "Returns the playlist IDs of any spotify URLs in `msg`
      (extract-playlists \"https://open.spotify.com/playlist/37i9dQZF1DZ06evO2HVeMk?si=5f14646388304e1f\")
      => [\"37i9dQZF1DZ06evO2HVeMk\"]
   Note this lacks the 'spotify:' prefix because the Spotify API don't seem to expect it anywhere."
  [message]
  (re-find playlist-re message))
(defn extract-albums
  "Returns the album IDs of any spotify URLs in `msg`
      (extract-albums \"https://open.spotify.com/album/2exrjacqbNx8HAQLfn96qJ?si=XIuiXtuQQcSk3O-18F3mPw\")
      => [\"spotify:album:2exrjacqbNx8HAQLfn96qJ\"]"
  [message]
  (re-find album-re message))

(declare get-fresh-token)

(defn filter-existing-tracks [tracks]
  (let [playlist (sptfy/get-a-playlist {:playlist_id (get-state :playlist :id)} (get-fresh-token))
        existing-tracks (map (comp :uri :track) (:items (:tracks playlist)))]
    (filter #(not (some (fn [t] (= t %))
                        existing-tracks))  tracks)))

(defn get-user []
  (sptfy/get-current-users-profile {} (get-fresh-token)))

(defn set-playlist [msg]
  (let [playlist-id (second (extract-playlists msg))]
    (update-state [:playlist]
                  (sptfy/get-a-playlist {:playlist_id playlist-id} (get-fresh-token)))))

(defn new-playlist [name]
  (let [token (get-fresh-token)]
    (update-state [:playlist]
                  (sptfy/create-a-playlist {:user_id (:id (sptfy/get-current-users-profile {} token))
                                            :name name} token))))

(defn add-to-playlist [tracks]
  (sptfy/add-tracks-to-a-playlist
   {;; :user_id (get (sptfy/get-current-users-profile {} (lm/oauth-token :spotify)) :id)
    :playlist_id (get-state :playlist :id) :uris tracks}
   (get-fresh-token)))

(def ^:private oauth2-params
  {:base-url "https://accounts.spotify.com"
   :auth-endpoint "/authorize"
   :token-endpoint "/api/token"
   :client-id (env :SPOTIFY_OAUTH2_CLIENT_ID)
   :redirect-uri (str (env :BASE_URL) "/oauth2")
   :scope "playlist-read-private playlist-modify-public playlist-modify-private ugc-image-upload"
   :show-dialog "true"
   :client-secret (env :SPOTIFY_OAUTH2_CLIENT_SECRET)})

(defn authorize-uri [client-params csrf-token]
  (str
   (:base-url client-params)
   (:auth-endpoint client-params)
   "?response_type=code"
   "&client_id="
   (url-encode (:client-id client-params))
   "&redirect_uri="
   (url-encode (:redirect-uri client-params))
   "&scope="
   (url-encode (:scope client-params))
   "&state="
   (url-encode csrf-token)))

(defn log
  [x]
  (u/log ::exception :exception x)
  x)
(defn get-authentication-response [csrf-token response-params]
  (if (= csrf-token (:state response-params))
    (try
      (-> (http/post (str (:base-url oauth2-params) (:token-endpoint oauth2-params))
                     {:form-params {:code         (:code response-params)
                                    :grant_type   "authorization_code"
                                    :client_id    (:client-id oauth2-params)
                                    :redirect_uri (:redirect-uri oauth2-params)}
                      :basic-auth [(:client-id oauth2-params) (:client-secret oauth2-params)]
                      :as          :json})
          :body)
      (catch Exception e (u/log ::exception :exception e)))
    nil))

(defn- refresh-tokens
  "Request a new token pair"
  [refresh-token]
  (try+
   (let [{{access-token :access_token refresh-token :refresh_token} :body}
         (http/post (str (:base-url oauth2-params) (:token-endpoint oauth2-params))
                    {:form-params {:grant_type       "refresh_token"
                                   :refresh_token    refresh-token}
                     :basic-auth [(:client-id oauth2-params) (:client-secret oauth2-params)]
                     :as          :json})]
     (update-state [:auth :tokens] [access-token refresh-token])
     access-token)
   (catch [:status 401] _ nil)))

(defn get-fresh-token
  "Returns current token pair if they have not expired, or a refreshed token pair otherwise"
  []
  (let [[access-token refresh-token] (get-state :auth :tokens)
        test-response (sptfy/get-current-users-profile {} access-token)]
    (if (contains? test-response :error)
      (refresh-tokens refresh-token)
      access-token)))

(defn auth-handler [request]
  (u/log ::oauth-received :request request)
  (condp = (:uri request)
    "/oauth2" (let [{access-token :access_token refresh-token :refresh_token} (get-authentication-response (get-state :auth :csrf-token) (:params request))]
                (when (and access-token refresh-token)
                  (update-state [:auth :tokens] [access-token refresh-token]))
                {:status 200 :body "<script>window.close()</script>"})
    "/interaction" (ringr/redirect (authorize-uri oauth2-params (get-state :auth :csrf-token)))
    {:status 200
     :body (:uri request)}))

(defn start-auth-server
  "Starts our auth web server."
  []
  (future (-> auth-handler
              ringkp/wrap-keyword-params
              ringp/wrap-params
              (ringj/run-jetty {:port 3000}))))

;; hit api
;; if it errors with 401
;;     - refresh token
;;     - try again
;;     - if it fails again, DM me
;; if it errors with 403, don't bother trying to recover, DM me
;; if it errors with 439, wait 2 seconds, if it fails after that then DM me


;; https://open.spotify.com/track/61NudzDPuUUH9NuhKMDgRD?si=ec51a9fa907b4ed0
;; https://open.spotify.com/track/2aQ8NyRJcsQFgb8jwUil90?si=c0671cf6eeb04970")

