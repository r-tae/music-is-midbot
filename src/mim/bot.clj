(ns mim.bot
  (:require
   [mim.spotify :as spotify]
   [mim.state :refer [get-state update-state]]
   [mim.util :refer [generate-token]]
   [clojure.set :as set]
   [discljord.connections :as con]
   [discljord.messaging :as rest]
   [discljord.events :as events]
   [clojure.core.async :as a]
   [dotenv :refer [env]]
   [slash.command :as cmd]
   [slash.core :as slash]
   [slash.command.structure :as stc]
   [slash.gateway :refer [gateway-defaults]]
   [com.brunobonacci.mulog :as u]
   [slash.response :as rsp :refer [channel-message ephemeral]])
  (:gen-class))

(def discord-token
  (env :DISCORD_AUTH_TOKEN))
(def application-id "1141359842955624490")

(u/start-publisher! {:type :console})
(def rest-conn (rest/start-connection! discord-token))

(rest/bulk-overwrite-global-application-commands!
 rest-conn application-id
 [(stc/command
   "echo"
   "Echoes your input"
   :options
   [(stc/option "input"
                "Your input" :string :required true)])
  (stc/command
   "status"
   "Check playlist bot status"
   :options [])
  (stc/command
   "auth"
   "Fetch Spotify auth URL"
   :options [])
  (stc/command
   "playlist"
   "Create or use an existing Spotify playlist"
   :options
   [(stc/option "url"
                "Use an existing playlist" :string :required false)
    (stc/option "name"
                "Make a new playlist" :string :required false)])])

(defn respond-interaction [id token {:keys [type data]}]
  (rest/create-interaction-response! rest-conn id token type :data data))


(defn if-mod
  [member mod-branch not-mod-branch]
  (if  (seq (set/intersection (set (:roles member))
                              #{"1143560732089262090" ;;testing server
                                "1079637398281011200" ;; music-is-mid
                                }))
    (mod-branch) (not-mod-branch)))


;; (cmd/defhandler echo-handler
;;   ["echo"] ; Command path
;;   {:keys [id token] :as _interaction} ; Interaction binding
;;   [input] ; Command options
;;   (->> (channel-message {:content input})
;;        ephemeral
;;        (respond-interaction id token)))
(cmd/defhandler status-handler
  ["status"] ; Command path
  {:keys [id token] :as interaction} ; Interaction binding
  [] ; Command options
  (u/log ::handle-status-command)
  (u/log ::interaction :data interaction)
  (->> (channel-message {:content (str "Status:\nSpotify account: " (:display_name (spotify/get-user))
                                       "\nPlaylist: " (get-state :playlist :external_urls :spotify))})
       ephemeral
       (respond-interaction id token)))
(cmd/defhandler auth-handler
  ["auth"]
  {:keys [id token member] :as _interaction}
  []
  (if-mod member
          #(do (u/log ::handle-auth-command :mod true)
               (update-state [:auth :csrf-token] (generate-token))
               (->> (channel-message {:content
                                      (str "Follow link to authenticate your spotify account: " (env :BASE_URL) "/interaction")})
                    ephemeral
                    (respond-interaction id token)))
          #(do (u/log ::handle-auth-command :mod false)
               (->> (channel-message {:content
                                      (str "You must be a mod to run this command.")})
                    ephemeral
                    (respond-interaction id token)))))


(cmd/defhandler playlist-handler
  ["playlist"]
  {:keys [id token member] :as _interaction}
  [url name]
  (if-mod member
          #(do (u/log ::handle-playlist-command :mod true :url url :name name)
               (->>
                (cond
                  url (do (spotify/set-playlist url) (channel-message {:content (str "Using " (get-state :playlist :external_urls :spotify))}))
                  name (do (spotify/new-playlist name) (channel-message {:content (str "Created a new playlist: " (get-state :playlist :external_urls :spotify))}))
                  :else (channel-message {:content "Please give a URL or name for your playlist"}))
                ephemeral
                (respond-interaction id token)))
          #(do (u/log ::handle-playlist-command :mod false)
               (->> (channel-message {:content
                                      (str "You must be a mod to run this command.")})
                    ephemeral
                    (respond-interaction id token)))))

(cmd/defpaths command-paths
  status-handler
  auth-handler
  playlist-handler)

(defn handle-ready [_ _data] (u/log ::connected))

(defn handle-message-create
  [_ {{bot :bot} :author :keys [content] :as _data}]
  (when-not bot
    (let [tracks (spotify/filter-existing-tracks (spotify/extract-tracks content))]
      (when-not (empty? tracks)
        (u/log ::add-to-playlist :tracks tracks)
        (spotify/add-to-playlist tracks)))))

(def event-handlers
  {:ready [#'handle-ready]
   :interaction-create
   [#(slash/route-interaction (assoc gateway-defaults
                                     :application-command command-paths) %2)]
   :message-create [#'handle-message-create]})

(defn -main
  [& _args]
  (spotify/start-auth-server)
  (let [channel (a/chan 100)
        conn-ch (con/connect-bot! discord-token channel
                                  :intents #{:guilds :guild-messages})]
    (events/message-pump! channel
                          (partial events/dispatch-handlers event-handlers))
    (rest/stop-connection! rest-conn)
    (con/disconnect-bot! conn-ch)
    (a/close! channel)))
