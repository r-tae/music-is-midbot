(ns mim.state
  (:require
   [clojure.edn :as edn]
   [com.brunobonacci.mulog :as u]))

;; Try to load from file, otherwise start with empty state
(def ^:private
  state (atom
         (try
           (clojure.edn/read-string (slurp "state.edn"))
           (catch Exception _ {}))))

(defn update-state
  "Add or modify a state key-value pair. Saves to a file on each update.
(update-state [:user :user-id] \"userid \") "
  [keys value]
  (swap! state assoc-in keys value)
  (spit "state.edn" (pr-str @state)))

(defn get-state
  " Allows easy access to nested state.
  ```(get-state :user)
  (get-state :user :user-id)
  ```"
  [& args]
  ((apply comp (reverse args)) @state))
