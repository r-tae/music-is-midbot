(ns mim.spotify-test
  (:require [clojure.test :refer :all]
            [mim.spotify :refer :all]))

(deftest url-extractors
  (is (extract-tracks
       (str "Lorem https://open.spotify.com/track/61NudzDPuUUH9NuhKMDgRD?"
            "si=ec51a9fa907b4ed0 ipsum sit dolor https://open.spotify.com"
            "/track/2aQ8NyRJcsQFgb8jwUil90?si=c0671cf6eeb04970\n"
            "ipsum"))
      ["spotify:track:61NudzDPuUUH9NuhKMDgRD"
       "spotify:track:2aQ8NyRJcsQFgb8jwUil90"])
  (is
   (extract-playlists
    "lorem https://open.spotify.com/playlist/37i9dQZF1DZ06evO2HVeMk?si=5f14646388304e1f ipsum sit dolor")
   ["37i9dQZF1DZ06evO2HVeMk"])
  (is
   (extract-albums "https://open.spotify.com/album/2exrjacqbNx8HAQLfn96qJ?si=XIuiXtuQQcSk3O-18F3mPw")
   ["spotify:album:2exrjacqbNx8HAQLfn96qJ"]))
