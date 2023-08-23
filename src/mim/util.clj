(ns mim.util)

(defn generate-token []
  (let [random (java.security.SecureRandom.)
        base64 (.withoutPadding (java.util.Base64/getUrlEncoder))
        buffer (byte-array 32)]
    (.nextBytes random buffer)
    (.encodeToString base64 buffer)))
