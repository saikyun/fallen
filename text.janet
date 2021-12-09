(defn capitalize
  [s]
  (string
    (string/ascii-upper (string/slice s 0 1))
    (string/slice s 1)))