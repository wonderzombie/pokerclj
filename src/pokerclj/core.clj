(ns pokerclj.core)

(def allsuits
  ['C 'D 'H 'S])

(def allranks
  ".A23456789TJQKA")

(def deck
  (for [s allsuits
        r (drop 3 allranks)]
    (list r s)))

(defn parseint [x]
  (-> x str read-string))

(defn rankforcard [c]
  (.indexOf allranks c))

(defn deal [nhands ncards]
  (let [decks (iterate #(drop ncards %) (shuffle deck))
        draws (take nhands decks)]
    (map #(take ncards %) draws)))

(defn ranks [hand]
  (sort (map rankforcard (map first hand))))

(defn nkinds? [n ranks]
  (get (group-by count (partition-by identity ranks)) n))

(defn straight? [hand]
  (let [ranks   (ranks hand)
        minr    (parseint (last ranks))
        ncards  (count ranks)]
    (= (range minr ncards) (map parseint ranks))))

(defn flush? [hand]
  (let [suits (map second hand)
        s (first suits)]
    (every? #(= s %) suits)))

(defn royalflush? [hand]
  (let [isf (flush? hand)
        iss (straight? hand)]
    (and isf iss)))