(ns clj-maze.core
  (:gen-class))

(declare create-maze)
(def size 10)

(defn create-rooms []
  (vec
    (for [row (range size)]
      (vec
        (for [col (range size)]
        {:row row, :col col, :visited? false, :bottom? true, :right? true :start? false :end? false})))))


(defn possible-neighbors [rooms row col]
  (vec
    (filter (fn [room]
              (and room (= false (:visited? room))))
    [(get-in rooms [(dec row) col])
    (get-in rooms [(inc row) col])
    (get-in rooms [row (dec col)])
    (get-in rooms [row (inc col)])])))


(defn random-neighbor [rooms row col]
  (let [neighbors (possible-neighbors rooms row col)]
    (if (pos? (count neighbors))
      (rand-nth neighbors)
      nil)))


(defn tear-down-wall [rooms old-row old-col new-row new-col]
  (cond
    ; going up
    (< new-row old-row)
    (assoc-in rooms [new-row new-col :bottom?] false)
    ; going down
    (> new-row old-row)
    (assoc-in rooms [old-row old-col :bottom?] false)
    ; going left
    (< new-col old-col)
    (assoc-in rooms [new-row new-col :right?] false)
    ; going right
    (> new-col old-col)
    (assoc-in rooms [old-row old-col :right?] false)))

(defn endpoint-reached [rooms]
  (> (count
       (set
         (for [row rooms
               room row]
           (:end? room)))) 1))



(defn create-maze [rooms row col first?]
  (let [rooms       (assoc-in rooms [row col :visited?] true)
        next-room   (random-neighbor rooms row col)
        rooms       (if (and (nil? next-room) (not (endpoint-reached rooms)))
                       (assoc-in rooms [row col :end?] true)
                       rooms)
        rooms (if first? (assoc-in rooms [row col :start?] true)
                         rooms)]
    (if next-room
      (loop [old-rooms (tear-down-wall rooms row col (:row next-room) (:col next-room))]
        (let [new-rooms (create-maze old-rooms (:row next-room) (:col next-room) false)]
          (if (= old-rooms new-rooms)
            old-rooms
            (recur new-rooms))))
      rooms)))


(defn print-room [room]
  ; start / bottom / end check
  (cond
    (and (:bottom? room) (:start? room))
    (print ">")
    (:start? room)
    (print "v")
    (and (:bottom? room) (:end? room))
    (print "±")
    (:end? room)
    (print "+")
    (:bottom? room)
    (print "_")
    :else (print " "))
  ; either wall or not
  (if (:right? room)
    (print "|")
    (print " ")))

(defn -main []
  (let [rooms (create-rooms)
        rooms (create-maze rooms 0 0 true)]

    ; print top walls
    (doseq [_ rooms]
      (print " _"))
    (println)
    ; print grid
    (doseq [row rooms]
      (print "|")
      (doseq [room row]
        (print-room room))
        (println))))
