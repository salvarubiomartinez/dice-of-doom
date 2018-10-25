(ns dice-of-doom.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def *num-players* 2)
(def *max-dice* 3)
(def *board-size* 2)
(def *board-hexnum* (* *board-size* *board-size*))

(defn gen-board []
  (take *board-hexnum* (repeatedly (fn [] (list (rand-int *num-players*) (+ 1 (rand-int *max-dice*)))))))

(defn player-letter [n]
  (if (= n 0) :A :B))

(defn draw-board [board]
  (dorun
   (map println 
        (map
         (fn [y]
           (apply str
                  (flatten
                   (list
                    (map (fn [x] "   ")
                         (range (- *board-size* y))) 
                    (map (fn [x]
                           (let [hex (nth board (+ x (* *board-size* y)))]
                             (str (player-letter (first hex)) "-" (second hex) " ")))
                         (range *board-size*))))))
         (range *board-size*)))))

(def board (gen-board))

(defn game-tree [board player spare-dice first-move]
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(defn add-passing-move [board player spare-dice first-move moves]
  (if first-move
    moves
    (cons (list nil
                (game-tree (add-new-dice board player (- spare-dice 1))
                           (mod (+ player 1) *num-players*)
                           true))
          moves)))

