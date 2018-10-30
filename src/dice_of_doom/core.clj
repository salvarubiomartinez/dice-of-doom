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

(defn gen-board' []
  (take *board-hexnum* (repeatedly (fn [] (list (rand-int *num-players*) (+ 1 (rand-int *max-dice*)))))))

(defn gen-board []
  (apply vector (for [x (range *board-hexnum*)]
                  (list (rand-int *num-players*) (+ 1 (rand-int *max-dice*))))))

(defn player-letter [n]
  (if (= n 0) :A :B))

(defn draw-board' [board]
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

(defn draw-board [board]
  (doseq [y (range *board-size*)]
    (doseq [x (range (- *board-size* y))]
      (print "   "))
    (doseq [x (range *board-size*)]
      (let [hex (nth board (+ x (* *board-size* y)))]
        (print (str (player-letter (first hex)) "-" (second hex) " "))))
    (println "")))

(def board (gen-board))

(declare game-tree)
(declare add-passing-move)
(declare attacking-moves)
(declare board-attack)
(declare neighbors)
(declare add-new-dice)

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
                           0
                           true))
          moves)))

(defn attacking-moves' [board cur-player spare-dice]
  (let [player (fn [pos] (first (board pos)))
        dice (fn [pos] (second (board pos)))]
    (map (fn [src]
           (when (= (player src) cur-player)
             (map (fn [dst]
                    (when (and (not (= (player dst) cur-player))
                               (> (dice src) (dice dst)))
                      (list
                       (list (list src dst)
                             (game-tree (board-attack board cur-player src dst (dice src))
                                        cur-player
                                        (+ spare-dice (dice dst))
                                        nil)))))
                  (neighbors src))))
         (range *board-hexnum*))))

(defn attacking-moves [board cur-player spare-dice]
  (let [player (fn [pos] (first (board pos)))
        dice (fn [pos] (second (board pos)))]
    (for [src (range *board-hexnum*)
          :when (= (player src) cur-player)
          dst (neighbors src)
          :when (and (not (= (player dst) cur-player))
                     (> (dice src) (dice dst)))]        
      (list (list src dst)
            (game-tree (board-attack board cur-player src dst (dice src))
                       cur-player
                       (+ spare-dice (dice dst))
                       nil)))))

(defn attacking-moves'' [board cur-player spare-dice]
  (let [player (fn [pos] (first (board pos)))
        dice (fn [pos] (second (board pos)))
        source (filter (fn [src] (= (player src) cur-player)) (range *board-hexnum*))
        destination (neighbors source)]
    ()))

(defn neighbors [pos]
  (let [up (- pos *board-size*)
        down (+ pos *board-size*)]
    (for [p (concat (list up down)
                    (when-not (zero? (mod pos *board-size*))
                      (list (- up 1) (- pos 1)))
                    (when-not (zero? (mod (+ pos 1) *board-size*))
                      (list (+ pos 1) (+ down 1))))
          :when (and (>= p 0) (< p *board-hexnum*))]
      p)))

(defn board-attack [board player src dst dice]
  (apply vector (for [pos (range *board-hexnum*)
                      :let [hex (board pos)]]
                  (cond (= pos src) (list player 1)
                        (= pos dst) (list player (- dice 1))
                        :else hex))))

(defn add-new-dice [board player spare-dice]
  (letfn [(f [lst n]
            (cond (nil? lst) nil
                  (zero? n) lst
                  :else (let [cur-player (first (first lst))
                              cur-dice (first (rest (first lst)))]
                          (if (and (= cur-player player) (< cur-dice *max-dice*))
                            (cons (list cur-player (+ cur-dice 1))
                                  (f (rest lst) (- n 1)))
                            (cons (first lst) (f (rest lst) n))))))]
    (apply vector (f (apply list board) spare-dice))))

(declare play-vs-human)
(declare print-info)
(declare handle-human)
(declare announce-winner)

(defn play-vs-human [tree]
  (print-info tree)
  (if (not-empty (nth tree 2) )
    (play-vs-human (handle-human tree))
    (announce-winner (nth tree 1))))

(defn print-info [tree]
  (println "")
  (println (str "current player = " (player-letter (first tree))))
  (draw-board (second tree)))

(defn handle-human [tree]
  (println "choose your move:")
  (let [moves (nth tree 2)]
    (doseq [move moves]
      (let [action (first move)]
        (print (str (+ (.indexOf moves move) 1) ". "))
        (if (not (nil? action))
          (println (str (first action) " -> " (second action)))
          (println "end turn"))))
    (do (println "")
    (second (nth  moves (- (read) 1))))))

(defn winners [board]
  (let [tally (map (fn [hex] (first hex)) board)
        totals (frequencies tally)
        best (apply max (vals totals))]
    (map first (filter (fn [x] (= (second x) best)) totals))))

(defn announce-winner [board]
  (let [w (winners board)]
    (if (> (count w) 1)
      (println (str "The game is a tie between " (apply str (map player-letter w))))
      (println (str "The winner is " (player-letter (first w)))))))
