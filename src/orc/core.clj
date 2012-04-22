(ns orc.core)

(defn randval [n]
  (inc (rand-int (max 1 n))))

(defn read-from-in []
  (swank.core/with-read-line-support (read-line)))

(defn fresh-line []
  (println ""))

(defn read-int []
  (let [x (clojure.string/replace (read-from-in) "\n" "")]
    (if (every? #(Character/isDigit %) x)
      (Integer/parseInt x)
      -1)))

(defn monster-dead? [m]
  (<= (:health m) 0))

(defn monsters-dead? [ms]
  (every? monster-dead? ms))

::monster
(defmulti monster-show (fn [m] (:type m)))
(defmulti monster-attack :type)
(defmulti monster-hit :type)

(defmethod monster-show ::monster [m]
  (str "A " (:type m) " with " (:health m) " health."))

(defn monster-name [t]
  (case t
    ::orc "orc"
    ::hydra "hydra"
    ::brigand "brigand"
    ::slime-mold "slime-mold"))

(defn hit-monster [m amt]
  (assoc m :health (max 0 (- (:health m) amt))))

(defmethod monster-hit ::monster [m amt]
  (let [m1 (hit-monster m amt)]
    {:attack
     (if (monster-dead? m1)
       (str "You killed the " (monster-name (:type m)) "! ")
       (str "You hit the " (monster-name (:type m)) ", knocking off " amt " health points! "))
     :monster m1}))
        
  
(defn orc [health club-level]
  {:type ::orc :health health :club-level club-level})

(defn make-orc []
  (orc (randval 10) (randval 8)))

(derive ::orc ::monster)

(defmethod monster-show ::orc [m]
    (str "A wicked orc with a level "
         (:club-level m)
         " club."))

(defmethod monster-attack ::orc [m player]
    (let [x (randval (:club-level m))]
      {:attack
       (str "An orc swings his club at you and knocks off "
            x
            " of your health points.")
       :player (hit-player player :health x)}))

(defn hydra [health]
  {:type ::hydra :health health})

(defn make-hydra []
  (hydra (randval 10)))

(derive ::hydra ::monster)

(defmethod monster-show ::hydra [m]
  (str "A hydra with " (:health m) " heads!"))

(defmethod monster-attack ::hydra [m player]
    (let [x (randval (Math/round (/ (:health m) 2.0)))]
      {:attack
       (str "A hydra attacks you with "
            x
            " of its heads! It also grows back one more head!")
       :player (hit-player player :health x)
       :monster (hydra (inc (:health m)))}))

(defmethod monster-hit ::hydra [m amt]
  (let [m1 (hit-monster m amt)]
    {:attack
     (if (monster-dead? m1)
       "The corpse of the fully decapitated and decapacitated hydra falls to the floor!"
       (str "You lop off " amt " of the hydra's heads! "))
     :monster m1}))

(defn slime-mold [health sliminess]
  {:type ::slime-mold :health health :sliminess sliminess})

(defn make-slime []
  (slime-mold (randval 10) (randval 5)))

(derive ::slime-mold ::monster)

(defmethod monster-show ::slime-mold [m]
  (str "A slime mold with a sliminess of "
       (:sliminess m) "."))

(defmethod monster-attack ::slime-mold [m player]
  (let [x (randval (:sliminess m))
        p (hit-player player :agility x)
        squirt? (zero? (rand-int 2))]
    {:attack
     (str "A slime mold wraps around your legs and decreases your agility by "
          x "."
          (when squirt?
            "It also squirts in your face, taking away a health point! "))
     :player (if squirt?
               (hit-player p :health 1)
               p)}))

(defn brigand [health]
  {:type ::brigand :health health})

(defn make-brigand []
  (brigand (randval 10)))

(derive ::brigand ::monster)

(defmethod monster-show ::monster [m]
  (str "A brigand with " (:health m) " health."))

(defmethod monster-attack ::brigand [m player]
  (let [attr (reduce (fn [x y]
                      (if (> (y player) (x player))
                          y
                          x))
                     [:health :agility :strength])]
    {:attack
     (case attr
       :health "A brigand hits you with his slingshot, taking off 2 health points! "
       :agility "A brigand catches your leg with his whip, taking off 2 agility points! "
       :strength "A brigand cuts your arm with his whip, taking off 2 strength points! ")
     :player (hit-player player attr 2)}))
         

(defn monster-builder []
  [make-brigand make-slime make-hydra make-orc])

(defn init-monsters [n]
  (let [m-builder (monster-builder)]
    (vec (map (fn [_] ((nth m-builder
                            (dec (randval (count m-builder))))))
              (range n)))))

(defn random-monster [monsters]
  (let [n (dec (randval (count monsters)))]
        (if (monster-dead? (nth monsters n))
          (random-monster monsters)
          n)))

(defn create-player [max-health max-agility max-strength]
  {:health max-health :max-health max-health :agility max-agility :max-agility max-agility :strength max-strength :max-strength max-strength})

(defn hit-player [player attr dmg]
  (assoc player attr (max 0 (- (attr player) dmg))))

(defn player-dead? [player]
  (<= (:health player) 0))

(defn show-player [player]
  (str "You are a valiant knight with a health of "
       (:health player)
       ", an agility of "
       (:agility player)
       ", and a strength of "
       (:strength player)))

(defn level-up [player attr amt]
  (assoc player attr (+ (attr player) amt)))

(defn heal [player]
  (assoc player
    :health (:max-health player)
    :agility (:max-agility player)
    :strength (:max-strength player)))


(defn pick-monster [monsters]
  (fresh-line)
  (print "Monster #:")
  (let [x (read-int)]
    (if (not (and (>= x 1) (<= x (count monsters))))
      (do
        (println "That is not a valid monster number")
        (pick-monster monsters))
      (let [m (nth monsters (dec x))]
        (if (monster-dead? m)
          (do (println "That monster is already dead")
              (pick-monster monsters))
          (dec x))))))

(defn show-monsters [ms]
  (fresh-line)
  (print "Your foes:")
  (doseq [[m c] (map list ms (range (count ms)))]
    (fresh-line)
    (print (str "    " (inc c) "."
                (if (monster-dead? m)
                  "***DEAD***"
                  (str "(Health=" (:health m) ") "
                       (monster-show m)))))))

(defn hit-nth-monster [monsters n hit]
  (let [r (monster-hit (nth monsters n) hit)]
    (print (:attack r))
    (assoc monsters n (:monster r))))

(defn stab-attack [player monsters]
  (hit-nth-monster monsters (pick-monster monsters)
                   (+ 2 (randval (Math/floor (/ (:strength player) 2))))))

(defn double-swing-attack [player monsters]
  (let [x (randval (Math/floor (/ (:strength player) 6)))]
              (print (str "Your double swing has a strength of " x))
              (let [m2 (hit-nth-monster monsters (pick-monster monsters) x)]
                (if (not (monsters-dead? m2))
                  (hit-nth-monster m2 (pick-monster m2) x)))))

(defn roundhouse-attack [player monsters]
  (let [hits (inc (randval (Math/floor (/ (:strength player) 3))))]
    (loop [m monsters h hits]
      (if (or (<= h 0) (monsters-dead? m))
        m
        (recur (hit-nth-monster m (random-monster m) 1) (dec h))))))
      
(defn player-attack [player monsters]
  (fresh-line)
  (print "Attach style: [s]tab [d]ouble swing [r]oundhouse:")
  (let [a (read-from-in)]
    (case a
      "s\n" (stab-attack player monsters)
      "d\n" (double-swing-attack player monsters)
      (roundhouse-attack player monsters))))

(defn- attack-monsters [[player monsters] n]
  (if (monsters-dead? monsters)
    [player monsters]
    (do
      (show-monsters monsters)
      [player (player-attack player monsters)])))

(defn num-attacks [player]
  (inc (Math/floor (/ (max 0 (:agility player)) 15))))

(defn player-round [player monsters]
  (second
   (reduce attack-monsters [player monsters]
           (range (num-attacks player)))))
  
(defn- attack-player [player monster]
  (if (monster-dead? monster)
    player
    (let [r (monster-attack monster player)]
      (print (:attack r))
      (:player r))))

(defn monster-round [player monsters]
  (reduce attack-player player monsters))

(defn level-loop [player monsters]
  (loop [m monsters p player]
    (if (or (monsters-dead? m) (player-dead? p))
      p
      (do
        (print (str "\n" (show-player p)))
        (let [m2 (player-round p m)]
          (recur m2 (monster-round p m2)))))))

(defn orc-battle []
  (let [player (level-loop (create-player 30 30 30) (init-monsters 12))]
    (if (player-dead? player)
      (println "\nYou have been killed. Game Over.")
      (println "\nCongratulations! You have vanquished all of your foes."))))