(ns aoc
  (:require [clojure.string  :as str]
            [instaparse.core :as insta]))

(defn day1-1 []
  (let [input (map read-string (str/split (slurp "../input/day1.input") #"\n"))]
    (reduce + input)))

(defn day1-2 []
  (let [input (cycle (map read-string (clojure.string/split (slurp "../input/day1.input") #"\n")))]
    (reduce
     (fn [[cur seen] new]
       (let [new-cur (+ new cur)]
         (if (contains? seen new-cur)
           (reduced new-cur)
           [new-cur (conj seen new-cur)])))
     [0 #{}]
     input)))

(defn letter-count [word]
  (reduce (fn [counts letter]
            (if (contains? counts letter)
              (update counts letter inc)
              (conj counts [letter 1])))
          {}
          word))

(defn day2-1 []
  (let [input (str/split (slurp "../input/day2.input") #"\n")
        counts (map letter-count input)
        two-letters   (count (filter #(contains? (set %) 2) (map vals counts)))
        three-letters (count (filter #(contains? (set %) 3) (map vals counts)))]
    (* two-letters three-letters)))

(defn word-intersection [w1 w2]
  (apply str (map first (filter #(= (first %) (second %)) (map list w1 w2)))))

(defn count-word-differences [w1 w2]
  (- (max (count w1) (count w2))
     (count (word-intersection w1 w2))))

(defn make-pairs [a b]
  (if (empty? a)
    []
    (do (into (make-pairs (rest a) b)
              (map list
                   (repeat (first a))
                   b)))))

(defn day2-2 []
  (let [input (map str/trim (str/split (slurp "../input/day2.input") #"\n"))
        pairs (make-pairs input input)]
    (apply word-intersection
           (first
            (filter #(and (= (count-word-differences (first %) (second %)) 1))
                    pairs)))))

(def day3-parser (insta/parser
                  "
<line> : <'#'> id <whitespace> <'@'> <whitespace> coordinates <':'> <whitespace> area
<numbers> : #'\\d+'
id : numbers
<whitespace> : #'\\s+'
coordinates : x <','> y
x : numbers
y : numbers
area : x <'x'> y
"))

(defn line->map [l]
  (reduce (fn [rest [tag & info]]
            (if (> (count info) 1)
              (assoc rest tag (line->map info))
              (assoc rest tag (read-string (first info)))))
          {}
          l))

(defn insert-row2 [board id x y n]
  (let [new-board (update-in board [x y] conj id)]
    (if (= n 1)
      new-board
      (recur new-board
             id (inc x) y (dec n)))))

(defn insert-patch2 [board id x y n m]
  (if (= m 1)
    (insert-row2 board id x y n)
    (recur (insert-row2 board id x y n)
           id x (inc y) n (dec m))))

(defn insert-fabric2 [board {:keys [id coordinates area]}]
  (insert-patch2 board id
                 (:x coordinates) (:y coordinates)
                 (:x area) (:y area)))

(defn insert-row [board {{:keys [x y]} :coordinates {n :x} :area :keys [id] :as fabric}]
  (let [new-board (update-in board [x y] conj id)]
    (if (= n 1)
      new-board
      (recur new-board
             (update-in (update-in fabric [:coordinates :x] inc)
                        [:area :x] dec)))))

(defn insert-fabric [board {{:keys [x y]} :coordinates {n :x m :y} :area :keys [id] :as fabric}]
  (let [updated-board (insert-row board fabric)]
    (if (= m 1)
      updated-board
      (recur updated-board
             (update-in (update-in fabric [:coordinates :y] inc)
                        [:area :y] dec)))))

(defn get-day3-input [file]
  (map (comp line->map day3-parser str/trim) (str/split (slurp file) #"\n")))

(defn day3-1 []
  (let [input (get-day3-input "../input/day3.input")
        empty (vec (repeat 1000 (vec (repeat 1000 #{}))))
        board (reduce insert-fabric
                      empty
                      input)]
    (reduce +
            (map (comp count (fn [x] (filter #(> (count %) 1) x))) board))))
(defn day3-2 []
  (let [input (get-day3-input "../input/day3.input")
        empty (vec (repeat 1000 (vec (repeat 1000 #{}))))
        board (reduce insert-fabric
                      empty
                      input)
        ids (set (map :id input))
        double-ids (reduce into
                           #{}
                           (reduce (fn [col new] (into col new))
                                   #{}
                                   (map (fn [x] (filter #(> (count %) 1) x)) board)))]
    (remove #(contains? double-ids %)
            ids)))

(def day4-parser (insta/parser
                  "
<log> : time <whitespace> command
time : <'['> date <whitespace> hour  <']'>
date : number '-' number '-' number
hour : number ':' number
<whitespace> : #'\\s+'
<command> : wake | sleep | shift
wake : <'wakes up'>
sleep : <'falls asleep'>
shift : <'Guard #'> number <' begins shift'>
<number> : #'\\d+'
"))

(defn shift? [[_ [command]]]
  (= command :shift))
(defn sleep? [[_ [command]]]
  (= command :sleep))
(defn wake? [[_ [command]]]
  (= command :wake))
(defn next-day [date]
  (let [[year month day] (str/split date #"-")
        day-int (inc (Integer/valueOf day))
        new-day (if (< day-int 10)
                  (str "0" day-int)
                  day-int)]
    (str year "-" month "-" new-day)))
(defn correct-date [date time]
  (let [[hour min] (str/split time #":")]
    (if (= hour "23")
      (next-day date)
      date)))

(defn insert-shift [guards [[_ date time] [_ guard-number]]]
  (update guards guard-number #(assoc % :dates (conj (get % :dates []) (correct-date date time)))))

(defn insert-sleep [days current-guard [[_ sleep-date sleep-time] _] [[_ wake-date wake-time] _]]
  (let [[_ sleep-min] (str/split sleep-time #":")
        [_ wake-min] (str/split wake-time #":")
        sleep (Integer/valueOf sleep-min)
        wake (Integer/valueOf wake-min)]
    (update
     (update days sleep-date #(assoc % :time-slept (+ (get % :time-slept 0)
                                                      (- wake sleep))))
     sleep-date
     #(assoc % :minutes (concat (get % :minutes) (range sleep wake))))))

(defn parse-guard-sleep
  ([guards days current-guard [[time command :as log] & remaining]]
   (if (not time)
     [guards days]
     (cond (shift? log)
           (recur (insert-shift guards log)
                  days
                  (second command)
                  remaining)
           (and (sleep? log)
                (wake? (first remaining)))
           (recur guards
                  (insert-sleep days current-guard log (first remaining))
                  current-guard
                  (rest remaining)))))
  ([commands]
   (parse-guard-sleep {} {} 0 commands)))

(defn get-day4-input [file]
  (map (comp (partial insta/transform
                      {:date str
                       :hour str
                       })
             day4-parser
             str/trim)
       (sort
        (str/split (slurp file) #"\n"))))

(defn time-slept [days dates]
  (transduce (comp (map days) (remove nil?) (map :time-slept))
             +
             0
             dates))

(defn add-time-slept [guards days]
  (transduce (map
              (fn [[k m]]
                [k (assoc m :time-slept (time-slept days (:dates m)))]))
             conj
             {}
             guards))

(defn get-minutes [days dates]
  (transduce (comp (map days) (map :minutes))
             concat
             dates))

(defn day4-1 []
  (let [input (get-day4-input "../input/day4.input")
        [simple-guards days] (parse-guard-sleep input)
        guards (add-time-slept simple-guards days)
        sleepiest-guard (reduce (partial max-key (comp :time-slept second)) guards)
        sleepiest-minute (first
                          (apply
                           (partial max-key count)
                           (partition-by
                            identity
                            (sort
                             (get-minutes days (:dates (second sleepiest-guard)))))))]
    (* (Integer/valueOf (first sleepiest-guard))
       sleepiest-minute)))

(defn day4-2 []
  (let [input (get-day4-input "../input/day4.input")
        [simple-guards days] (parse-guard-sleep input)
        guards (add-time-slept simple-guards days)
        guards-max-minutes (map #(list
                                  (first %)
                                  (apply
                                   (partial max-key count '(nil))
                                   (remove
                                    empty?
                                    (partition-by
                                     identity
                                     (sort
                                      (get-minutes days (:dates (second %))))))))
                                guards)
        [g m] (apply (partial max-key (comp count second))
                     guards-max-minutes)]
    (* (Integer/valueOf g)
       (first m))))

(defn get-day5-input [file]
  (str/trim (slurp file)))

(defn reacts? [a b]
  (and (not= a b)
       (= (str/lower-case a) (str/lower-case b))))

(defn react-reducer [res new]
  (if (empty? res)
    (str res new)
    (let [prev (last res)]
      (if (reacts? prev new)
        (apply str (butlast res))
        (str res new)))))

(defn reduce-polymer [polymer]
  (reduce react-reducer
          ""
          polymer))

(defn day5-1 []
  (let [polymer (get-day5-input "../input/day5.input")
        reduced-polymer (reduce-polymer polymer)]
    (println reduced-polymer)
    (println (count reduced-polymer))))

(defn improve-polymer [polymer c]
  (reduce-polymer (apply str
                         (filter #(not= (str/lower-case c) (str/lower-case %))
                                 polymer))))

(defn day5-2 []
  (let [polymer (reduce-polymer (get-day5-input "../input/day5.input"))]
    (apply min
           (for [c "abcdefghijklmnopqrstuvwxyz"]
             (count (improve-polymer polymer c))))))

(defrecord point [x y])
(defrecord spot [closest distance])
(defn taxicab-distance [p1 p2]
  (+ (Math/abs (- (:x p1) (:x p2)))
     (Math/abs (- (:y p1) (:y p2)))))

(defn board-size [points]
  (inc (max (apply max (map :x points))
            (apply max (map :y points)))))

(defn make-spot [points x y]
  (let [p (->point x y)
        p-distance (partial taxicab-distance p)
        closest-points (first (partition-by p-distance (sort-by p-distance points)))
        closest (first closest-points)]
    (->spot (if (= (count closest-points) 1)
              closest
              ".")
       (taxicab-distance p closest))))

(defn get-day6-input [file]
  (map (comp (partial apply ->point)
             #(list (Integer/valueOf (first %)) (Integer/valueOf (second %)))
             #(str/split % #", ")
             str/trim)
       (str/split (slurp file) #"\n")))

(defn make-board [f points]
  (let [size (board-size points)]
    (loop [x 0
           y 0
           board (vec (repeat size []))]
      (if (= x y size)
        board
        (let [[new-x new-y] (if (= y size)
                              [(inc x) 0]
                              [x (inc y)])
              new-board (update board x #(conj % (f points x y)))]
          (recur new-x new-y new-board))))))

(defn perimeter-points [board]
  (let [size (count board)]
    (into #{}
          (filter #(instance? point %)
                  (concat (map :closest (first board))
                          (map :closest (last board))
                          (loop [x 1
                                 points []]
                            (if (= x size)
                              points
                              (let [row (get board x)]
                                (recur (inc x)
                                       (conj points
                                             (:closest (first row))
                                             (:closest (last row))))))))))))

(defn day6-1 []
  (let [points (get-day6-input "../input/day6.input")
        board (make-board make-spot points)
        perimeter (perimeter-points board)]
    (apply max-key val
           (reduce
            (fn [res new] ; rows
              (merge-with +
                          res
                          (reduce
                           (fn [r n] ; cols
                             (let [c (:closest n)]
                               (if (or (not (instance? point c))
                                       (contains? perimeter c))
                                 r
                                 (assoc r c (inc (get r c 0))))))
                           {}
                           new)))
            {}
            board))))

(defn make-safe-spot [points x y]
  (let [p (->point x y)
        d (reduce + (map #(taxicab-distance p %) points))]
    (->spot "." d)))

(defn day6-2 []
  (let [points (get-day6-input "../input/day6.input")
        board (make-board make-safe-spot points)]
    (reduce
     (fn [res new]
       (+ res
          (reduce
           (fn [r n]
             (if (< (:distance n) 10000)
               (inc r)
               r))
           0
           new)))
     0
     board)))

(def day7-parser (insta/parser
                  "
<requirement> : <'Step '> first <' must be finished before step '> second <' can begin.'>
first : letter
second : letter
<letter> : #'[A-Z]'
"))

(defn get-day7-input [file]
  (map (comp line->map day7-parser str/trim) (str/split (slurp file) #"\n")))

(defn make-connections [m]
  (reduce (fn [res {:keys [first second]}]
            (update res (str second) #(conj (or % #{}) (str first))))
          {}
          m))

(defn topological-sort
  ([available completed connections]
   (if (empty? available)
     '()
     (let [next (first (sort available))
           new-completed (conj completed next)
           new-available (clojure.set/difference
                          (into available
                                (reduce (fn [res [node required]]
                                          (if (every? #(contains? new-completed %) required)
                                            (conj res node)
                                            res))
                                        #{}
                                        connections))
                          new-completed)]
       (cons next
             (topological-sort
              new-available
              new-completed
              connections)))))
  ([available connections]
   (topological-sort available #{} connections)))

(defrecord worker [job time])
(defn worker-started? [worker]
  (not= (:job worker) ""))
(defn worker-finished? [worker]
  (= (:time worker) 0))
(defn work [worker]
  (if (or (not (worker-started? worker))
          (worker-finished? worker))
    worker
    (update worker :time dec)))
(defn create-worker [job]
  (->worker job
     (- (int (get job 0))
        5)))

(defn simultaneous-topological-sort
  ([available completed connections workers time]
   (if (and (every? worker-finished? workers)
            (empty? available))
     (list " - " time " seconds")
     (let [updated-workers (map work workers)
           newly-finished (transduce (comp (filter worker-finished?) (map :job))
                                     conj
                                     []
                                     updated-workers)
           new-completed (into completed newly-finished)
           new-job-count (count newly-finished)
           [new-jobs not-taken] (split-at new-job-count available)
           working-workers (into (remove worker-finished? updated-workers)
                                 (map create-worker new-jobs))
           new-workers (into working-workers (repeat (- (count workers)
                                                        (count working-workers))
                                                     (->worker "" 0)))
           new-available (clojure.set/difference
                          (into available
                                (reduce (fn [res [node required]]
                                          (if (every? #(contains? new-completed %) required)
                                            (conj res node)
                                            res))
                                        #{}
                                        connections))
                          new-completed
                          (set (map :job new-workers)))]
       (concat newly-finished
               (simultaneous-topological-sort new-available
                                              new-completed
                                              connections
                                              new-workers
                                              (inc time))))))
  ([available completed connections workers]
   (simultaneous-topological-sort available completed connections workers 0))
  ([available connections workers]
   (simultaneous-topological-sort available (sorted-set) connections workers))
  ([available connections]
   (simultaneous-topological-sort available connections (vec (repeat 5 (->worker "" 0))))))

(defn day7-1 []
  (let [input (get-day7-input "../input/day7.input")
        connections (make-connections input)
        available (clojure.set/difference
                   (reduce (fn [r [k v]]
                             (conj (into r v)
                                   k))
                           #{}
                           connections)
                   (keys connections))]
    (apply str (topological-sort available connections))))

(defn day7-2 []
  (let [input (get-day7-input "../input/day7.input")
        connections (make-connections input)
        available (clojure.set/difference
                   (reduce (fn [r [k v]]
                             (conj (into r v)
                                   k))
                           (sorted-set)
                           connections)
                   (keys connections))]
    (apply str (simultaneous-topological-sort available connections))))

(defn parse-nodes
  ([d n so-far]
   (if (empty? d)
     so-far
     (if (zero? n)
       [so-far d]
       (let [[[num-children num-metadata] data] (split-at 2 d)
             [children meta] (parse-nodes data num-children [])
             [metadata rest] (split-at num-metadata meta)]
         (recur rest (dec n) (conj so-far {:children children :meta metadata}))))))
  ([d n]
   (parse-nodes d n []))
  ([d]
   (parse-nodes d 1)))

(defn get-day8-input [file]
  (map read-string (str/split (slurp file) #"\s")))

(defn sum-metadata [nodes]
  (if (contains? nodes :meta)
    (+ (reduce + (:meta nodes))
       (sum-metadata (:children nodes)))
    (reduce + (map sum-metadata nodes))))

(defn node-value [node]
  (if (empty? (:children node))
    (reduce + 0 (:meta node))
    (let [valuable-children (map #(get (:children node) (dec %)) (:meta node))]
      (reduce + (map node-value valuable-children)))))

(defn day8-1 []
  (let [input (get-day8-input "../input/day8.input")]
    (sum-metadata (parse-nodes input))))

(defn day8-2 []
  (let [input (get-day8-input "../input/day8.input")]
    (node-value (first (parse-nodes input)))))

(defn power [serial x y]
  (-> x
      (+ 10)
      (* y)
      (+ serial)
      (* (+ x 10))
      (mod 1000)
      (quot 100)
      (- 5)))

(defn area-power
  ([serial size x y]
   (reduce + 0
           (map #(apply power serial %)
                (for [i (range x (+ x size)) j (range y (+ y size))]
                  [i j]))))
  ([serial x y]
   (area-power serial 3 x y )))

(defn day11-1 []
  (let [serial 5535
        size 300
        grid (for [i (range 1 (- size 2)) j (range 1 (- size 2))]
               [i j])]
    (apply max-key #(apply area-power serial %) grid)))

(defn make-grid [grid-size area-size]
  (for [i (range 1 (- grid-size (dec area-size))) j (range 1 (- grid-size (dec area-size)))]
               [i j]))

(defn day11-2 []
  (let [serial 5535
        max-grid-size 300]
    (loop [max-power [0 0]
           area 1]
      (println area)
      (if (= area 300)
        max-power
        (recur (max-key second
                        max-power
                        (list area
                              (apply max-key
                                     #(apply area-power serial area %)
                                     (make-grid max-grid-size area))))
               (inc area))))))
