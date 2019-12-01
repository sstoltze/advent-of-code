;;; http://adventofcode.com/

(ql:quickload '(split-sequence
                cl-json
                yason
                ironclad
                cl-ppcre))



(defun day-one ()
  (let ((input (split-sequence:split-sequence #\Space "R4, R5, L5, L5, L3, R2, R1, R1, L5, R5, R2, L1, L3, L4, R3, L1, L1, R2, R3, R3, R1, L3, L5, R3, R1, L1, R1, R2, L1, L4, L5, R4, R2, L192, R5, L2, R53, R1, L5, R73, R5, L5, R186, L3, L2, R1, R3, L3, L3, R1, L4, L2, R3, L5, R4, R3, R1, L1, R5, R2, R1, R1, R1, R3, R2, L1, R5, R1, L5, R2, L2, L4, R3, L1, R4, L5, R4, R3, L5, L3, R4, R2, L5, L5, R2, R3, R5, R4, R2, R1, L1, L5, L2, L3, L4, L5, L4, L5, L1, R3, R4, R5, R3, L5, L4, L3, L1, L4, R2, R5, R5, R4, L2, L4, R3, R1, L2, R5, L5, R1, R1, L1, L5, L5, L2, L1, R5, R2, L4, L1, R4, R3, L3, R1, R5, L1, L4, R2, L3, R5, R3, R1, L3"))
        (north-pos 0)
        (east-pos 0)
        (facing 0) ;; Mod 4: 0 - N, 1 - E, 2 - S, 3 - W
        (visited (make-hash-table :test #'equalp))
        (current-north 0)
        (current-east 0)
        (first-visit nil)
        (var 0))
    (setf (gethash (cons 0 0) visited) 1)
    (loop for direction in input
       for instruction = (elt direction 0)
       for distance = (parse-integer (subseq direction 1)
                                     :junk-allowed t)
       do
         (if (char-equal instruction #\R)
             (incf facing)
             (decf facing))
         (cond ((= (mod facing 4) 0) (incf north-pos distance))
               ((= (mod facing 4) 1) (incf east-pos  distance))
               ((= (mod facing 4) 2) (decf north-pos distance))
               ((= (mod facing 4) 3) (decf east-pos  distance)))
         (format t "(~D ~D) -> (~D ~D)~%" current-north current-east north-pos east-pos)
         (if (not first-visit)
             (if (= (mod facing 2) 0) ; N-S direction
                 (do ((step (if (= (mod facing 4) 0)
                                1
                                -1))
                      (steps-left distance (1- steps-left)))
                     ((or first-visit (= steps-left 0)))
                   (incf current-north step)
                   (if (>= (incf (gethash (cons current-north
                                                current-east)
                                          visited
                                          0))
                           2)
                       (setq first-visit (cons current-north current-east))))
                 (do ((step (if (= (mod facing 4) 1)
                                1
                                -1))
                      (steps-left distance (1- steps-left)))
                     ((or first-visit (= steps-left 0)))
                   (incf current-east step)
                   (if (>= (incf (gethash (cons current-north
                                                current-east)
                                          visited
                                          0))
                           2)
                       (setq first-visit (cons current-north current-east)))))))
    (values (+ (abs north-pos) (abs east-pos))
            first-visit)))
    
(defun day-two ()
  "Old grid:
123
456
789
New grid:
    1
  2 3 4
5 6 7 8 9
  A B C
    D"
  (let ((input "DUURRDRRURUUUDLRUDDLLLURULRRLDULDRDUULULLUUUDRDUDDURRULDRDDDUDDURLDLLDDRRURRUUUDDRUDDLLDDDURLRDDDULRDUDDRDRLRDUULDLDRDLUDDDLRDRLDLUUUDLRDLRUUUDDLUURRLLLUUUUDDLDRRDRDRLDRLUUDUDLDRUDDUDLLUUURUUDLULRDRULURURDLDLLDLLDUDLDRDULLDUDDURRDDLLRLLLLDLDRLDDUULRDRURUDRRRDDDUULRULDDLRLLLLRLLLLRLURRRLRLRDLULRRLDRULDRRLRURDDLDDRLRDLDRLULLRRUDUURRULLLRLRLRRUDLRDDLLRRUDUDUURRRDRDLDRUDLDRDLUUULDLRLLDRULRULLRLRDRRLRLULLRURUULRLLRRRDRLULUDDUUULDULDUDDDUDLRLLRDRDLUDLRLRRDDDURUUUDULDLDDLDRDDDLURLDRLDURUDRURDDDDDDULLDLDLU
LURLRUURDDLDDDLDDLULRLUUUDRDUUDDUDLDLDDLLUDURDRDRULULLRLDDUDRRDRUDLRLDDDURDUURLUURRLLDRURDRLDURUDLRLLDDLLRDRRLURLRRUULLLDRLULURULRRDLLLDLDLRDRRURUUUDUDRUULDLUDLURLRDRRLDRUDRUDURLDLDDRUULDURDUURLLUDRUUUUUURRLRULUDRDUDRLLDUDUDUULURUURURULLUUURDRLDDRLUURDLRULDRRRRLRULRDLURRUULURDRRLDLRUURUDRRRDRURRLDDURLUDLDRRLDRLLLLRDUDLULUDRLLLDULUDUULLULLRLURURURDRRDRUURDULRDDLRULLLLLLDLLURLRLLRDLLRLUDLRUDDRLLLDDUDRLDLRLDUDU
RRDDLDLRRUULRDLLURLRURDLUURLLLUUDDULLDRURDUDRLRDRDDUUUULDLUDDLRDULDDRDDDDDLRRDDDRUULDLUDUDRRLUUDDRUDLUUDUDLUDURDURDLLLLDUUUUURUUURDURUUUUDDURULLDDLDLDLULUDRULULULLLDRLRRLLDLURULRDLULRLDRRLDDLULDDRDDRURLDLUULULRDRDRDRRLLLURLLDUUUDRRUUURDLLLRUUDDDULRDRRUUDDUUUDLRRURUDDLUDDDUDLRUDRRDLLLURRRURDRLLULDUULLURRULDLURRUURURRLRDULRLULUDUULRRULLLDDDDURLRRRDUDULLRRDURUURUUULUDLDULLUURDRDRRDURDLUDLULRULRLLURULDRUURRRRDUDULLLLLRRLRUDDUDLLURLRDDLLDLLLDDUDDDDRDURRL
LLRURUDUULRURRUDURRDLUUUDDDDURUUDLLDLRULRUUDUURRLRRUDLLUDLDURURRDDLLRUDDUDLDUUDDLUUULUUURRURDDLUDDLULRRRUURLDLURDULULRULRLDUDLLLLDLLLLRLDLRLDLUULLDDLDRRRURDDRRDURUURLRLRDUDLLURRLDUULDRURDRRURDDDDUUUDDRDLLDDUDURDLUUDRLRDUDLLDDDDDRRDRDUULDDLLDLRUDULLRRLLDUDRRLRURRRRLRDUDDRRDDUUUDLULLRRRDDRUUUDUUURUULUDURUDLDRDRLDLRLLRLRDRDRULRURLDDULRURLRLDUURLDDLUDRLRUDDURLUDLLULDLDDULDUDDDUDRLRDRUUURDUULLDULUUULLLDLRULDULUDLRRURDLULUDUDLDDRDRUUULDLRURLRUURDLULUDLULLRD
UURUDRRDDLRRRLULLDDDRRLDUDLRRULUUDULLDUDURRDLDRRRDLRDUUUDRDRRLLDULRLUDUUULRULULRUDURDRDDLDRULULULLDURULDRUDDDURLLDUDUUUULRUULURDDDUUUURDLDUUURUDDLDRDLLUDDDDULRDLRUDRLRUDDURDLDRLLLLRLULRDDUDLLDRURDDUDRRLRRDLDDUDRRLDLUURLRLLRRRDRLRLLLLLLURULUURRDDRRLRLRUURDLULRUUDRRRLRLRULLLLUDRULLRDDRDDLDLDRRRURLURDDURRLUDDULRRDULRURRRURLUURDDDUDLDUURRRLUDUULULURLRDDRULDLRLLUULRLLRLUUURUUDUURULRRRUULUULRULDDURLDRRULLRDURRDDDLLUDLDRRRRUULDDD"))
    (with-input-from-string (stream input)
      (do ((l (read-line stream nil)
              (read-line stream nil))
           (code (list) (cons (cons x-coord y-coord) code))
           (x-coord -2) ;; Old: (x-coord 1)
           (y-coord  0) ;; Old: (y-coord 1)
           )
          ((null l) (reverse code))
        (loop for c across l do
             (cond ((char-equal c #\U)
                    (if (not (and
                              (<= y-coord 0)
                              (= (+ (abs y-coord) (abs x-coord))
                                 2))) ;; Old: (not (= y-coord 0))
                        (decf y-coord)))
                   ((char-equal c #\D)
                    (if (not (and
                              (>= y-coord 0)
                              (= (+ (abs y-coord) (abs x-coord))
                                 2))) ;; Old: (not (= y-coord 2))
                        (incf y-coord)))
                   ((char-equal c #\L)
                    (if (not (and
                              (<= x-coord 0)
                              (= (+ (abs y-coord) (abs x-coord))
                                 2))) ;; Old: (not (= x-coord 0))
                        (decf x-coord)))
                   ((char-equal c #\R)
                    (if (not (and
                              (>= x-coord 0)
                              (= (+ (abs y-coord) (abs x-coord))
                                 2))) ;; Old: (not (= x-coord 2))
                        (incf x-coord)))))))))
    
(defun day-three (directions &optional (robot nil))
  (loop for dir across directions
     with santa-x = 0
     with santa-y = 0
     with robot-x = 0
     with robot-y = 0
     with visited = '((0 0))
     with santas-turn = t
     do
       (if santas-turn
           (case dir
             (#\^ (incf santa-y))
             (#\> (incf santa-x))
             (#\< (decf santa-x))
             (#\v (decf santa-y)))
           (case dir
             (#\^ (incf robot-y))
             (#\> (incf robot-x))
             (#\< (decf robot-x))
             (#\v (decf robot-y))))
       (push (if robot
                 (if santas-turn
                     (list santa-x santa-y)
                     (list robot-x robot-y))
                 (list (+ santa-x robot-x)
                       (+ santa-y robot-y)))
             visited)
       (setq santas-turn (not santas-turn))
     finally (return (length (remove-duplicates visited :test #'equalp)))))

(defun day-four (input &optional (number-zeroes 5) (start 1))
  (loop for i from start
     with match = (make-string number-zeroes :initial-element #\0)
     for current-hash = (ironclad:byte-array-to-hex-string
                         (ironclad:digest-sequence :md5 
                                                   (ironclad:ascii-string-to-byte-array 
                                                    (concatenate 'string
                                                                 input
                                                                 (write-to-string i)))))
     do
       (if (equalp (subseq current-hash 0 number-zeroes)
                   match)
           (return i))))

(defun day-five ()
  (with-open-file (f "./day-five-input")
    (do ((current-line (read-line f nil) 
                       (read-line f nil))
         (nice nil))
        ((null current-line) (length nice))
      (if ;; (and ;; Old version
          ;; (>= (count-if #'(lambda (c) (find c "aeiou")) current-line) 3)
          ;; (loop for i from 0 to (- (length current-line) 2)
          ;;      when (char= (elt current-line i)
          ;;                  (elt current-line (1+ i)))
          ;;      do (return t)
          ;;      finally (return nil))
          ;; (not (or (search "ab" current-line)
          ;;          (search "cd" current-line)
          ;;          (search "pq" current-line)
          ;;          (search "xy" current-line))))
       (loop for i from 0 to (- (length current-line) 3)
          with test-one = nil
          with test-two = nil
          for current-char = (elt current-line i)
          for current-subseq = (subseq current-line i (+ i 2))
          when (search current-subseq
                       current-line 
                       :start2 (+ i 2))
          do
            (setq test-one t)
          when (char-equal current-char (elt current-line (+ i 2)))
          do
            (setq test-two t)
          finally (return (and test-one
                               test-two)))
      (push current-line nice)))))

(defun day-six () ; Noter til den gamle version
  (with-open-file (f "./day-six-input")
    (do ((lights (make-array '(1000 1000) :initial-element 0)) ; nil
         (line (read-line f nil)
               (read-line f nil)))
        ((null line) (loop for x from 0 to 999
                          summing (loop for y from 0 to 999 summing (aref lights x y)))) ; count (aref...)
      (let* ((instructions (split-sequence:split-sequence #\Space line))
             (toggle (equalp (nth 0 instructions) "toggle")))
        (destructuring-bind (x-start y-start)
            (mapcar #'parse-integer
                    (split-sequence:split-sequence #\,
                                                   (nth (if toggle
                                                            1
                                                            2)
                                                        instructions)))
          (destructuring-bind (x-end y-end)
              (mapcar #'parse-integer
                      (split-sequence:split-sequence #\,
                                                     (first
                                                      (last instructions))))
            (loop for x from x-start to x-end
               do
                 (loop for y from y-start to y-end
                    do
                      (incf (aref lights x y)
                            (cond (toggle 2) ; (not (aref lights x y))
                                  ((equalp (nth 1 instructions) "on")
                                   1) ; t
                                  (t -1))) ; nil
                      (if (< (aref lights x y) 0) ; Overflødig i gamle version
                          (setf (aref lights x y) 0))))))))))

(defun day-seven ()
  (labels ((calculate-circuit (hash key)
             (let ((result (gethash key hash)))
;               (setf (gethash key hash)
                     (cond ((null result) 0)
                           ((integerp result) result)
                           ((symbolp result)  (calculate-circuit hash result))
                           (t (case (first result)
                                (OR  (logior (calculate-circuit hash (second result))
                                             (calculate-circuit hash (third  result))))
                                (NOT (lognot (calculate-circuit hash (second result))))
                                (AND (logand (calculate-circuit hash (second result))
                                             (calculate-circuit hash (third  result))))
                                (LSHIFT (ash (calculate-circuit hash (second result))
                                             (calculate-circuit hash (third  result))))
                                (RSHIFT (ash (calculate-circuit hash (second result))
                                             (- (calculate-circuit hash (third result))))))))));)
           (parse-gate (g)
             (if (null g)
                 nil
                 (let ((result (parse-integer g :junk-allowed t)))
                   (if result
                       result
                       (intern (string-upcase g)))))))
    (with-open-file (f "./day-seven-input")
      (do ((line (read-line f nil)
                 (read-line f nil))
           (circuit (make-hash-table))
           (match (cl-ppcre:create-scanner "(\\w+)\\s?(\\w+)?\\s?(\\w+)? -> (\\w+)")))
          ((null line) (calculate-circuit circuit 'a))
                                        ; (match (cl-ppcre:create-scanner "([a-z]+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)")))
                                        ;         (line-regex (cl-ppcre:create-scanner )))
        (cl-ppcre:register-groups-bind ((#'parse-gate x y z result))
            (match line)
;          (format t "~a ~a ~a -> ~a~%" x y z result)
          (setf (gethash result circuit)
                (if (null y) ; Sker kun hvis hverken y eller z matcher
                    x
                    (if (null z)
                        (list x y)
                        (list y x z)))))))))


(defun day-eight ()
  (with-open-file (f "./day-eight-input")
    (do ((line (read-line f nil)
               (read-line f nil))
         (literal 0)
         (value 0)
         (new-encoding 0))
        ((null line) (list (- literal value)
                           (- new-encoding literal)))
      (incf literal (length line))
      (incf new-encoding (+ (loop for c across line counting (or (equalp c #\")
                                                                  (equalp c #\\)))
                            (length line)
                            2))
      (loop for index from 1 to (- (length line) 2)
         for char = (elt line index)
         do
           (if (equalp char #\\)
               (case (elt line (1+ index))
                 (#\x (incf index 3)
                      (incf value))
                 (t   (incf index)
                      (incf value)))
               (incf value))))))

(defun day-eleven (current-password)
  (let ((password (string-downcase current-password)))
    (labels ((increment-password (p)
               (if (= (length p) 0)
                   ""
                   (let* ((length (length p))
                          (all-but-last (subseq p 0 (1- length)))
                          (last-char (elt p (1- length)))
                          (new-char (elt "abcdefghijklmnopqrstuvwxyza"
                                         (1+ (- (char-code last-char)
                                                (char-code #\a))))))
                     (if (char= new-char #\a)
                         (concatenate 'string 
                                      (increment-password all-but-last)
                                      "a")
                         (concatenate 'string 
                                      all-but-last 
                                      (string new-char))))))
             (test-one (p)
               (some #'identity (loop for i from 0 to (- (length p) 3)
                                   collecting (search (subseq p i (+ i 3)) "abcdefghijklmnopqrstuvwxyz"))))
             (test-two (p)
               (not (or (find #\i p)
                        (find #\o p)
                        (find #\l p))))
             (test-three (p)
               (>= (length (remove-duplicates (loop for i from 0 to (- (length p) 2)
                                                 for current = (elt p i)
                                                 for next = (elt p (1+ i))
                                                 when (char= current next)
                                                 collect current)))
                   2)))
      (do ((pass (increment-password password)
                 (increment-password pass)))
          ((and (test-one pass)
                (test-two pass)
                (test-three pass)) pass)))))

(defun run-day-twelve-prime ()
  (let ((file-name "./day-twelve-input"))
    (with-open-file (s file-name)
      (day-twelve-prime (json:decode-json s)))))

(defun day-twelve-prime-helper ()
  (with-open-file (s "./day-twelve-input")
    (json:decode-json s)))

(defun day-twelve-prime (json)
  (cond ((null json) 0)
        ((listp json)
         (do ((d    (car json) (car rest))
              (rest (cdr json) (cdr rest))
              (total 0))
             ((or (null rest)
                  (not (listp rest))) (+ total
                                         (cond ((numberp d) d)
                                               ((listp   d) (day-twelve-prime d))
                                               (t           0))
                                         (cond ((numberp rest) rest)
                                               (t              0))))
           (cond ((numberp d) (incf total d))
                 ((listp   d) (incf total (day-twelve-prime d))))))
        ((numberp json) json)
        (t 0)))

(defun run-day-twelve (&optional (ignore-symbol nil))
  (with-open-file (s "./day-twelve-input")
    (day-twelve (yason:parse (read-line s))
                ignore-symbol)))

(defun day-twelve (json &optional ignore-symbol)
  (cond ((null json) 0)
        ((hash-table-p json)
         (loop for value being the hash-values in json
            using   (hash-key key)
            summing (day-twelve value ignore-symbol)
            summing (day-twelve key   ignore-symbol)
            do
              (if (and ignore-symbol
                       (equalp ignore-symbol value))
                  (return 0))))
        ((listp json) (loop for item in json 
                         summing (day-twelve item)))
        ((numberp json) json)
        (t 0)))

(defun day-fifteen ()
  (labels ((mix-ingredients (ingredients max)
             (if (= max 0)
                 (mapcar #'(lambda (i) (cons 0 i))
                         ingredients)
                                        ; (loop for i from 0 to max do
                                        ; ())
             )))
  (with-open-file (f "./day-fiteen-input")
    (let ((ingredients (do ((result (make-hash-table :test #'equalp))
                            (line (read-line f nil)
                                  (read-line f nil))
                            (ingredients 0 (1+ ingredients))
                            (match (cl-ppcre:create-scanner "([a-z]+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)")))
                           ((null line) result)
                         (cl-ppcre:register-groups-bind (ingredient (#'parse-integer capacity durability flavor texture calories))
                                                        (match line)
                                                        (setf (gethash result ingredient)
                                                              (make-hash-table :test #'equalp
                                                                               :initial-contents
                                                                               (list (cons "name" ingredient)
                                                                                     (cons "capacity" capacity)
                                                                                     (cons "durability" durability)
                                                                                     (cons "flavor" flavor)
                                                                                     (cons "texture" texture)
                                                                                     (cons "calories" calories)))))))
          (max-ingredients 100))
      ;(loop for )
      ))))

(defun day-sixteen ()
  (with-open-file (f "./day-sixteen-input")
    (let ((sues (do ((sues nil)
                     (line (read-line f nil)
                           (read-line f nil))
                     (match (cl-ppcre:create-scanner "Sue (\\d+): ([a-z]+): (\\d+), ([a-z]+): (\\d+), ([a-z]+): (\\d+)")))
                    ((null line) sues)
                  (push (cl-ppcre:register-groups-bind (number first-thing first-number second-thing second-number third-thing third-number)
                                                       (match line)
                                                       (make-hash-table :test #'equalp
                                                                        :initial-contents
                                                                        (list (cons "number"     (parse-integer number))
                                                                              (cons first-thing  (parse-integer first-number))
                                                                              (cons second-thing (parse-integer second-number))
                                                                              (cons third-thing  (parse-integer third-number)))))
                        sues)))
          (gift-sue (make-hash-table :test #'equalp
                                     :initial-contents
                                     (list (cons "children" 3)
                                           (cons "cats" 7)
                                           (cons "samoyeds" 2)
                                           (cons "pomeranians" 3)
                                           (cons "akitas" 0)
                                           (cons "vizslas" 0)
                                           (cons "goldfish" 5)
                                           (cons "trees" 3)
                                           (cons "cars" 2)
                                           (cons "perfumes" 1))))
          (tests (make-hash-table :test #'equalp
                                  :initial-contents
                                  (list (cons "children" #'equalp)
                                        (cons "cats" #'>) ; #'equalp
                                        (cons "samoyeds" #'equalp)
                                        (cons "pomeranians" #'<) ; #'equalp
                                        (cons "akitas" #'equalp)
                                        (cons "vizslas" #'equalp)
                                        (cons "goldfish" #'<) ; #'equalp
                                        (cons "trees" #'>) ; #'equalp
                                        (cons "cars" #'equalp)
                                        (cons "perfumes" #'equalp)))))
      (loop for s in sues 
         when
           (= (length (loop for k being the hash-keys in gift-sue using (hash-value v)
                         when (and (gethash k s)
                                   (funcall (gethash k tests) 
                                            (gethash k s)
                                            v))
                         collect k))
              3)
         collect s))))

(defun day-seventeen ()
  (labels ((fill-options (buckets litres)
             (if (or (= litres 0)
                     (null buckets))
                 '(())
                 (do ((remaining (cdr buckets)
                                 (cdr remaining))
                      (b (car buckets)
                         (car remaining))
                      (results nil))
                     ((null b) results)
                   (if (<= b litres)
                       (setq results
                             (append (mapcar #'(lambda (l) (cons b l))
                                             (fill-options remaining (- litres b)))
                                     results)))))))
    (with-open-file (f "./day-seventeen-input")
      (let ((buckets (do ((line (read-line f nil)
                                (read-line f nil))
                          (result nil))
                         ((null line) result)
                       (push (parse-integer line) 
                             result))))
        ;(count-if #'(lambda (l)
        ;              (= (apply #'+ l)
        ;                 150))
        ;          (fill-options buckets 150))))))
        (let* ((ways (remove-if-not #'(lambda (l)
                                        (= (apply #'+ l)
                                           150))
                                    (fill-options buckets 150)))
               (minimum-length (apply #'min (mapcar #'length ways))))
          (count-if #'(lambda (l) (= (length l) minimum-length))
                    ways))))))

(defun day-twenty ()
  (let* ((input 36000000)
         (houses (make-array input
                             :initial-element 0)))
    (loop for i from 1 ; to (/ input 10)
       do
         (loop for j = i then (+ j i)
              for counter from 1
            do
              (if (or (= counter 50)
                      (>= j input)) (return))
              (incf (aref houses j) (* i 11))) ; 10
         (if (>= (aref houses i) input) (return i)))))
