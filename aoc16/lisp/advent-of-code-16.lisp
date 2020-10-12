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
        (first-visit nil))
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
