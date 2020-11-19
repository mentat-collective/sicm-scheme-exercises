(require '[sicmutils.env :use :all])

(defn total-distance [x1 y1 x2 y2] 
    (fn [xp]
        (+ (sqrt (+ (square (+ x1 xp))
                    (square y1)))
            (sqrt (+ (square (- x2 (+ x1 xp)))
                    (square y2))))))

(defn total-distance* [x1 y1 x2 y2] 
    (fn [xp]
        (up (sqrt (+ (square (+ x1 xp))
                        (square y1)))
            (sqrt (+ (square (- x2 (+ x1 xp)))
                        (square y2))))))

((total-distance* 1 2 3 4) 10)

