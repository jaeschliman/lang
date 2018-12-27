(set-symbol-value 'set set-symbol-value)
(set 'ignore1 (lambda (_)))

(set 'font (load-image "./res/charmap-futuristic_white.png"))
(set 'font-start 32)
(set 'font-chars-per-row 18)
(set 'font-char-width 7)
(set 'font-char-height 9)
(set 'font-char-size (make-point font-char-width font-char-height))

(set 'output (make-image 500 500))

(set 'blit-charcode-at
     (lambda (raw-code point)
       (let ((code (-i raw-code font-start)))
         (let ((col (%i code font-chars-per-row))
               (row (/i code font-chars-per-row)))
           (let ((origin (make-point (*i col font-char-width)
                                     (*i row font-char-height))))
             (blit
              font output point
              origin (point+ origin font-char-size)
              1.0 0.0))))))

(set 'screen-size #f)

(set 'strloop #f)
(set 'strloop (lambda (str fn idx len)
                (if (<i idx len)
                    (let ()
                      (fn (char-code-at str idx) idx)
                      (strloop str fn (+i 1 idx) len)))))

(set 'map-charcodes-with-index (lambda (str fn)
                                 (strloop str fn 0 (string-length str))))

(set 'display-string (lambda (at str)
                       (map-charcodes-with-index
                        str
                        (lambda (char idx)
                          (blit-charcode-at char
                                            (point+ at (make-point
                                                        (*i idx font-char-width)
                                                        0)))))
                       (blit-to-screen output 0@0 400 0)))

;;;;;;; register event handlers

(set 'onshow (lambda (w h)
               (set 'screen-size (make-point w h))
               (display-string 0@0 "hello, world!")))
(set 'onmousemove ignore1)
(set 'onmousedown ignore1)
(set 'onmousedrag ignore1)
(set 'onkey ignore1)
