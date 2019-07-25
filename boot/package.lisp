(define (package-get-meta pkg key) (ht-at (package-meta pkg) key))
(define (package-set-meta pkg key value) (ht-at-put (package-meta pkg) key value))

(define (package-get-subpackages pkg)
    (let ((exist (package-get-meta pkg 'subpackages)))
      (if-nil? exist
               (let ()
                 (set! exist (make-st))
                 (package-set-meta pkg 'subpackages exist)))
      exist))

(define (package-get-subpackage-nametable pkg)
    (let ((exist (package-get-meta pkg 'subpackage-nametable)))
      (if-nil? exist
               (let ()
                 (set! exist (make-ht))
                 (package-set-meta pkg 'subpackage-nametable exist)))
      exist))

(define (package-add-subpackage parent child name)
    (ht-at-put (package-get-subpackages parent) name child)
  (ht-at-put (package-get-subpackage-nametable parent) child name)
  (package-set-meta child 'parent parent))

(define (package-find-subpackage pkg name)
    (ht-at (package-get-subpackages pkg) name))

(define (package-canonical-path pkg)
    (if (nil? (package-get-meta pkg 'canonical-path))
        (let ((path '())
              (loop #f))
          (set! loop (lambda (curr parent)
                       (unless (nil? parent)
                         (set! path (cons (ht-at (package-get-subpackage-nametable parent) curr) path))
                         (loop parent (package-get-meta parent 'parent)))))
          (loop pkg (package-get-meta pkg 'parent))
          (package-set-meta pkg 'canonical-path (reverse-list path))))
  (package-get-meta pkg 'canonical-path))

;; TODO: we will want to be able to 'symlink' packages, i.e. nicknames
(define (package-relative-path pkg relative-to)
    (let ((a (package-canonical-path pkg))
          (b (package-canonical-path relative-to))
          (loop #f))
      (set! loop (lambda (a b)
                   (if (not (or (nil? a) (nil? b)))
                       (if (string-equal (car a) (car b))
                           (loop (cdr a) (cdr b))
                           a)
                       a)))
      (if (and (not (or (nil? a) (nil? b)))
               (string-equal (car a) (car b)))
          (loop a b)
          (cons 'root a))))

(define root-package (%make-package "root"))
(package-add-subpackage root-package *package* "lang")

(define (find-package-by-path path)
    (let* ((root? (eq (car path) 'root))
           (pkg (if root? root-package *package*)))
      (when root? (set! path (cdr path)))
      (dolist (name path)
        (set! pkg (package-find-subpackage pkg name))
        (if-nil? pkg (throw `(could not find package at ,path))))
      pkg))
