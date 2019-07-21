(let ()
  (save-snapshot-and-exit (third *command-line-args*))
  (if (> (length *command-line-args*) 2)
      (load (third *command-line-args*))))
