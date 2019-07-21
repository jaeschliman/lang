(let ()
  (save-snapshot-and-exit "./cmdline.amb")
  (if (> (length *command-line-args*) 2)
      (load (third *command-line-args*))))
