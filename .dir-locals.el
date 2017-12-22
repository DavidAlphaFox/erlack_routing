((erlang-mode .
  ((eval .
    (setq inferior-erlang-machine-options
     (let* ((d (dir-locals-find-file "."))
            (p (file-name-directory (if (stringp d) d (car d))))
           )
      (list
       "-env" "ERL_LIBS" (expand-file-name "_build/default/lib" p)
      )
     )
    )
   )
  )
 )
)
