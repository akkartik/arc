(= persisted-vars* nil persisted-dir* "snapshots/")
(mac persisted (var init)
  (w/uniq (val file)
    `(do1 (fromdisk ,var
                    ,(+ persisted-dir* (string var))
                    ,init
                    unserialize:readfile1
                    (fn (,val ,file) (writefile (serialize ,val) ,file)))
          (pushnew ',var persisted-vars*))))

(const autosave-interval* 300) ; seconds
(def autosave-thread ()
  (thread "autosave" ; for persisted vars
    (while t
      (each var persisted-vars*
        (eval `(todisk ,var)))
      (sleep autosave-interval*))))
(autosave-thread)
