; HTTP Server.

; To improve performance with static files, set static-max-age*.

(= arcdir* "www/" logdir* "www/logs/" staticdir* "www/static/")

(= quitsrv* nil breaksrv* nil)

(def serve (? port 8080)
  (wipe quitsrv*)
  (ensure-srvdirs)
  (map [apply new-bgthread _] pending-bgthreads*)
  (w/socket s port
    (prn "ready to serve port " port)
    (flushout)
    (= currsock* s)
    (until quitsrv*
      (handle-request s breaksrv*)))
  (prn "quit server"))

(def serve1 (? port 8080)
  (w/socket s port (handle-request s t)))

(def ensure-srvdirs ()
  (map ensure-dir (list arcdir* logdir* staticdir*)))

(= srv-noisy* nil)

(mac wrapper (var . body)
  `(fn params (if params (do ,@body) ,var)))

; http requests currently capped at 2 meg by socket-accept

; should threads process requests one at a time? no, then
; a browser that's slow consuming the data could hang the
; whole server.

; wait for a connection from a browser and start a thread
; to handle it. also arrange to kill that thread if it
; has not completed in threadlife* seconds.

(= threadlife* 30  requests* 0  requests/ip* (table)
   throttle-ips* (table)  ignore-ips* (table)  spurned* (table))

(def handle-request (s breaksrv)
  (if breaksrv
      (handle-request-1 s)
      (errsafe (handle-request-1 s))))

(def handle-request-1 (s)
  (++ requests*)
  (withs ((i o ip)    (socket-accept s)
          ip-wrapper  (wrapper ip (= ip car.params))
          th1         nil
          th2         nil)
    (= th1 (thread
             (after (handle-request-thread i o ip-wrapper)
                    (close i o)
                    (if th2 (kill-thread th2)))))
    (= th2 (thread
             (sleep threadlife*)
             (unless (and th1 (dead th1))
               (prn "srv thread took too long for " ip))
             (if th1 (kill-thread th1))
             (force-close i o)))))

; Returns true if ip has made req-limit* requests in less than
; req-window* seconds.  If an ip is throttled, only 1 request is
; allowed per req-window* seconds.  If an ip makes req-limit*
; requests in less than dos-window* seconds, it is a treated as a DoS
; attack and put in ignore-ips* (for this server invocation).

; To adjust this while running, adjust the req-window* time, not
; req-limit*, because algorithm doesn't enforce decreases in the latter.

(= req-times* (table) req-limit* 30 req-window* 10 dos-window* 2)

(wipe show-abuse*)
(def abusive-ip (ip)
  (++ (requests/ip* ip 0))
  (if show-abuse*
    (if (ignore-ips* ip)
      (prn ip " ignored")
      (prn ip " " (if (abusive-ip-core ip) "" "not ") "abusive (" requests/ip*.ip ")")))
  (and (or (ignore-ips* ip) (abusive-ip-core ip))
       (++ (spurned* ip 0))))

(def abusive-ip-core (ip)
  (and (only.> (requests/ip* ip) 250)
       (~is ip "127.0.0.1")
       (let now (seconds)
         (do1 (if (req-times* ip)
                  (and (>= (qlen (req-times* ip))
                           (if (throttle-ips* ip) 1 req-limit*))
                       (let dt (- now (deq (req-times* ip)))
                         (if (< dt dos-window*) (set (ignore-ips* ip)))
                         (< dt req-window*)))
                  (do (= (req-times* ip) (queue))
                      nil))
              (enq now (req-times* ip))))))

(let proxy-header "X-Forwarded-For: "
  (def strip-header (s)
    (subst "" proxy-header s))
  (def proxy-ip (ip-wrapper lines)
    (aif (only.strip-header (car:keep [headmatch proxy-header _] lines))
        (ip-wrapper it)
        (ip-wrapper))))

(wipe show-requests*)
(def handle-request-thread (i o ip-wrapper)
  (with (nls 0 lines nil line nil responded nil t0 (msec))
    (after
      (whilet c (unless responded (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2)
                (do
                  (handle-request-2 lines i o ip-wrapper t0)
                  (set responded))
                (do (push (string (rev line)) lines)
                    (wipe line)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close i o)))
  (harvest-fnids))

(def handle-request-2 (lines i o ip-wrapper t0)
  (let (type op args n cooks) (parseheader (rev lines))
    (if show-requests* (prn lines))
    (unless (abusive-ip (proxy-ip ip-wrapper lines))
      (handle-request-3 type i o op args n cooks (ip-wrapper) t0))))

(def handle-request-3 (type i o op args n cooks ip t0)
  (let t1 (msec)
    (let arg-wrapper (wrapper args (zap [+ _ car.params] args))
      (case type
        'get  (respond o op args cooks ip)
        'post (handle-post i o op arg-wrapper n cooks ip)
              (respond-err o "Unknown request: " ip " " type)))
    (log-request type op args cooks ip t0 t1)))

(def log-request (type op args cooks ip t0 t1)
  (with (parsetime (- t1 t0) respondtime (- (msec) t1))
    (srvlog 'srv ip
                 parsetime
                 respondtime
                 (if (> (+ parsetime respondtime) 1000) "***" "")
                 type
                 op
                 args
                 cooks)))

; Could ignore return chars (which come from textarea fields) here by
; (unless (is c #\return) (push c line))

(def handle-post (i o op arg-wrapper n cooks ip)
  (if srv-noisy* (pr "Post Contents: "))
  (if (no n)
      (respond-err o "Post request without Content-Length.")
      (let line nil
        (whilet c (and (> n 0) (readc i))
          (if srv-noisy* (pr c))
          (-- n)
          (push c line))
        (if srv-noisy* (pr "\n\n"))
        (arg-wrapper (parseargs (string (rev line))))
        (respond o op (arg-wrapper) cooks ip))))

(= header* "HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8
Connection: close")

(= type-header* (table))

(def gen-type-header (ctype)
  (+ "HTTP/1.0 200 OK
Content-Type: "
     ctype
     "
Connection: close"))

(map (fn ((k v)) (= (type-header* k) (gen-type-header v)))
     '((gif       "image/gif")
       (jpg       "image/jpeg")
       (png       "image/png")
       (ico       "image/x-icon")
       (swf       "application/x-shockwave-flash")
       (text/css  "text/css")
       (text/javascript "text/javascript")
       (text/html "text/html; charset=utf-8")))

(= rdheader* "HTTP/1.0 302 Moved")

(= srvops* (table) redirector* (table) optimes* (table) opcounts* (table))

(def save-optime (name elapsed)
  ; this is the place to put a/b testing
  ; toggle a flag and push elapsed into one of two lists
  (++ (opcounts* name 0))
  (unless (optimes* name) (= (optimes* name) (queue)))
  (enq-limit elapsed (optimes* name) 1000))

; For ops that want to add their own headers.  They must thus remember
; to prn a blank line before anything meant to be part of the page.

(mac defop-raw (name parms . body)
  (w/uniq t1
    `(= (srvops* ',name)
        (fn ,parms
          (let ,t1 (msec)
            (do1 (do ,@body)
                 (save-optime ',name (- (msec) ,t1))))))))

(mac defopr-raw (name parms . body)
  `(= (redirector* ',name) t
      (srvops* ',name)     (fn ,parms ,@body)))

(mac defop (name parm . body)
  (w/uniq gs
    `(do (wipe (redirector* ',name))
         (defop-raw ,name (,gs ,parm)
           (w/stdout ,gs (prn) ,@body)))))

; Defines op as a redirector.  Its retval is new location.

(mac defopr (name parm . body)
  (w/uniq gs
    `(do (set (redirector* ',name))
         (defop-raw ,name (,gs ,parm)
           ,@body))))

;(mac testop (name . args) `((srvops* ',name) ,@args))

(deftem request
  args  nil
  cooks nil
  ip    nil)

(= unknown-msg* "Unknown." max-age* (table) static-max-age* 3600)

(def respond (str op args cooks ip)
  (w/stdout str
    (iflet f (srvops* op)
           (let req (inst 'request 'args args 'cooks cooks 'ip ip)
             (if (redirector* op)
                 (do (prn rdheader*)
                     (prn "Location: " (f str req))
                     (prn))
                 (do (prn header*)
                     (awhen (max-age* op)
                       (prn "Cache-Control: max-age=" it))
                     (f str req))))
           (when (secure? op)
             (let filetype (static-filetype op)
               (aif (and filetype (file-exists (string staticdir* op)))
                    (do (prn (type-header* filetype))
                        (awhen static-max-age*
                          (prn "Cache-Control: max-age=" it))
                        (prn)
                        (w/infile i it
                          (whilet b (readb i)
                            (writeb b str))))
                    (respond-err str unknown-msg*)))))))

(def secure? (sym)
  (~posmatch ".." string.sym))

(def static-filetype (sym)
  (case (downcase (last (check (tokens string.sym #\.) ~single)))
    "gif"  'gif
    "jpg"  'jpg
    "jpeg" 'jpg
    "png"  'png
    "ico"  'ico
    "swf"  'swf
    "css"  'text/css
    "js"   'text/javascript
    "txt"  'text/html
    "htm"  'text/html
    "html" 'text/html
    "arc"  'text/html
    ))

(def respond-err (str msg . args)
  (w/stdout str
    (prn header*)
    (prn)
    (apply pr msg args)))

(def parseheader (lines)
  (let (type op args) (parseurl (car lines))
    (list type
          op
          args
          (and (is type 'post)
               (some (fn (s)
                       (and (begins downcase.s "content-length:")
                            (errsafe:coerce (cadr (tokens s)) 'int)))
                     (cdr lines)))
          (some (fn (s)
                  (and (begins downcase.s "cookie:")
                       (parsecookies s)))
                (cdr lines)))))

; (parseurl "GET /p1?foo=bar&ug etc") -> (get p1 (("foo" "bar") ("ug")))

(def parseurl (s)
  (let (type url) (tokens s)
    (let (base args) (tokens url #\?)
      (list (sym (downcase type))
            (sym (cut base 1))
            (if args
                (parseargs args)
                nil)))))

; I don't urldecode field names or anything in cookies; correct?

(def parseargs (s)
  (map (fn ((k v)) (list k (urldecode v)))
       (map [tokens _ #\=] (tokens s #\&))))

(def parsecookies (s)
  (map [tokens _ #\=]
       (cdr (tokens s [or (whitec _) (is _ #\;)]))))

(def arg (req key) (alref req!args string.key))

; *** Warning: does not currently urlencode args, so if need to do
; that replace v with (urlencode v).

(def reassemble-args (req)
  (aif req!args
       (apply string "?" (intersperse '&
                                      (map (fn ((k v))
                                             (string k '= v))
                                           it)))
       ""))

(= fns* (table) fnids* nil timed-fnids* nil)

; count on huge (expt 64 10) size of fnid space to avoid clashes

(def new-fnid ()
  (check (sym (rand-string 10)) ~fns* (new-fnid)))

(def fnid (f)
  (atlet key (new-fnid)
    (= (fns* key) f)
    (push key fnids*)
    key))

(def timed-fnid (lasts f)
  (atlet key (new-fnid)
    (= (fns* key) f)
    (push (list key (seconds) lasts) timed-fnids*)
    key))

; Within f, it will be bound to the fn's own fnid.  Remember that this is
; so low-level that need to generate the newline to separate from the headers
; within the body of f.

(mac afnid (f)
  `(atlet it (new-fnid)
     (= (fns* it) ,f)
     (push it fnids*)
     it))

;(defop test-afnid req
;  (tag (a href (url-for (afnid (fn (req) (prn) (pr "my fnid is " it)))))
;    (pr "click here")))

; To be more sophisticated, instead of killing fnids, could first
; replace them with fns that tell the server it's harvesting too
; aggressively if they start to get called.  But the right thing to
; do is estimate what the max no of fnids can be and set the harvest
; limit there-- beyond that the only solution is to buy more memory.

(def harvest-fnids (? n 50000)  ; was 20000
  (when (len> fns* n)
    (pull (fn ((id created lasts))
            (when (> (since created) lasts)
              (wipe (fns* id))
              t))
          timed-fnids*)
    (atlet nharvest (trunc (/ n 10))
      (let (kill keep) (split (rev fnids*) nharvest)
        (= fnids* (rev keep))
        (each id kill
          (wipe (fns* id)))))))

(= fnurl* "/x" rfnurl* "/r" rfnurl2* "/y" jfnurl* "/a")

(= dead-msg* "\nUnknown or expired link.")

(defop-raw x (str req)
  (w/stdout str
    (aif (fns* (sym (arg req "fnid")))
         (it req)
         (pr dead-msg*))))

(defopr-raw y (str req)
  (aif (fns* (sym (arg req "fnid")))
       (w/stdout str (it req))
       "deadlink"))

; For asynchronous calls; discards the page.  Would be better to tell
; the fn not to generate it.

(defop-raw a (str req)
  (aif (fns* (sym (arg req "fnid")))
       (tostring (it req))))

(defopr r req
  (aif (fns* (sym (arg req "fnid")))
       (it req)
       "deadlink"))

(defop deadlink req
  (pr dead-msg*))

(def url-for (fnid)
  (string fnurl* "?fnid=" fnid))

(def flink (f)
  (string fnurl* "?fnid=" (fnid (fn (req) (prn) (f req)))))

(def rflink (f)
  (string rfnurl* "?fnid=" (fnid f)))

; Since it's just an expr, gensym a parm for (ignored) args.

(mac w/link (expr . body)
  `(tag (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(mac w/rlink (expr . body)
  `(tag (a href (rflink (fn (,(uniq)) ,expr)))
     ,@body))

(mac onlink (text . body)
  `(w/link (do ,@body) (pr ,text)))

(mac onrlink (text . body)
  `(w/rlink (do ,@body) (pr ,text)))

; bad to have both flink and linkf; rename flink something like fnid-link

(mac linkf (text parms . body)
  `(tag (a href (flink (fn ,parms ,@body))) (pr ,text)))

(mac rlinkf (text parms . body)
  `(tag (a href (rflink (fn ,parms ,@body))) (pr ,text)))

;(defop top req (linkf 'whoami? (req) (pr "I am " (get-user req))))

;(defop testf req (w/link (pr "ha ha ha") (pr "laugh")))

(mac w/link-if (test expr . body)
  `(tag-if ,test (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(def fnid-field (id)
  (gentag input type 'hidden name 'fnid value id))

; f should be a fn of one arg, which will be http request args.

(def fnform (f bodyfn ? redir nil)
  (tag (form method 'post action (if redir rfnurl2* fnurl*))
    (fnid-field (fnid f))
    (bodyfn)))

; Could also make a version that uses just an expr, and var capture.
; Is there a way to ensure user doesn't use "fnid" as a key?

(mac aform (f . body)
  (w/uniq ga
    `(tag (form method 'post action fnurl*)
       (fnid-field (fnid (fn (,ga)
                           (prn)
                           (,f ,ga))))
       ,@body)))

;(defop test1 req
;  (fnform (fn (req) (prn) (pr req))
;          (fn () (single-input "" 'foo 20 "submit"))))

;(defop test2 req
;  (aform (fn (req) (pr req))
;    (single-input "" 'foo 20 "submit")))

; Like aform except creates a fnid that will last for lasts seconds
; (unless the server is restarted).

(mac taform (lasts f . body)
  (w/uniq (gl gf gi ga)
    `(withs (,gl ,lasts
             ,gf (fn (,ga) (prn) (,f ,ga)))
       (tag (form method 'post action fnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf) (fnid ,gf)))
         ,@body))))

(mac arform (f . body)
  `(tag (form method 'post action rfnurl*)
     (fnid-field (fnid ,f))
     ,@body))

; overlong

(mac tarform (lasts f . body)
  (w/uniq (gl gf)
    `(withs (,gl ,lasts ,gf ,f)
       (tag (form method 'post action rfnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf) (fnid ,gf)))
         ,@body))))

(mac aformh (f . body)
  `(tag (form method 'post action fnurl*)
     (fnid-field (fnid ,f))
     ,@body))

(mac arformh (f . body)
  `(tag (form method 'post action rfnurl2*)
     (fnid-field (fnid ,f))
     ,@body))

; only unique per server invocation

(= unique-ids* (table))

(def unique-id (? len 8)
  (let id (sym (rand-string (max 5 len)))
    (if (unique-ids* id)
        (unique-id)
        (= (unique-ids* id) id))))

(def srvlog (type . args)
  (w/appendfile o (logfile-name type)
    (w/stdout o (atomic (write (cons (seconds) args)) (prn)))))

(def logfile-name (type)
  (string logdir* type "-" (memodate)))

(with (lastasked nil lastval nil)

(def memodate ()
  (let now (seconds)
    (if (or (no lastasked) (> (- now lastasked) 60))
        (= lastasked now lastval (datestring))
        lastval)))

)

(defop || req (pr "It's alive."))

(defop topips req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (each ip (let leaders nil
                   (maptable (fn (ip n)
                               (when (> n 100)
                                 (insort (compare > requests/ip*)
                                         ip
                                         leaders)))
                             requests/ip*)
                   leaders)
          (let n (requests/ip* ip)
            (row ip n (pr (num (* 100 (/ n requests*)) 1)))))))))

(defop spurned req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (map (fn ((ip n)) (row ip n))
             (sortable spurned*))))))

; eventually promote to general util

(def sortable (ht ? f >)
  (let res nil
    (maptable (fn kv
                (insort (compare f cadr) kv res))
              ht)
    res))


; Background Threads

(= bgthreads* (table) pending-bgthreads* nil)

(def new-bgthread (id f sec)
  (aif (bgthreads* id) (break-thread it))
  (= (bgthreads* id) (new-thread (fn ()
                                   (while t
                                     (sleep sec)
                                     (f))))))

; should be a macro for this?

(mac defbg (id sec . body)
  `(do (pull [caris _ ',id] pending-bgthreads*)
       (push (list ',id (fn () ,@body) ,sec)
             pending-bgthreads*)))



; Idea: make form fields that know their value type because of
; gensymed names, and so the receiving fn gets args that are not
; strings but parsed values.

