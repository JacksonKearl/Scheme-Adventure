;;;; An adventure game at MIT

(define the-clock)
(define all-places)
(define heaven)
(define all-people)
(define my-avatar)

(define (start-adventure my-name)
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
        (create-avatar my-name (car all-places)))
  (whats-here))

(define (get-all-places)
  all-places)

(define (get-heaven)
  heaven)

(define (get-clock)
  the-clock)

;;; User interface

(define (go direction)
  (let ((exit
         (find-exit-in-direction direction
                                 (get-location my-avatar))))
    (if exit
        (take-exit! exit my-avatar)
        (narrate! (list "No exit in" direction "direction")
                  my-avatar)))
  unspecific)

(define (take-thing name)
  (let ((thing (find-thing name (here))))
    (if thing
        (take-thing! thing my-avatar)))
  unspecific)

(define (drop-thing name)
  (let ((thing (find-thing name my-avatar)))
    (if thing
        (drop-thing! thing my-avatar)))
  unspecific)

(define (look-in-bag #!optional person-name)
  (let ((person
         (if (default-object? person-name)
             my-avatar
             (find-person person-name))))
    (if person
        (tell! (let ((referent (local-possessive person))
                     (things (get-things person)))
                 (if (n:pair? things)
                     (cons* referent "bag contains" things)
                     (list referent "bag is empty")))
               my-avatar)))
  unspecific)

(define (whats-here)
  (look-around my-avatar)
  unspecific)

(define (say . message)
  (say! my-avatar message)
  unspecific)

(define (message from to . message)
  (send-sms! from to (car (get-networks (find-object-by-name from (get-things my-avatar)))) message)
  (tell! `("I just sent a message to" ,(find-object-by-name to (get-endpoints! (car (get-networks (find-object-by-name from (get-things my-avatar))))))) my-avatar)
  unspecific)

(define (tell person-name . message)
  (tell! message (find-person person-name))
  unspecific)

(define (hang-out ticks)
  (do ((i 0 (n:+ i 1)))
      ((not (n:< i ticks)))
      (heal! (weighted-random '(2 7 3 2 1 1)) my-avatar)
      (tick! (get-clock))
      (announce! '("A day passes...")))
  (say! my-avatar '("That was relaxing."))
  unspecific)

;;; Support for UI

(define (here)
  (get-location my-avatar))

(define (find-person name)
  (let ((person
         (find-object-by-name name (visible-people-here my-avatar))))
    (if (not person)
        (tell! (list "There is no one called" name "here")
               my-avatar))
    person))

(define (find-thing name person-or-place)
  (let ((thing
         (find-object-by-name
          name
          (person-or-place-things person-or-place))))
    (if (not thing)
        (tell! (cons* "There is nothing called"
                      name
                      (person-or-place-name person-or-place))
               my-avatar))
    thing))

(define (person-or-place-things person-or-place)
  (if (place? person-or-place)
      (all-things-in-place person-or-place)
      (get-things person-or-place)))

(define (person-or-place-name person-or-place)
  (if (place? person-or-place)
      '("here")
      (list "in" (local-possessive person-or-place) "bag")))

(define (local-possessive person)
  (if (eqv? person my-avatar)
      "Your"
      (possessive person)))

(define (send-sms! sender-endpoint receiver-endpoint network message)
  (send-message! `("Recieved a"
                   ,network
                   "message from"
                   ,(find-object-by-name sender-endpoint (get-endpoints! network))
                   ":"
                   ,@(filter (lambda x #t) message))
                 (find-object-by-name receiver-endpoint (get-endpoints! network))))

(define (create-mit)
  (let ((athena (create-p2p-network 'athena))     ; person-to-person location-centric
        (verizon (create-p2p-network 'verizon)))  ; person-to-person object-centric
    (let ((32G (create-networked-place 'gates-tower athena))
          (32D (create-networked-place 'dreyfoos-tower athena))
          (barker-library (create-networked-place 'barker-library athena))
          (cp32 (create-networked-place 'bldg-32-cp-hq athena))
          (infinite (create-networked-place 'infinite-corridor athena))

          (great-dome (create-place 'great-dome))
          (little-dome (create-place 'little-dome))
          (lobby-10 (create-place 'lobby-10))
          (10-250 (create-place '10-250))
          (lobby-7 (create-place 'lobby-7))

          (bldg-26 (create-place 'bldg-26))
          (tunnel (create-place 'lab-supplies))

          (32-123 (create-place '32-123))
          (student-street (create-place 'student-street))
          (great-court (create-place 'great-court))
          (bldg-54 (create-place 'green-building))
          (the-dot (create-place 'the-dot))
          (dorm-row (create-place 'dorm-row))
          (E25 (create-place 'mit-medical)))



      (can-go-both-ways lobby-10 'up 'down 10-250)
      (can-go-both-ways 10-250 'up 'down barker-library)
      (can-go-both-ways barker-library 'up 'down great-dome)
      (can-go-both-ways lobby-10 'west 'east lobby-7)
      (can-go-both-ways lobby-7 'west 'east dorm-row)
      (can-go-both-ways lobby-7 'up 'down little-dome)
      (can-go-both-ways lobby-10 'south 'north great-court)
      (can-go-both-ways lobby-10 'east 'west infinite)
      (can-go-both-ways infinite 'north 'south bldg-26)
      (can-go-both-ways infinite 'east 'west bldg-54)
      (can-go-both-ways bldg-26 'east 'west student-street)
      (can-go-both-ways student-street 'down 'up cp32)
      (can-go-both-ways cp32 'south 'north tunnel)
      (can-go-both-ways tunnel 'up 'down bldg-54)
      (can-go-both-ways bldg-54 'south 'north the-dot)
      (can-go-both-ways the-dot 'west 'east great-court)
      (can-go-both-ways student-street 'in 'out 32-123)
      (can-go-both-ways student-street 'up 'down 32G)
      (can-go-both-ways student-street 'skew 'down 32D)
      (can-go-both-ways 32G 'east 'west E25)
      (can-go-both-ways bldg-54 'skew 'skew E25)

      ; Add line-of-sight into the mix
      (can-see bldg-54 32G)
      (can-see bldg-54 32D)
      (can-see bldg-54 great-dome)
      (can-see bldg-54 little-dome)
      (can-see bldg-54 great-court)
      (can-see bldg-54 the-dot)
      (can-see bldg-54 E25)
      (can-see lobby-10 great-court)
      (can-see great-dome great-court)
      (can-see-both-ways 32D 32G)
      (can-see-both-ways great-dome little-dome)
      (can-see-both-ways lobby-10 infinite)
      (can-see-both-ways infinite bldg-26)
      (can-see-both-ways lobby-10 lobby-7)

      ; networking objects
      (create-networked-mobile-thing 'iphone6 verizon 32-123)
      (create-networked-mobile-thing 'iphone7 verizon 10-250)
      (create-networked-mobile-thing 'iphone8 verizon student-street)
      (create-networked-mobile-thing 'andriod verizon dorm-row)
      (create-networked-mobile-thing 'nokia   verizon barker-library)
      (create-networked-mobile-thing 'nokia2   verizon barker-library)

      ; Create some things
      (create-thing 'blackboard 10-250)
      (create-thing 'lovely-trees great-court)
      (create-thing 'flag-pole great-court)
      (create-thing 'calder-sculpture the-dot)
      (create-mobile-thing 'problem-set 32-123)
      (create-mobile-thing 'recitation-problem 32-123)
      (create-mobile-thing 'sicp student-street)
      (create-mobile-thing 'engineering-book barker-library)

      (create-mobile-thing 'cloak-of-invisibility barker-library)

      (list barker-library great-dome little-dome lobby-10
            10-250 lobby-7
            infinite bldg-26 cp32
            tunnel 32-123 32D 32G
            student-street bldg-54 the-dot
            dorm-row E25))))
  
(define (create-people places)
  (append (create-students places)
          (create-house-masters places)
          (create-trolls places)))

(define (create-students places)
  (map (lambda (name)
         (create-student name
                         (random-choice places)
                         (random-bias 5)
                         (random-bias 5)))
       '(ben-bitdiddle alyssa-hacker course-6-frosh lambda-man)))

(define (create-house-masters places)
  (map (lambda (name)
         (create-house-master name
                              (random-choice places)
                              (random-bias 3)
                              (random-bias 3)))
       '(dr-evil mr-bigglesworth)))

(define (create-trolls places)
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       (random-bias 3)))
       '(grendel registrar)))

(define (create-thing name location)
  (make-thing 'name name
              'location location))

(define (create-networked-thing name location network)
  (add-endpoint!  (make-thing 'name name
                              'network network
                              'location location) network))

(define (create-mobile-thing name location)
  (make-mobile-thing 'name name
                     'location location))

(define (create-networked-mobile-thing name network location)
  (let ((thing (make-mobile-thing 'name name
                                  'networks (list network)
                                  'location location)))
      (add-endpoint! network thing)
      thing))

(define (create-place name)
  (make-place 'name name))

(define (create-networked-place name network)
  (let ((place (make-place 'name name
                          'networks (list network))))
      (add-endpoint! network place)
      place))

;(define (create-network name)
;  (make-network 'name name))

(define (create-p2p-network name)
  (make-p2p-network 'name name))

(define (create-exit from direction to)
  (make-exit 'name 'exit
             'from from
             'direction direction
             'to to))

(define (create-student name home restlessness acquisitiveness)
  (make-student 'name name
                'location home
                'restlessness restlessness
                'acquisitiveness acquisitiveness))

(define (create-house-master name home restlessness irritability)
  (make-house-master 'name name
                     'location home
                     'restlessness restlessness
                     'acquisitiveness 1/10
                     'irritability irritability))

(define (create-troll name home restlessness hunger)
  (make-troll 'name name
              'location home
              'restlessness restlessness
              'acquisitiveness 1/10
              'hunger hunger))

(define (create-avatar name home)
  (make-avatar 'name name
               'location home
               'screen (make-screen 'name 'the-screen)))

(define (can-go-both-ways from direction reverse-direction to)
  (create-exit from direction to)
  (create-exit to reverse-direction from))

(define (can-see a b)
  (add-vista! a b))

(define (can-see-both-ways a b)
  (can-see a b)
  (can-see b a))
