;;; Object types for Adventure game

(define thing:location
  (make-property 'location
                 'predicate (lambda (x) (container? x))))

(define thing:networks
  (make-property 'networks
                 'predicate (lambda (x) (is-list-of networks x))
                 'default-value '()))

(define thing?
  (make-type 'thing (list thing:location thing:networks)))

(set-predicate<=! thing? object?)

(define make-thing
  (type-instantiator thing?))

(define get-location
  (property-getter thing:location thing?))

(define get-networks
  (property-getter thing:networks thing?))

(define-generic-procedure-handler set-up! (match-args thing?)
  (lambda (super thing)
    (super thing)
    (add-thing! (get-location thing) thing)))

(define-generic-procedure-handler tear-down! (match-args thing?)
  (lambda (super thing)
    (remove-thing! (get-location thing) thing)
    (super thing)))

(define-generic-procedure-handler send-message!
  (match-args message? thing?)
  (lambda (message thing)
    unspecific))

;;; Containers

(define container:things
  (make-property 'things
                 'predicate (is-list-of thing?)
                 'default-value '()))

(define container?
  (make-type 'container (list container:things)))
(set-predicate<=! container? object?)

(define get-things
  (property-getter container:things container?))

(define add-thing!
  (property-adder container:things container? thing?))

(define remove-thing!
  (property-remover container:things container? thing?))

;;; Exits

(define exit:from
  (make-property 'from
                 'predicate (lambda (x) (place? x))))

(define exit:to
  (make-property 'to
                 'predicate (lambda (x) (place? x))))

(define exit:direction
  (make-property 'direction
                 'predicate direction?))

(define exit?
  (make-type 'exit (list exit:from exit:to exit:direction)))
(set-predicate<=! exit? object?)

(define make-exit
  (type-instantiator exit?))

(define get-from
  (property-getter exit:from exit?))

(define get-to
  (property-getter exit:to exit?))

(define get-direction
  (property-getter exit:direction exit?))

(define-generic-procedure-handler set-up! (match-args exit?)
  (lambda (super exit)
    (super exit)
    (add-exit! (get-from exit) exit)))

;;; Places

(define place:vistas
  (make-property 'vistas
                 'predicate (lambda (x)
                              (and (n:list? x) (every place x)))
                 'default-value '()))

(define place:exits
  (make-property 'exits
                 'predicate (lambda (x)
                              (and (n:list? x) (every place x)))
                 'default-value '()))

(define place:networks
 (make-property 'networks
                'predicate (lambda (x) (is-list-of networks x))
                'default-value '()))

(define get-place-networks
  (property-getter place:networks thing?))


(define place?
  (make-type 'place (list place:vistas place:exits place:networks)))
(set-predicate<=! place? container?)

(define make-place
  (type-instantiator place?))

(define get-vistas
  (property-getter place:vistas place?))

(define add-vista!
  (property-adder place:vistas place? place?))

(define get-exits
  (property-getter place:exits place?))

(define add-exit!
  (property-adder place:exits place? exit?))

(define (find-exit-in-direction direction place)
  (find (lambda (exit)
          (eqv? (get-direction exit) direction))
        (get-exits place)))

(define (people-in-place place)
  (filter person? (get-things place)))

(define (visible-people-in-place place)
  (filter get-visibility (filter person? (get-things place))))

(define (things-in-place place)
  (remove person? (get-things place)))

(define (visible-things-in-place place)
  (if (pair? (filter (lambda (x) (not (get-visibility x))) (filter person? (get-things place))))
    (filter
      (lambda (x) (or (not thing?) (not (equal? (get-name x) 'cloak-of-invisibility))) )
      (remove person? (get-things place)))
    (remove person? (get-things place))))


(define (all-things-in-place place)
  (append (things-in-place place)
          (append-map get-things (people-in-place place))))

(define (takeable-things place)
  (append (filter mobile-thing? (things-in-place place))
          (append-map get-things (people-in-place place))))

;(define (announce-to-networks message place)
;  (for-each (lambda (network)
;              (send-message! `(,place
;                                    "sends a message over the"
;                                    ,(get-name network)
;                                    ":"
;                                    ,@(filter (lambda x #t) message))
;                              network))
;            (get-networks place)))

(define-generic-procedure-handler send-message!
  (match-args message? place?)
  (lambda (message place)
    (for-each (lambda (person)
                (send-message! message person))
              (people-in-place place))))

;;; Broadcast Networks
;
;(define network:endpoints
;  (make-property 'endpoints
;                 'predicate (lambda (x)
;                              (and (n:list? x) (every network x)))
;                 'default-value '()))
;
;(define network?
;  (make-type 'network (list network:endpoints)))
;(set-predicate<=! network? container?)
;
;(define make-network
;  (type-instantiator network?))
;
;(define get-endpoints
;  (property-getter network:endpoints network?))
;
;(define add-endpoint!
;  (property-adder network:endpoints network? thing?))
;
;(define add-network!
;  (property-adder thing:networks thing? network?))
;
;(define add-network!
;  (property-adder place:networks place? network?))
;
;(define-generic-procedure-handler send-message!
;  (match-args message? network?)
;  (lambda (message network)
;    (for-each (lambda (endpoint)
;                (if (not (equal? (get-name endpoint) (get-name (car message))))
;                  (send-message! message endpoint)))
;              (get-endpoints network))))
;
;

;;; p2p Networks

(define p2p-network:endpoints
  (make-property 'endpoints
                 'predicate (lambda (x)
                              (and (n:list? x) (every p2p-network x)))
                 'default-value '()))

(define p2p-network?
  (make-type 'p2p-network (list p2p-network:endpoints)))
(set-predicate<=! p2p-network? container?)

(define make-p2p-network
  (type-instantiator p2p-network?))

(define get-endpoints!
  (property-getter p2p-network:endpoints p2p-network?))

(define add-endpoint!
  (property-adder p2p-network:endpoints p2p-network? object?))

(define add-network
  (property-adder thing:networks thing? p2p-network?))

(define add-network!
  (property-adder place:networks place? p2p-network?))

;;; Mobile things

(define mobile-thing:origin
  (make-property 'origin
                 'predicate place?
                 'default-to-property thing:location))

(define mobile-thing?
  (make-type 'mobile-thing (list mobile-thing:origin)))
(set-predicate<=! mobile-thing? thing?)

(define make-mobile-thing
  (type-instantiator mobile-thing?))

(define set-location!
  (property-setter thing:location mobile-thing? container?))

(define get-origin
  (property-getter mobile-thing:origin mobile-thing?))

(define enter-place!
  (chaining-generic-procedure 'enter-place! 1))

(define-generic-procedure-default-handler enter-place!
  (lambda (mobile-thing) #f))

(define leave-place!
  (std-generic-procedure 'leave-place! 1))

(define-generic-procedure-default-handler leave-place!
  (lambda (mobile-thing) #f))

(define-generic-procedure-handler send-message!
  (match-args message? mobile-thing?)
  (lambda (message receiver)
    (if (bag? (get-location receiver))
      (send-message! message (get-holder (get-location receiver))))))

;;; People

(define person:visible
  (make-property 'visible
                 'predicate n:boolean?
                 'default-value #t))

(define person:health
  (make-property 'health
                 'predicate n:exact-integer?
                 'default-supplier (lambda () (+ 7 (weighted-random '(1 2 3 4 3 2 1))))))

(define person:bag
  (make-property 'bag
                 'predicate (lambda (x) (bag? x))
                 'default-supplier (lambda () (make-bag 'name 'my-bag))))

(define person?
  (make-type 'person (list person:health person:bag person:visible)))
(set-predicate<=! person? mobile-thing?)

(define get-health
  (property-getter person:health person?))

(define set-health!
  (property-setter person:health person? any-object?))

(define get-visibility
  (property-getter person:visible person?))

(define set-visibility!
  (property-setter person:visible person? any-object?))

(define get-bag
  (property-getter person:bag person?))

(define-generic-procedure-handler set-up! (match-args person?)
  (lambda (super person)
    (super person)
    (set-holder! (get-bag person) person)))

(define-generic-procedure-handler get-things (match-args person?)
  (lambda (person)
    (get-things (get-bag person))))

(define-generic-procedure-handler enter-place!
  (match-args person?)
  (lambda (super person)
    (super person)
    (walk-it-off! person)
    (if (get-visibility person)
      (narrate! (list person "enters" (get-location person))
                person))

    (if (equal? 'mit-medical (get-name (get-location person)))
      (heal! (+ 5 (weighted-random '(1 2 3 2 1))) person))
    (let ((people (visible-people-here person)))
      (if (and (get-visibility person) (n:pair? people))
          (say! person (cons "Hi" people))))))

(define (when-alive callback)
  (lambda (person)
    (if (n:> (get-health person) 0)
        (callback person))))

(define (walk-it-off! person)
  (if (get-visibility person)
    (heal! (weighted-random '(6 2 1)) person)
    (begin (say! person '("Ouch, I'm so invisible")) (suffer! (weighted-random '(0 2 3 5 7)) person))))

(define (people-here person)
  (delv person (people-in-place (get-location person))))

(define (visible-people-here person)
  (delv person (visible-people-in-place (get-location person))))

(define (things-here person)
  (things-in-place (get-location person)))

(define (visible-things-here person)
  (visible-things-in-place (get-location person)))

(define (vistas-here person)
  (get-vistas (get-location person)))

(define (exits-here person)
  (get-exits (get-location person)))

;(define (networks-here person)
;  (get-networks (get-location person)))

(define (peoples-things person)
  (append-map get-things (visible-people-here person)))

(define (heal! points person)
  (guarantee n:exact-nonnegative-integer? points)
  (if (> points 4)
    (say! person (list "I feel exceptionally well-rested!")))

  (if (> (+ (get-health person) points) 15)
    (set-health! person 15)
    (set-health! person (+ (get-health person) points))))

(define (suffer! hits person)
  (guarantee n:exact-positive-integer? hits)
  (say! person (list "Ouch!" hits "hits is more than I want!"))
  (set-health! person (- (get-health person) hits))
  (if (< (get-health person) 1)
      (die! person)))

(define (die! person)
  (for-each (lambda (thing)
              (drop-thing! thing person))
            (get-things person))
  (announce!
   '("An earth-shattering, soul-piercing scream is heard..."))
  (set-health! person 0)
  (move! person (get-heaven) person))

(define (resurrect! person health)
  (guarantee n:exact-positive-integer? health)
  (set-health! person health)
  (move! person (get-origin person) person))

;;; Bags

(define bag:holder
  (make-property 'holder
                 'predicate
                 (lambda (x) (or (not x) (person? x)))
                 'default-value #f))

(define bag?
  (make-type 'bag (list bag:holder)))
(set-predicate<=! bag? container?)

(define make-bag
  (type-instantiator bag?))

(define get-holder
  (property-getter bag:holder bag?))

(define set-holder!
  (property-setter bag:holder bag? person?))

;;; Autonomous people (non-player characters)

(define autonomous-agent:restlessness
  (make-property 'restlessness
                 'predicate bias?))

(define autonomous-agent:acquisitiveness
  (make-property 'acquisitiveness
                 'predicate bias?))

(define autonomous-agent?
  (make-type 'autonomous-agent
             (list autonomous-agent:restlessness
                   autonomous-agent:acquisitiveness)))
(set-predicate<=! autonomous-agent? person?)

(define get-restlessness
  (property-getter autonomous-agent:restlessness
                   autonomous-agent?))

(define get-acquisitiveness
  (property-getter autonomous-agent:acquisitiveness
                   autonomous-agent?))

(define-generic-procedure-handler set-up!
  (match-args autonomous-agent?)
  (lambda (super agent)
    (super agent)
    (register-with-clock! agent (get-clock))))

(define-generic-procedure-handler tear-down!
  (match-args autonomous-agent?)
  (lambda (super agent)
    (unregister-with-clock! agent (get-clock))
    (super agent)))

(define (move-and-take-stuff! agent)
  (if (flip-coin (get-restlessness agent))
      (move-somewhere! agent))
  (if (flip-coin (get-acquisitiveness agent))
      (take-something! agent)))

(define (move-somewhere! agent)
  (let ((exit (random-choice (exits-here agent))))
    (if exit
        (take-exit! exit agent))))

(define (take-something! agent)
  (let ((thing
         (random-choice (append (visible-things-here agent)
                                (peoples-things agent)))))
    (if thing
        (take-thing! thing agent))))

(define-clock-handler autonomous-agent? move-and-take-stuff!)

;(define-generic-procedure-handler send-message!
;  (match-args message? person?)
;  (lambda (message person)
;    (shout! person (list "I just" message))
;    ))

;;; Students

(define student?
  (make-type 'student '()))
(set-predicate<=! student? autonomous-agent?)

(define make-student
  (type-instantiator student?))

;;; House masters

(define house-master:irritability
  (make-property 'irritability
                 'predicate bias?))

(define house-master?
  (make-type 'house-master (list house-master:irritability)))
(set-predicate<=! house-master? autonomous-agent?)

(define make-house-master
  (type-instantiator house-master?))

(define get-irritability
  (property-getter house-master:irritability house-master?))

(define (irritate-students! master)
  (let ((students (filter student? (visible-people-here master))))
    (if (flip-coin (get-irritability master))
        (if (n:pair? students)
            (begin
              (say! master
                    '("What are you doing still up?"
                      "Everyone back to their rooms!"))
              (for-each (lambda (student)
                          (narrate! (list student "goes home to"
                                          (get-origin student))
                                    student)
                          (move! student
                                 (get-origin student)
                                 student))
                        students))
            (say! master
                  '("Grrr... When I catch those students...")))
        (if (n:pair? students)
            (say! master
                  '("I'll let you off this once..."))))))

(define-clock-handler house-master? irritate-students!)

;;; Trolls

(define troll:hunger
  (make-property 'hunger
                 'predicate bias?))

(define troll?
  (make-type 'troll (list troll:hunger)))
(set-predicate<=! troll? autonomous-agent?)

(define make-troll
  (type-instantiator troll?))

(define get-hunger
  (property-getter troll:hunger troll?))

(define (eat-people! troll)
  (if (flip-coin (get-hunger troll))
      (let ((people (visible-people-here troll)))
        (if (n:pair? people)
            (let ((victim (random-choice people))
                  (bite   (weighted-random '(0 0 1 1 2 2 4 4 6 6 8 8 6 6))))
              (case bite
                ((2 3 4 5) (narrate! (list troll "takes an itty-bitty biterino out of" victim) troll))
                ((6 7 8 9) (narrate! (list troll "takes a concerningly consumptuous bite out of" victim) troll))
                (else      (narrate! (list troll "takes a significantly sizeable bite out of" victim) troll)))
              (suffer! bite victim))
            (narrate! (list (possessive troll) "belly rumbles")
                      troll)))))

(define-clock-handler troll? eat-people!)

;;; Avatars

(define avatar:screen
  (make-property 'screen
                 'predicate screen?))

(define avatar?
  (make-type 'avatar (list avatar:screen)))
(set-predicate<=! avatar? person?)

(define make-avatar
  (type-instantiator avatar?))

(define get-screen
  (property-getter avatar:screen avatar?))

(define-generic-procedure-handler send-message!
  (match-args message? avatar?)
  (lambda (message avatar)
    (send-message! message (get-screen avatar))))

(define-generic-procedure-handler enter-place!
  (match-args avatar?)
  (lambda (super avatar)
    (super avatar)
    (look-around avatar)
    (tick! (get-clock))))

(define (look-around avatar)
  (tell! (list "You are in" (get-location avatar))
         avatar)
  (tell! (list "You have" (get-health avatar) "health")
        avatar)
  (let ((my-things (get-things avatar)))
    (if (n:pair? my-things)
        (tell! (cons "Your bag contains:" my-things)
               avatar)))
  (let ((things
         (append (visible-things-here avatar)
                 (visible-people-here avatar))))
    (if (n:pair? things)
        (tell! (cons "You see here:" things)
               avatar)))
  (let ((vistas (vistas-here avatar)))
    (if (n:pair? vistas)
        (tell! (cons "You can see:" vistas)
               avatar)))
  (tell! (let ((exits (exits-here avatar)))
           (if (n:pair? exits)
               (cons "You can exit:"
                     (map get-direction exits))
               '("There are no exits..."
                 "you are dead and gone to heaven!")))
         avatar))

;;; Motion

(define (take-thing! thing person)
  (move! thing (get-bag person) person))

(define (drop-thing! thing person)
  (move! thing (get-location person) person))

(define (take-exit! exit mobile-thing)
  (generic-move! mobile-thing
                 (get-from exit)
                 (get-to exit)
                 mobile-thing))

(define (move! thing location actor)
  (generic-move! thing
                 (get-location thing)
                 location
                 actor))

(define generic-move!
  (std-generic-procedure 'generic-move! 4))

;;; TODO: guarantee that THING is in FROM.
;;; Also that the people involved are local.

;; coderef: generic-move:default
(define-generic-procedure-handler generic-move!
  (match-args thing? container? container? person?)
  (lambda (thing from to actor)
    (tell! (list thing "is not movable")
           actor)))

;; coderef: generic-move:steal
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from))
          (new-holder (get-holder to)))
      (cond ((eqv? from to)
             (tell! (list new-holder "is already carrying"
                          mobile-thing)
                    actor))
            ((eqv? actor former-holder)
             (narrate! (list actor
                             "gives" mobile-thing
                             "to" new-holder)
                       actor))
            ((eqv? actor new-holder)
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder)
                       actor))
            (else
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder
                             "and gives it to" new-holder)
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder (list "Yaaaah! I am upset!")))
      (if (not (eqv? actor new-holder))
          (say! new-holder (list "Whoa! Where'd you get this?")))
      (if (not (eqv? from to))
          (move-internal! mobile-thing from to)))))

;; coderef: generic-move:take
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? place? bag? person?)
  (lambda (mobile-thing from to actor)
    (let ((new-holder (get-holder to)))
      (cond ((eqv? actor new-holder)
             (narrate! (list actor
                             "picks up" mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             "picks up" mobile-thing
                             "and gives it to" new-holder)
                       actor)))
      (if (not (eqv? actor new-holder))
          (say! new-holder (list "Whoa! Thanks, dude!")))
      (move-internal! mobile-thing from to))))

;; coderef: generic-move:drop
(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? bag? place? person?)
  (lambda (mobile-thing from to actor)
    (let ((former-holder (get-holder from)))
      (cond ((eqv? actor former-holder)
             (narrate! (list actor
                             "drops" mobile-thing)
                       actor))
            (else
             (narrate! (list actor
                             "takes" mobile-thing
                             "from" former-holder
                             "and drops it")
                       actor)))
      (if (not (eqv? actor former-holder))
          (say! former-holder
                (list "What did you do that for?")))
      (move-internal! mobile-thing from to))))

(define-generic-procedure-handler generic-move!
  (match-args mobile-thing? place? place? person?)
  (lambda (mobile-thing from to actor)
    (cond ((eqv? from to)
           (tell! (list mobile-thing "is already in" from)
                  actor))
          (else
           (tell! (list "How do you propose to move"
                        mobile-thing
                        "without carrying it?")
                  actor)))))

;; coderef: generic-move:person
(define-generic-procedure-handler generic-move!
  (match-args person? place? place? person?)
  (lambda (person from to actor)
    (let ((exit (find-exit from to)))
      (cond ((or (eqv? from (get-heaven))
                 (eqv? to (get-heaven)))
             (move-internal! person from to))
            ((not exit)
             (tell! (list "There is no exit from" from
                          "to" to)
                    actor))
            ((eqv? person actor)
             (if (get-visibility person)
             (narrate! (list person "leaves via the"
                             (get-direction exit) "exit")
                       from))
             (move-internal! person from to))
            (else
             (tell! (list "You can't force"
                          person
                          "to move!")
                    actor))))))

(define (find-exit from to)
  (find (lambda (exit)
          (and (eqv? (get-from exit) from)
               (eqv? (get-to exit) to)))
        (get-exits from)))

(define (manage-visibilty from to)
  (if (bag? from)
    (begin (set-visibility! (get-holder from) #t) (narrate! `(,(get-holder from) "suddenly appears out of nowhere") (get-holder from))))
  (if (bag? to)
    (begin (set-visibility! (get-holder to)   #f) (narrate! `(,(get-holder to)   "suddenly vanishes into thin air") (get-holder to)))))

(define (move-internal! mobile-thing from to)
  (if (equal? (get-name mobile-thing) 'cloak-of-invisibility)
    (manage-visibilty from to))
  (leave-place! mobile-thing)
  (remove-thing! from mobile-thing)
  (set-location! mobile-thing to)
  (add-thing! to mobile-thing)
  (enter-place! mobile-thing))
