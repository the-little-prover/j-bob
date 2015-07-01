
(defun list0 () '())
(defun list0? (x) (equal x '()))

(defun list1 (x) (cons x (list0)))
(defun list1? (x)
  (if (atom x) 'nil (list0? (cdr x))))
(defun elem1 (xs) (car xs))

(defun list2 (x y) (cons x (list1 y)))
(defun list2? (x)
  (if (atom x) 'nil (list1? (cdr x))))
(defun elem2 (xs) (elem1 (cdr xs)))

(defun list3 (x y z) (cons x (list2 y z)))
(defun list3? (x)
  (if (atom x) 'nil (list2? (cdr x))))
(defun elem3 (xs) (elem2 (cdr xs)))

(defun tag (sym x) (cons sym x))
(defun tag? (sym x)
  (if (atom x) 'nil (equal (car x) sym)))
(defun untag (x) (cdr x))

(defun quote-c (value)
  (tag 'quote (list1 value)))
(defun quote? (x)
  (if (tag? 'quote x) (list1? (untag x)) 'nil))
(defun quote.value (e) (elem1 (untag e)))

(defun if-c (Q A E) (tag 'if (list3 Q A E)))
(defun if? (x)
  (if (tag? 'if x) (list3? (untag x)) 'nil))
(defun if.Q (e) (elem1 (untag e)))
(defun if.A (e) (elem2 (untag e)))
(defun if.E (e) (elem3 (untag e)))

(defun app-c (name args) (cons name args))
(defun app? (x)
  (if (atom x)
    'nil
    (if (quote? x)
      'nil
      (if (if? x)
        'nil
        't))))
(defun app.name (e) (car e))
(defun app.args (e) (cdr e))

(defun var? (x)
  (if (equal x 't)
    'nil
    (if (equal x 'nil)
      'nil
      (if (natp x)
        'nil
        (atom x)))))

(defun defun-c (name formals body)
  (tag 'defun (list3 name formals body)))
(defun defun? (x)
  (if (tag? 'defun x) (list3? (untag x)) 'nil))
(defun defun.name (def) (elem1 (untag def)))
(defun defun.formals (def) (elem2 (untag def)))
(defun defun.body (def) (elem3 (untag def)))

(defun dethm-c (name formals body)
  (tag 'dethm (list3 name formals body)))
(defun dethm? (x)
  (if (tag? 'dethm x) (list3? (untag x)) 'nil))
(defun dethm.name (def) (elem1 (untag def)))
(defun dethm.formals (def) (elem2 (untag def)))
(defun dethm.body (def) (elem3 (untag def)))

(defun if-QAE (e)
  (list3 (if.Q e) (if.A e) (if.E e)))
(defun QAE-if (es)
  (if-c (elem1 es) (elem2 es) (elem3 es)))

(defun member? (x ys)
  (if (atom ys)
    'nil
    (if (equal x (car ys))
      't
      (member? x (cdr ys)))))

(defun rator? (name)
  (member? name
    '(equal atom car cdr cons natp size + <)))

(defun rator.formals (rator)
  (if (member? rator '(atom car cdr natp size))
    '(x)
    (if (member? rator '(equal cons + <))
      '(x y)
      'nil)))

(defun def.name (def)
  (if (defun? def)
    (defun.name def)
    (if (dethm? def)
      (dethm.name def)
      def)))

(defun def.formals (def)
  (if (dethm? def)
    (dethm.formals def)
    (if (defun? def)
      (defun.formals def)
      '())))

(defun if-c-when-necessary (Q A E)
  (if (equal A E) A (if-c Q A E)))

(defun conjunction (es)
  (if (atom es)
    (quote-c 't)
    (if (atom (cdr es))
      (car es)
      (if-c (car es)
        (conjunction (cdr es))
        (quote-c 'nil)))))

(defun implication (es e)
  (if (atom es)
    e
    (if-c (car es)
      (implication (cdr es) e)
      (quote-c 't))))

(defun lookup (name defs)
  (if (atom defs)
    name
    (if (equal (def.name (car defs)) name)
      (car defs)
      (lookup name (cdr defs)))))

(defun undefined? (name defs)
  (if (var? name)
    (equal (lookup name defs) name)
    'nil))

(defun arity? (vars es)
  (if (atom vars)
    (atom es)
    (if (atom es)
      'nil
      (arity? (cdr vars) (cdr es)))))

(defun args-arity? (def args)
  (if (dethm? def)
    'nil
    (if (defun? def)
      (arity? (defun.formals def) args)
      (if (rator? def)
        (arity? (rator.formals def) args)
        'nil))))

(defun app-arity? (defs app)
  (args-arity? (lookup (app.name app) defs)
    (app.args app)))

(defun bound? (var vars)
  (if (equal vars 'any) 't (member? var vars)))

(defun exprs? (defs vars es)
  (if (atom es)
    't
    (if (var? (car es))
      (if (bound? (car es) vars)
        (exprs? defs vars (cdr es))
        'nil)
      (if (quote? (car es))
        (exprs? defs vars (cdr es))
        (if (if? (car es))
          (if (exprs? defs vars
                (if-QAE (car es)))
            (exprs? defs vars (cdr es))
            'nil)
          (if (app? (car es))
            (if (app-arity? defs (car es))
              (if (exprs? defs vars
                    (app.args (car es)))
                (exprs? defs vars (cdr es))
                'nil)
              'nil)
            'nil))))))
(defun expr? (defs vars e)
  (exprs? defs vars (list1 e)))

(defun get-arg-from (n args from)
  (if (atom args)
    'nil
    (if (equal n from)
      (car args)
      (get-arg-from n (cdr args) (+ from '1)))))
(defun get-arg (n args)
  (get-arg-from n args '1))

(defun set-arg-from (n args y from)
  (if (atom args)
    '()
    (if (equal n from)
      (cons y (cdr args))
      (cons (car args)
        (set-arg-from n (cdr args) y
          (+ from '1))))))
(defun set-arg (n args y)
  (set-arg-from n args y '1))

(defun <=len-from (n args from)
  (if (atom args)
    'nil
    (if (equal n from)
      't
      (<=len-from n (cdr args) (+ from '1)))))
(defun <=len (n args)
  (if (< '0 n) (<=len-from n args '1) 'nil))

(defun subset? (xs ys)
  (if (atom xs)
    't
    (if (member? (car xs) ys)
      (subset? (cdr xs) ys)
      'nil)))

(defun list-extend (xs x)
  (if (atom xs)
    (list1 x)
    (if (equal (car xs) x)
      xs
      (cons (car xs)
        (list-extend (cdr xs) x)))))

(defun list-union (xs ys)
  (if (atom ys)
    xs
    (list-union (list-extend xs (car ys))
      (cdr ys))))

(defun formals? (vars)
  (if (atom vars)
    't
    (if (var? (car vars))
      (if (member? (car vars) (cdr vars))
        'nil
        (formals? (cdr vars)))
      'nil)))

(defun direction? (dir)
  (if (natp dir)
    't
    (member? dir '(Q A E))))

(defun path? (path)
  (if (atom path)
    't
    (if (direction? (car path))
      (path? (cdr path))
      'nil)))

(defun quoted-exprs? (args)
  (if (atom args)
    't
    (if (quote? (car args))
      (quoted-exprs? (cdr args))
      'nil)))

(defun step-args? (defs def args)
  (if (dethm? def)
    (if (arity? (dethm.formals def) args)
      (exprs? defs 'any args)
      'nil)
    (if (defun? def)
      (if (arity? (defun.formals def) args)
        (exprs? defs 'any args)
        'nil)
      (if (rator? def)
        (if (arity? (rator.formals def) args)
          (quoted-exprs? args)
          'nil)
        'nil))))

(defun step-app? (defs app)
  (step-args? defs
    (lookup (app.name app) defs)
    (app.args app)))

(defun step? (defs step)
  (if (path? (elem1 step))
    (if (app? (elem2 step))
      (step-app? defs (elem2 step))
      'nil)
    'nil))

(defun steps? (defs steps)
  (if (atom steps)
    't
    (if (step? defs (car steps))
      (steps? defs (cdr steps))
      'nil)))

(defun induction-scheme-for? (def vars e)
  (if (defun? def)
    (if (arity? (defun.formals def) (app.args e))
      (if (formals? (app.args e))
        (subset? (app.args e) vars)
        'nil)
      'nil)
    'nil))

(defun induction-scheme? (defs vars e)
  (if (app? e)
    (induction-scheme-for?
      (lookup (app.name e) defs)
      vars
      e)
    'nil))

(defun seed? (defs def seed)
  (if (equal seed 'nil)
    't
    (if (defun? def)
      (expr? defs (defun.formals def) seed)
      (if (dethm? def)
        (induction-scheme? defs
          (dethm.formals def)
          seed)
        'nil))))

(defun extend-rec (defs def)
  (if (defun? def)
    (list-extend defs
      (defun-c
        (defun.name def)
        (defun.formals def)
        (app-c (defun.name def)
          (defun.formals def))))
    defs))

(defun def-contents? (known-defs formals body)
  (if (formals? formals)
    (expr? known-defs formals body)
    'nil))

(defun def? (known-defs def)
  (if (dethm? def)
    (if (undefined? (dethm.name def)
          known-defs)
      (def-contents? known-defs
        (dethm.formals def)
        (dethm.body def))
      'nil)
    (if (defun? def)
      (if (undefined? (defun.name def)
            known-defs)
        (def-contents?
          (extend-rec known-defs def)
          (defun.formals def)
          (defun.body def))
        'nil)
      'nil)))

(defun defs? (known-defs defs)
  (if (atom defs)
    't
    (if (def? known-defs (car defs))
      (defs? (list-extend known-defs (car defs))
        (cdr defs))
      'nil)))

(defun list2-or-more? (pf)
  (if (atom pf)
    'nil
    (if (atom (cdr pf))
      'nil
      't)))

(defun proof? (defs pf)
  (if (list2-or-more? pf)
    (if (def? defs (elem1 pf))
      (if (seed? defs (elem1 pf) (elem2 pf))
        (steps? (extend-rec defs (elem1 pf))
          (cdr (cdr pf)))
        'nil)
      'nil)
    'nil))

(defun proofs? (defs pfs)
  (if (atom pfs)
    't
    (if (proof? defs (car pfs))
      (proofs?
        (list-extend defs (elem1 (car pfs)))
        (cdr pfs))
      'nil)))

(defun sub-var (vars args var)
  (if (atom vars)
    var
    (if (equal (car vars) var)
      (car args)
      (sub-var (cdr vars) (cdr args) var))))

(defun sub-es (vars args es)
  (if (atom es)
    '()
    (if (var? (car es))
      (cons (sub-var vars args (car es))
        (sub-es vars args (cdr es)))
      (if (quote? (car es))
        (cons (car es)
          (sub-es vars args (cdr es)))
        (if (if? (car es))
          (cons
            (QAE-if
              (sub-es vars args
                (if-QAE (car es))))
            (sub-es vars args (cdr es)))
          (cons
            (app-c (app.name (car es))
              (sub-es vars args
                (app.args (car es))))
            (sub-es vars args (cdr es))))))))
(defun sub-e (vars args e)
  (elem1 (sub-es vars args (list1 e))))

(defun exprs-recs (f es)
  (if (atom es)
    '()
    (if (var? (car es))
      (exprs-recs f (cdr es))
      (if (quote? (car es))
        (exprs-recs f (cdr es))
        (if (if? (car es))
          (list-union
            (exprs-recs f (if-QAE (car es)))
            (exprs-recs f (cdr es)))
          (if (equal (app.name (car es)) f)
            (list-union
              (list1 (car es))
              (list-union
                (exprs-recs f
                  (app.args (car es)))
                (exprs-recs f (cdr es))))
            (list-union
              (exprs-recs f (app.args (car es)))
              (exprs-recs f
                (cdr es)))))))))
(defun expr-recs (f e)
  (exprs-recs f (list1 e)))

(defun totality/< (meas formals app)
  (app-c '<
    (list2 (sub-e formals (app.args app) meas)
      meas)))

(defun totality/meas (meas formals apps)
  (if (atom apps)
    '()
    (cons
      (totality/< meas formals (car apps))
      (totality/meas meas formals (cdr apps)))))

(defun totality/if (meas f formals e)
  (if (if? e)
    (conjunction
      (list-extend
        (totality/meas meas formals
          (expr-recs f (if.Q e)))
        (if-c-when-necessary (if.Q e)
          (totality/if meas f formals
            (if.A e))
          (totality/if meas f formals
            (if.E e)))))
    (conjunction
      (totality/meas meas formals
        (expr-recs f e)))))

(defun totality/claim (meas def)
  (if (equal meas 'nil)
    (if (equal (expr-recs (defun.name def)
                 (defun.body def))
               '())
      (quote-c 't)
      (quote-c 'nil))
    (if-c
      (app-c 'natp (list1 meas))
      (totality/if meas (defun.name def)
        (defun.formals def)
        (defun.body def))
      (quote-c 'nil))))

(defun induction/prems (vars claim apps)
  (if (atom apps)
    '()
    (cons
      (sub-e vars (app.args (car apps)) claim)
      (induction/prems vars claim (cdr apps)))))

(defun induction/if (vars claim f e)
  (if (if? e)
    (implication
      (induction/prems vars claim
        (expr-recs f (if.Q e)))
      (if-c-when-necessary (if.Q e)
        (induction/if vars claim f (if.A e))
        (induction/if vars claim f (if.E e))))
    (implication
      (induction/prems vars claim
        (expr-recs f e))
      claim)))

(defun induction/defun (vars claim def)
  (induction/if vars claim (defun.name def)
    (sub-e (defun.formals def) vars
      (defun.body def))))

(defun induction/claim (defs seed def)
  (if (equal seed 'nil)
    (dethm.body def)
    (induction/defun (app.args seed)
      (dethm.body def)
      (lookup (app.name seed) defs))))

(defun find-focus-at-direction (dir e)
  (if (equal dir 'Q)
    (if.Q e)
    (if (equal dir 'A)
      (if.A e)
      (if (equal dir 'E)
        (if.E e)
        (get-arg dir (app.args e))))))

(defun rewrite-focus-at-direction (dir e1 e2)
  (if (equal dir 'Q)
    (if-c e2 (if.A e1) (if.E e1))
    (if (equal dir 'A)
      (if-c (if.Q e1) e2 (if.E e1))
      (if (equal dir 'E)
        (if-c (if.Q e1) (if.A e1) e2)
        (app-c (app.name e1)
          (set-arg dir (app.args e1) e2))))))

(defun focus-is-at-direction? (dir e)
  (if (equal dir 'Q)
    (if? e)
    (if (equal dir 'A)
      (if? e)
      (if (equal dir 'E)
        (if? e)
        (if (app? e)
          (<=len dir (app.args e))
          'nil)))))

(defun focus-is-at-path? (path e)
  (if (atom path)
    't
    (if (focus-is-at-direction? (car path) e)
      (focus-is-at-path? (cdr path)
        (find-focus-at-direction (car path) e))
      'nil)))

(defun find-focus-at-path (path e)
  (if (atom path)
    e
    (find-focus-at-path (cdr path)
      (find-focus-at-direction (car path) e))))

(defun rewrite-focus-at-path (path e1 e2)
  (if (atom path)
    e2
    (rewrite-focus-at-direction (car path) e1
      (rewrite-focus-at-path (cdr path)
        (find-focus-at-direction (car path) e1)
        e2))))

(defun prem-A? (prem path e)
  (if (atom path)
    'nil
    (if (equal (car path) 'A)
      (if (equal (if.Q e) prem)
        't
        (prem-A? prem (cdr path)
          (find-focus-at-direction (car path)
            e)))
      (prem-A? prem (cdr path)
        (find-focus-at-direction (car path)
          e)))))

(defun prem-E? (prem path e)
  (if (atom path)
    'nil
    (if (equal (car path) 'E)
      (if (equal (if.Q e) prem)
        't
        (prem-E? prem (cdr path)
          (find-focus-at-direction (car path)
            e)))
      (prem-E? prem (cdr path)
        (find-focus-at-direction (car path)
          e)))))

(defun follow-prems (path e thm)
  (if (if? thm)
    (if (prem-A? (if.Q thm) path e)
      (follow-prems path e (if.A thm))
      (if (prem-E? (if.Q thm) path e)
        (follow-prems path e (if.E thm))
        thm))
    thm))

(defun unary-op (rator rand)
  (if (equal rator 'atom)
    (atom rand)
    (if (equal rator 'car)
      (car rand)
      (if (equal rator 'cdr)
        (cdr rand)
        (if (equal rator 'natp)
          (natp rand)
          (if (equal rator 'size)
            (size rand)
            'nil))))))

(defun binary-op (rator rand1 rand2)
  (if (equal rator 'equal)
    (equal rand1 rand2)
    (if (equal rator 'cons)
      (cons rand1 rand2)
      (if (equal rator '+)
        (+ rand1 rand2)
        (if (equal rator '<)
          (< rand1 rand2)
          'nil)))))

(defun apply-op (rator rands)
  (if (member? rator '(atom car cdr natp size))
    (unary-op rator (elem1 rands))
    (if (member? rator '(equal cons + <))
      (binary-op rator
        (elem1 rands)
        (elem2 rands))
      'nil)))

(defun rands (args)
  (if (atom args)
    '()
    (cons (quote.value (car args))
      (rands (cdr args)))))

(defun eval-op (app)
  (quote-c
    (apply-op (app.name app)
      (rands (app.args app)))))

(defun app-of-equal? (e)
  (if (app? e)
    (equal (app.name e) 'equal)
    'nil))

(defun equality (focus a b)
  (if (equal focus a)
    b
    (if (equal focus b)
      a
      focus)))

(defun equality/equation (focus concl-inst)
  (if (app-of-equal? concl-inst)
    (equality focus
      (elem1 (app.args concl-inst))
      (elem2 (app.args concl-inst)))
    focus))

(defun equality/path (e path thm)
  (if (focus-is-at-path? path e)
    (rewrite-focus-at-path path e
      (equality/equation
        (find-focus-at-path path e)
        (follow-prems path e thm)))
    e))

(defun equality/def (claim path app def)
  (if (rator? def)
    (equality/path claim path
      (app-c 'equal (list2 app (eval-op app))))
    (if (defun? def)
      (equality/path claim path
        (sub-e (defun.formals def)
          (app.args app)
          (app-c 'equal
            (list2
              (app-c (defun.name def)
                (defun.formals def))
              (defun.body def)))))
      (if (dethm? def)
        (equality/path claim path
          (sub-e (dethm.formals def)
            (app.args app)
            (dethm.body def)))
        claim))))

(defun rewrite/step (defs claim step)
  (equality/def claim (elem1 step) (elem2 step)
    (lookup (app.name (elem2 step)) defs)))

(defun rewrite/continue (defs steps old new)
  (if (equal new old)
    new
    (if (atom steps)
      new
      (rewrite/continue defs (cdr steps) new
        (rewrite/step defs new (car steps))))))

(defun rewrite/steps (defs claim steps)
  (if (atom steps)
    claim
    (rewrite/continue defs (cdr steps) claim
      (rewrite/step defs claim (car steps)))))

(defun rewrite/prove (defs def seed steps)
  (if (defun? def)
    (rewrite/steps defs
      (totality/claim seed def)
      steps)
    (if (dethm? def)
      (rewrite/steps defs
        (induction/claim defs seed def)
        steps)
      (quote-c 'nil))))

(defun rewrite/prove+1 (defs pf e)
  (if (equal e (quote-c 't))
    (rewrite/prove defs (elem1 pf) (elem2 pf)
      (cdr (cdr pf)))
    e))

(defun rewrite/prove+ (defs pfs)
  (if (atom pfs)
    (quote-c 't)
    (rewrite/prove+1 defs (car pfs)
      (rewrite/prove+
        (list-extend defs (elem1 (car pfs)))
        (cdr pfs)))))

(defun rewrite/define (defs def seed steps)
  (if (equal (rewrite/prove defs def seed steps)
             (quote-c 't))
    (list-extend defs def)
    defs))

(defun rewrite/define+1 (defs1 defs2 pfs)
  (if (equal defs1 defs2)
    defs1
    (if (atom pfs)
      defs2
      (rewrite/define+1 defs2
        (rewrite/define defs2
          (elem1 (car pfs))
          (elem2 (car pfs))
          (cdr (cdr (car pfs))))
        (cdr pfs)))))

(defun rewrite/define+ (defs pfs)
  (if (atom pfs)
    defs
    (rewrite/define+1 defs
      (rewrite/define defs
        (elem1 (car pfs))
        (elem2 (car pfs))
        (cdr (cdr (car pfs))))
      (cdr pfs))))

(defun J-Bob/step (defs e steps)
  (if (defs? '() defs)
    (if (expr? defs 'any e)
      (if (steps? defs steps)
        (rewrite/steps defs e steps)
        e)
      e)
    e))

(defun J-Bob/prove (defs pfs)
  (if (defs? '() defs)
    (if (proofs? defs pfs)
      (rewrite/prove+ defs pfs)
      (quote-c 'nil))
    (quote-c 'nil)))

(defun J-Bob/define (defs pfs)
  (if (defs? '() defs)
    (if (proofs? defs pfs)
      (rewrite/define+ defs pfs)
      defs)
    defs))

(defun axioms ()
  '((dethm atom/cons (x y)
      (equal (atom (cons x y)) 'nil))
    (dethm car/cons (x y)
      (equal (car (cons x y)) x))
    (dethm cdr/cons (x y)
      (equal (cdr (cons x y)) y))
    (dethm equal-same (x)
      (equal (equal x x) 't))
    (dethm equal-swap (x y)
      (equal (equal x y) (equal y x)))
    (dethm if-same (x y)
      (equal (if x y y) y))
    (dethm if-true (x y)
      (equal (if 't x y) x))
    (dethm if-false (x y)
      (equal (if 'nil x y) y))
    (dethm if-nest-E (x y z)
      (if x 't (equal (if x y z) z)))
    (dethm if-nest-A (x y z)
      (if x (equal (if x y z) y) 't))
    (dethm cons/car+cdr (x)
      (if (atom x)
        't
        (equal (cons (car x) (cdr x)) x)))
    (dethm equal-if (x y)
      (if (equal x y) (equal x y) 't))
    (dethm natp/size (x)
      (equal (natp (size x)) 't))
    (dethm size/car (x)
      (if (atom x)
        't
        (equal (< (size (car x)) (size x)) 't)))
    (dethm size/cdr (x)
      (if (atom x)
        't
        (equal (< (size (cdr x)) (size x)) 't)))
    (dethm associate-+ (a b c)
      (equal (+ (+ a b) c) (+ a (+ b c))))
    (dethm commute-+ (x y)
      (equal (+ x y) (+ y x)))
    (dethm natp/+ (x y)
      (if (natp x)
        (if (natp y)
          (equal (natp (+ x y)) 't)
          't)
        't))
    (dethm positives-+ (x y)
      (if (< '0 x)
        (if (< '0 y)
          (equal (< '0 (+ x y)) 't)
          't)
        't))
    (dethm common-addends-< (x y z)
      (equal (< (+ x z) (+ y z)) (< x y)))
    (dethm identity-+ (x)
      (if (natp x) (equal (+ '0 x) x) 't))))

(defun prelude ()
  (J-Bob/define (axioms)
    '(((defun list-induction (x)
         (if (atom x)
           '()
           (cons (car x)
             (list-induction (cdr x)))))
       (size x)
       ((A E) (size/cdr x))
       ((A) (if-same (atom x) 't))
       ((Q) (natp/size x))
       (() (if-true 't 'nil)))
      ((defun star-induction (x)
         (if (atom x)
           x
           (cons (star-induction (car x))
             (star-induction (cdr x)))))
       (size x)
       ((A E A) (size/cdr x))
       ((A E Q) (size/car x))
       ((A E) (if-true 't 'nil))
       ((A) (if-same (atom x) 't))
       ((Q) (natp/size x))
       (() (if-true 't 'nil))))))
