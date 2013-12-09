Clojure core.logic quines.
==========================

This is translation to clojure core.logic for scheme relational interpreter in miniKanren.
Using [@webyrd quines repo](https://github.com/webyrd/quines/) as source.
Introduction is in the [video](http://2013.flatmap.no/danwill.html).

- warmup.clj is to get feel of miniKanren/core.logic
- numbers.clj — translated [numbers.scm](https://github.com/webyrd/quines/blob/master/numbers.scm)
- interp.clj — Non-relational λ-calculus interpreter from [quines.pdf](http://webyrd.net/quines/quines.pdf) (chapter 3).
- core.clj — Relational λ-calculus interpreter ([q.scm](https://github.com/webyrd/quines/blob/master/q.scm) and [mk.scm](https://github.com/webyrd/quines/blob/master/mk.scm))

[core.logic repo](https://github.com/clojure/core.logic) ([logic source file](https://github.com/clojure/core.logic/blob/master/src/main/clojure/clojure/core/logic.clj)),
[@frenchy64 logic tutorial](https://github.com/frenchy64/Logic-Starter/wiki),
[a core.logic primer](https://github.com/clojure/core.logic/wiki/A-Core.logic-Primer).
