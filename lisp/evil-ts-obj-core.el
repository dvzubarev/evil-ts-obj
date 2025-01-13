;;; evil-ts-obj-core.el --- Provides evil text-objects using tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
;; Homepage: https://github.com/dvzubarev/evil-ts-obj
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides evil text-objects using tree-sitter
;;
;;; Code:

(require 'generator)
(require 'treesit)

(require 'evil-ts-obj-conf)
(require 'evil-ts-obj-util)


(defvar evil-ts-obj--command-num 0)
(defvar-local evil-ts-obj--last-text-obj-spec nil)
(defvar-local evil-ts-obj--last-text-obj-range nil)
(defvar-local evil-ts-obj--last-text-obj-node-range nil)
(defvar-local evil-ts-obj--last-traversed-sibling nil)


;;; Range helpers
(defconst evil-ts-obj--range-incl-both 0)
(defconst evil-ts-obj--range-excl-start 1)
(defconst evil-ts-obj--range-excl-end 2)

;;;; Range functions
(cl-defun evil-ts-obj-node-range-create (node &optional end &key
                                              (exclude-start nil)
                                              (exclude-end nil))
  "Create node range from NODE.
If END is not nil it is used as end node. If EXCLUDE-START or
EXCLUDE-END is not exclude corresponding end node from the range. See
`evil-ts-obj--node-range-make-int-range' for more information how
exclusion works."
  (let ((node-range (list node (if end end node) evil-ts-obj--range-incl-both)))
    (when exclude-start
      (evil-ts-obj-node-range-change-start node-range :exclude t))
    (when exclude-end
      (evil-ts-obj-node-range-change-end node-range :exclude t))
    node-range))

(defun evil-ts-obj--node-range-set-bounds-flags (node-range val exclude include)
  ""
  (when (and exclude include)
    (user-error "Passed both exclude and include"))

  (let ((cur (caddr node-range)))
    (cond
     (exclude
      (setf (caddr node-range) (logior cur val)))
     (include
      ;; remove val from cur
      (setf (caddr node-range) (logand cur (lognot val)))))))

(cl-defun evil-ts-obj-node-range-change-start (node-range &optional &key
                                                          (node nil)
                                                          (exclude nil)
                                                          (include nil))
  "Change start node of the NODE-RANGE.
If NODE is not set it as new start node of the NODE-RANGE. if EXCLUDE is
not nil exclude start node from the range. The opposite is for INCLUDE."
  (when node
    (setf (car node-range) node))
  (evil-ts-obj--node-range-set-bounds-flags node-range evil-ts-obj--range-excl-start exclude include))

(cl-defun evil-ts-obj-node-range-change-end (node-range &optional &key
                                                        (node nil)
                                                        (exclude nil)
                                                        (include nil))
  "Change end node of the NODE-RANGE.
If NODE is not set it as new end node of the NODE-RANGE. if EXCLUDE is
not nil exclude end node from the range. The opposite is for INCLUDE."
  (when node
    (setf (cadr node-range) node))
  (evil-ts-obj--node-range-set-bounds-flags node-range evil-ts-obj--range-excl-end exclude include))

(defun evil-ts-obj-node-range-merge (node-range other-node-range)
  "Merge NODE-RANGE and OTHER-NODE-RANGE.
Node ranges should intersect and NODE-RANGE should precede other range."
  (pcase-let* ((`(,start-node ,end-node ,flags) node-range)
               (`(,other-start-node ,other-end-node ,other-flags) other-node-range))
    (when (< (treesit-node-start other-start-node)
             (treesit-node-start start-node))
      (user-error "Start position of other node range should be after the start node range!"))
    (when (< (treesit-node-end end-node)
             (treesit-node-start other-start-node))
      (user-error "Merge is supported only intersected node ranges!"))
    (list start-node other-end-node
          (logior
           (logand flags evil-ts-obj--range-excl-start)
           (logand other-flags evil-ts-obj--range-excl-end)))))

(defun evil-ts-obj--node-range-make-int-range (node-range)
  "Convert NODE-RANGE to the range of integers.
Convert rules:
- [n1, n2] - [(start-pos n1), (end-pos n2)];
- (n1, n2] - [(end-pos n1), (end-pos n2)];
- [n1, n2) - [(start-pos n1), (start-pos n2)];
- (n1, n2) - [(end-pos n1), (start-pos n2)]."
  (pcase-let* ((`(,start-node ,end-node ,flags) node-range)
               (start (treesit-node-start start-node))
               (end (treesit-node-end end-node)))
    (unless (zerop (logand flags evil-ts-obj--range-excl-start))
      (setq start (treesit-node-end start-node)))

    (unless (zerop (logand flags evil-ts-obj--range-excl-end))
      (setq end (treesit-node-start end-node)))

    (list start end)))

;;; Misc functions

(defun evil-ts-obj--pre-command ()
  (cl-incf evil-ts-obj--command-num))

(defun evil-ts-obj--root-at (pos)
  "Return root node at the given `POS'."
  (or (when-let ((parser
                  (car (treesit-local-parsers-at pos))))
        (treesit-parser-root-node parser))
      (treesit-buffer-root-node (treesit-language-at pos))))

(defun evil-ts-obj--smallest-node-at (pos &optional named)
  (let* ((root (evil-ts-obj--root-at pos))
         (node  (treesit-node-on pos pos nil named)))
    (when (not (treesit-node-eq node root))
      node)))

;;; Text objects Range functions

(defun evil-ts-obj--text-bounds-at-pos (pos)
  (let ((syntax (char-to-string (char-syntax (char-after pos))))
        (start nil) (end nil))

    (save-excursion
      (goto-char pos)
      (skip-syntax-backward syntax)
      (setq start (point))
      (goto-char pos)
      (skip-syntax-forward syntax)
      (setq end (point)))
    (cons start end)))

(defun evil-ts-obj--find-next-prev-non-space (pos &optional end-pos)
  (let ((end-pos (or end-pos pos))
        (prev-pos nil)
        (next-pos nil))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t")
      (setq prev-pos
            (and (not (bolp)) (1- (point))))

      (goto-char end-pos)
      (skip-chars-forward " \t")
      (setq next-pos
            (and (not (eolp))
                 (point))))
    (cons prev-pos next-pos)))

(defun evil-ts-obj--get-nearest-words-pos (pos seps)

  (if (memq (char-after pos) '(32 9 10 nil))
      ;; pos at whitespace
      (pcase-let ((`(,prev-pos . ,next-pos) (evil-ts-obj--find-next-prev-non-space pos)))
        (list 'space prev-pos next-pos))

    (pcase-let* ((`(,cur-word-start . ,cur-word-end) (evil-ts-obj--text-bounds-at-pos pos))
                 (cur-word (buffer-substring-no-properties cur-word-start cur-word-end)))

      (if (member cur-word seps)
          (pcase-let ((`(,prev-pos . ,next-pos)
                       (evil-ts-obj--find-next-prev-non-space cur-word-start cur-word-end)))

            (list 'sep prev-pos next-pos))
        (list 'other nil nil)))))

(defun evil-ts-obj--node-at-or-around (pos)
  "Return leaf node, which is at or near `POS'.
If `POS' is inside some leaf node (node-start <= pos < node-end),
then return this node. If `POS' is on a whitespace, examine
previous and next nodes that are on the same line. Prefer named
nodes over anonymous ones. If both nodes are named select the
next one. If both nodes are anonymous prefer node, which text is
a member of `evil-ts-obj-conf-seps' otherwise the next one. If
the pos is on a node that has text that is a member of
`evil-ts-obj-conf-seps' select the previous named node, if it
exists. Return nil if no node can be found."

  (pcase-let* ((seps (plist-get (plist-get evil-ts-obj-conf-seps (treesit-language-at pos))
                                'all))
               (`(,pos-at ,prev-pos ,next-pos) (evil-ts-obj--get-nearest-words-pos pos seps))
               (prefer-previous))

    (pcase pos-at
      ('other
       (treesit-node-at pos))
      ((or 'space 'sep)
       (setq prefer-previous (eq pos-at 'sep))
       (let (node
             next-node
             next-named
             prev-node)

         (when next-pos
           (setq next-node (treesit-node-at next-pos))
           (when (and (not prefer-previous)
                      (setq next-named (treesit-node-check next-node 'named)))
             (setq node next-node)))


         (when (and prev-pos
                    (null node))
           (setq prev-node (treesit-node-at prev-pos))
           (when (or prefer-previous
                     (treesit-node-check prev-node 'named))
             (setq node prev-node)))

         (when (null node)
           (cond (next-named
                  ;; prefer-previous was t, but previous one was null
                  (setq node next-node))

                 ;; no named nodes on both sides
                 ((and
                   prev-node (member (treesit-node-type prev-node) seps))
                  (setq node (evil-ts-obj--node-at-or-around (treesit-node-start prev-node))))
                 ((and
                   next-node (member (treesit-node-type next-node) seps))
                  (if prev-node
                      (setq node prev-node)
                    (setq node (evil-ts-obj--node-at-or-around (treesit-node-start next-node)))))
                 ((null next-node)
                  (setq node prev-node))
                 (t
                  (setq node next-node))))
         node)))))
(defun evil-ts-obj--child-identical? (node child)
  "Determine whether CHILD of a NODE is identical to NODE.
They are considered identical if their ranges are the same or if
the only difference between CHILD and NODE is the termination
symbol. To obtain termination symbols for current language
variable `evil-ts-obj-conf-terms' is consulted."
  (let ((child-start (treesit-node-start child))
        (child-end (treesit-node-end child)))
    (and
     (= (treesit-node-start node) child-start)
     (or (= (treesit-node-end node) child-end)
         (and (= (treesit-node-child-count node) 2)
              (when-let* ((terms (plist-get evil-ts-obj-conf-terms
                                            (treesit-language-at child-start)))
                          (last-child (treesit-node-child node -1)))
                (member (treesit-node-type last-child) terms)))))))

(defun evil-ts-obj--propagate-to-identical-parent (node thing)
  "Handle edge case when the parent is identical to a child NODE.
Parent should also match with THING. They are considered
identical if `evil-ts-obj--child-identical?' returns t."
  (let ((init-start (treesit-node-start node)))
    (while-let ((parent (treesit-node-parent node))
                ((= (treesit-node-start parent) init-start))
                ((treesit-node-match-p parent thing t))
                ((evil-ts-obj--child-identical? parent node)))
      (setq node parent))
    node))


(defun evil-ts-obj--thing-around (pos thing &optional dont-step-forward)
  "Return the node that represents thing, which encloses `POS' or is near it.
`THING' should be a thing defined in `treesit-thing-settings'. If
there is no enclosing thing, jump to the next one unless
`DONT-STEP-FORWARD'. This function also handles the case when
multiple things start at the same position. For example, yaml:

- item1
- item2

 The first hyphen symbol is the beginning of the list and a list
item. When there are multiple overlapping things, the result
depends on `POS'. If `POS' is inside of any thing, then the
smallest enclosing thing is returned. If the `POS' is before any
thing (on the same line), function returns the largest node that
starts after `POS'. If the `POS' is after any thing, function
returns the largest node that ends before `POS'. It returns nil
if no thing can be found (e.g. empty line)."

  (let* ((cursor (evil-ts-obj--node-at-or-around pos))
         (enclosing-node (treesit-parent-until cursor thing t))
         (before-cursor (and enclosing-node
                             (< pos (treesit-node-start enclosing-node))))
         (after-cursor (and enclosing-node
                            (<= (treesit-node-end enclosing-node) pos))))

    (cond
     (before-cursor
      (while (and
              (setq cursor (treesit-parent-until enclosing-node thing))
              (when (= (treesit-node-start enclosing-node)
                       (treesit-node-start cursor))
                (setq enclosing-node cursor)))))
     (after-cursor
      (while (and
              (setq cursor (treesit-parent-until cursor thing))
              (when (= (treesit-node-end enclosing-node)
                       (treesit-node-end cursor))
                (setq enclosing-node cursor))))))

    (evil-ts-obj--propagate-to-identical-parent
     (or enclosing-node
         (unless dont-step-forward
           (treesit-thing-next pos thing))
         ;; last chance; point may be inside empty compound...
         (when-let* ((node (treesit-parent-until (treesit-node-at pos) thing t))
                     ((not (and dont-step-forward
                                (< pos (treesit-node-start node))))))
           node))
     thing)))

(defun evil-ts-obj--current-thing (node things)
  "If `THINGS' is symbol or string it is returned as is.
If it is list, it is considered to be in the form (or <thing1>
<thing2> &rest). In this case, all things are tried sequentially
and the first that matches against `NODE' is returned."
  (pcase things
    ((or (pred symbolp)
         (pred stringp))
     things)
    ((pred listp)
     (seq-find (lambda (thing) (treesit-node-match-p node thing t))
               (cdr things) nil))
    (_ (error "Unsupported thing %s" things))))

(defun evil-ts-obj--make-spec (thing &optional action mod command)
  "Create plist that describes the current text object.
`THING' may be symbol or alist. If it is symbol, it is just added
to spec as is. If it is alist then multiple specs are created,
for each element of `THING' alist. Alist should contain
pairs (thing . mod). If `ACTION' is nil, it is set to op. `MOD'
may be nil if ACTION is nav or it is specified in
THINGs alist. If `COMMAND' is nil the first non-nil from this
list is chosen: `evil-this-operator', `avy-action',
`this-command'. If `evil-visual-state-p' returns t, then keyword
:visual is put to spec with the value t."
  (let* ((action (or action 'op))
         (cmd (or command
                  (bound-and-true-p evil-this-operator)
                  (and (bound-and-true-p avy-action)
                       (not (eq avy-action #'identity))
                       avy-action)
                  this-command))
         (spec `(:thing ,thing
                 :mod ,mod
                 :act ,action
                 :command ,cmd
                 :visual ,(region-active-p))))
    (if (not (listp thing))
        spec
      (mapcar (pcase-lambda (`(,thing . ,mod))
                (let ((spec-copy (copy-sequence spec)))
                  (cl-callf plist-put spec-copy :thing thing)
                  (cl-callf plist-put spec-copy :mod mod)
                  (cons thing spec-copy)))
              thing))))

(defun  evil-ts-obj--make-nav-spec (thing &optional command)
  "Create spec for navigation action.
THING may be symbol or list. If it is list it is expected in the
form (or thing1 thing2 ...). COMMAND is the same as for
`evil-ts-obj--make-spec'."
  (if (not (listp thing))
      (evil-ts-obj--make-spec thing 'nav nil command)
    (evil-ts-obj--make-spec (mapcar (lambda (th) (cons th (pcase th
                                                            ('compound 'outer)
                                                            (_ 'inner))))
                                    (cdr thing))
                            'nav
                            nil command)))

(defun evil-ts-obj--default-range (node spec)
  "Return default text object node range for a `NODE' based on `SPEC'.
This function produces basic outer-{param,statement} and
{upper,lower}-{param,statement} text objects. It returns node range - a
list of start, end nodes and range flags."
  (let ((node-range (evil-ts-obj-node-range-create node))
        (thing (plist-get spec :thing))
        (act (plist-get spec :act)))

    (if (eq act 'nav)
        ;; If we collecting text objects for movement or previewing of candidates,
        ;; we do not modify thing bounds for now by default.
        node-range
      (if-let* ((lang (treesit-language-at (treesit-node-start node)))
                (thing-trav (plist-get evil-ts-obj-conf-sibling-trav lang))
                (trav (plist-get thing-trav thing))
                (fetcher (evil-ts-obj-trav-fetcher trav))
                (kind-func (evil-ts-obj-trav-kind-func trav))
                (kind-lambda (lambda (cn ck n)
                               (funcall kind-func cn ck n (plist-get
                                                           (plist-get evil-ts-obj-conf-seps lang)
                                                           thing))))
                (upper-lower-extend (or (eq thing 'param) 'if-sep-found)))
          ;; Handle outer/upper/lower using traverse functions.
          (pcase spec
            ((pmap (:mod 'outer))
             (evil-ts-obj-generic-thing-with-sep-outer node kind-lambda fetcher
                                                       (not (eq thing 'param))
                                                       (eq thing 'param)))
            ((pmap (:mod 'upper))
             (evil-ts-obj-generic-thing-upper node kind-lambda fetcher))
            ((pmap (:mod 'UPPER))
             (evil-ts-obj-generic-thing-upper node kind-lambda fetcher upper-lower-extend))
            ((pmap (:mod 'lower))
             (evil-ts-obj-generic-thing-lower node kind-lambda fetcher))
            ((pmap (:mod 'LOWER))
             (evil-ts-obj-generic-thing-lower node kind-lambda fetcher upper-lower-extend))
            ((pmap (:mod 'all))
             (evil-ts-obj-node-range-merge
              (evil-ts-obj-generic-thing-upper node kind-lambda fetcher nil)
              (evil-ts-obj-generic-thing-lower node kind-lambda fetcher nil)))
            (_
             node-range))
        ;; default handling of upper/lower text objects
        (let ((parent (treesit-node-parent node)))
          (pcase spec
            ((pmap (:mod (or 'upper 'UPPER)))
             (let ((final-sibling (treesit-node-child parent 0 t)))
               (evil-ts-obj-node-range-change-start node-range :node final-sibling)))
            ((pmap (:mod (or 'lower 'LOWER)))
             (let ((final-sibling (treesit-node-child parent -1 t)))
               (evil-ts-obj-node-range-change-end node-range :node final-sibling)))
            ((pmap (:mod 'all))
             (let ((first-node (treesit-node-child parent 0 t))
                   (last-node (treesit-node-child parent -1 t)))
               (evil-ts-obj-node-range-create first-node last-node)))
            (_
             node-range)))))))

(defun evil-ts-obj--apply-modifiers (node thing spec &optional dont-set-last)
  "Apply modifiers for creating a text object from the `THING'.
THING may be a symbol or a list of multiple things. If thing is a
list then one thing that represents the NODE is kept. In case if
THING is symbol, SPEC is a plist that contains all context for
the current operation, otherwise it is alist that maps thing to a
spec. This function returns integer range that is obtained via transformation
function from `evil-ts-obj-conf-thing-modifiers'. NODE and SPEC are passed
to modifiers. If no modifier is found or modifier returns nil
fallback to `evil-ts-obj--default-range'.

This function sets a number of local variables:

- `evil-ts-obj--last-text-obj-spec' - the specification of the thing.
- `evil-ts-obj--last-text-obj-range' - integer range after all extension
- functions were applied.
- `evil-ts-obj--last-text-obj-node-range' - range of nodes from which
  integer range was derived.

If `DONT-SET-LAST' is t do not update those variables."
  (when node
    (let* ((current-thing (evil-ts-obj--current-thing node thing))
           (current-spec (if (listp thing)
                             (alist-get current-thing spec)
                           spec))
           (range
            (if-let* ((pos (treesit-node-start node))
                      (modifier (plist-get evil-ts-obj-conf-thing-modifiers
                                           (treesit-language-at pos)))
                      (r (funcall modifier current-spec node)))
                r
              (evil-ts-obj--default-range node current-spec)))
           int-range
           node-range)
      (pcase range
        ((pred treesit-node-p)
         (setq int-range (list (treesit-node-start range) (treesit-node-end range))
               node-range (evil-ts-obj-node-range-create range)))
        (`(,(pred integerp) ,(pred integerp))
         (setq int-range range
               node-range (evil-ts-obj-node-range-create node)))
        (`(,(pred treesit-node-p) ,(pred treesit-node-p) ,(pred integerp))
         (setq int-range (evil-ts-obj--node-range-make-int-range range)
               node-range range))
        (`(,(and (pred integerp) start) ,(and (pred integerp) end)
           ,(pred treesit-node-p) ,(pred treesit-node-p) ,(pred integerp))
         (setq int-range (list start end)
               node-range (cddr range)))
        (_ (user-error "Unknown range pattern %s" range)))


      (unless dont-set-last
        (setq evil-ts-obj--last-text-obj-spec (plist-put current-spec :cmd-num evil-ts-obj--command-num)
              evil-ts-obj--last-text-obj-node-range (copy-sequence node-range)
              evil-ts-obj--last-text-obj-range (copy-sequence int-range)))
      int-range)))


(defun evil-ts-obj--expand-active-region? (thing spec)
  ""
  (when-let* (((not (listp thing)))       ;multiple things are not supported
              ((plist-get spec :visual))
              ((plist-get evil-ts-obj--last-text-obj-spec :visual))
              ((eq (plist-get spec :mod)
                   (plist-get evil-ts-obj--last-text-obj-spec :mod)))
              (start (region-beginning))
              (end (1+ (region-end)))
              (cur-range (list start end))
              ((equal cur-range evil-ts-obj--last-text-obj-range)))
    ;; Selected the same text object in the visual state.
    cur-range))

(defun evil-ts-obj--finalize-text-obj-range (spec range)
  "Perform default finalizing operations on a RANGE.
SPEC describes a context for the current RANGE. This function
removes trailing new lines for compound outer text objects."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'outer))
     (pcase-let ((`(,first-pos ,last-pos) range))
       (when (and
              last-pos
              (eq (char-before last-pos) ?\n))
         (setq last-pos (1- last-pos)))
       (list first-pos last-pos)))
    (_ range)))


(defun evil-ts-obj--get-text-obj-range (pos-or-node thing spec &optional
                                                    extend-over-range
                                                    return-node)
  "Return a range for text object described via `SPEC'.
More information about `SPEC' and `THING' is in
`evil-ts-obj--apply-modifiers' documentation. If `POS-OR-NODE' is not
treesit-node, then it is considered to be position in the buffer. In
that case, find `THING' that is around this position using
`evil-ts-obj--thing-around'. If `EXTEND-OVER-RANGE' is not nil or region
is active and the last returned range is equal to the active region,
expand that region. It is done by searching for a matching parent of the
current thing. Otherwise apply range modifiers to the found thing and
return range of a text-object. If `RETURN-NODE' is t, return original
node that represents THING. Node is appended to the returned range. This
is node that is passed to `evil-ts-obj--apply-modifiers', so it is
generally not the node that was used for computing range returned by
this function. Consult `evil-ts-obj--apply-modifiers' for information
about access to nodes from which returned range was derived."

  (when-let ((node (if (treesit-node-p pos-or-node)
                       pos-or-node
                     (when (integerp pos-or-node)
                       (evil-ts-obj--thing-around pos-or-node thing)))))
    (let* ((pos (treesit-node-start node))
           (lang (treesit-language-at pos))
           (finalizer (plist-get evil-ts-obj-conf-range-finalizers lang))
           (range (if-let* ((bounds (or extend-over-range
                                        (evil-ts-obj--expand-active-region? thing spec)))
                            (bound-start (car bounds))
                            (bound-end (cadr bounds))
                            (cur-range (evil-ts-obj--apply-modifiers node thing spec)))

                      ;; Extend to the parent thing.
                      (progn
                        (while (and cur-range
                                    (<= bound-start (car cur-range))
                                    (<= (cadr cur-range) bound-end))
                          (setq node (treesit-parent-until node thing)
                                cur-range (evil-ts-obj--apply-modifiers node thing spec)))
                        cur-range)
                    ;; Bounds is nil, just apply modifiers.
                    (evil-ts-obj--apply-modifiers node thing spec))))
      (when (and finalizer range)
        (setq range (funcall finalizer evil-ts-obj--last-text-obj-spec range)))
      (append range (when (and range return-node) (list node))))))

(defun evil-ts-obj--last-range-of-this-cmd (&optional return-spec)
  "Return range plist of the last text object created during this command.
When RETURN-SPEC is t, return the spec plist object. If no text
objects were created during this command returns nil."
  (when (eql (plist-get evil-ts-obj--last-text-obj-spec :cmd-num)
             evil-ts-obj--command-num)
    (if return-spec
        evil-ts-obj--last-text-obj-spec
      evil-ts-obj--last-text-obj-range)))

(defun evil-ts-obj--last-thing-of-this-cmd (&optional fallback)
  "Return thing of the last text object created during this command.
If no text objects were created returns FALLBACK. If FALLBACK is
nil returns symbol \\='opaque."
  (or (plist-get (evil-ts-obj--last-range-of-this-cmd t) :thing)
      (or fallback 'opaque)))

;;; Movement

(defun evil-ts-obj--goto-begin-of-thing (thing)
  "Determine `THING' at point and move point to the beginning of it.
Beginning position is calculated based on spec with :act set
to nav, so all nav modifiers affect it (see
`evil-ts-obj-conf-thing-modifiers'). If point is already at the
beginning, move to the beginning of the parent thing."
  (when-let* ((spec (evil-ts-obj--make-nav-spec thing))
              (node (evil-ts-obj--thing-around (point) thing))
              (range (evil-ts-obj--get-text-obj-range node thing spec)))
    ;; jump to beginning of a parent.
    ;; Take into consideration that parent may start on the same position as the child.
    (while-let (((<= (point) (car range)))
                (parent-node (treesit-parent-until node thing)))
      (setq node parent-node
            range (evil-ts-obj--get-text-obj-range parent-node thing spec)))
    (when range
      (goto-char (car range)))))

(defun evil-ts-obj--goto-end-of-thing (thing)
  "Determine `THING' at point and move point to the end of it.
End position is calculated based on spec with :act set to
nav, so all nav modifiers affect it (see
`evil-ts-obj-conf-thing-modifiers'). If point is already at the
end, move to the end of the parent thing."
  (when-let* ((spec (evil-ts-obj--make-nav-spec thing))
              (node (evil-ts-obj--thing-around (point) thing))
              (range (evil-ts-obj--get-text-obj-range node thing spec)))
    (while-let (((<= (1- (cadr range)) (point)))
                (parent-node (treesit-parent-until node thing)))
      (setq node parent-node
            range (evil-ts-obj--get-text-obj-range parent-node thing spec)))

    (when range
      (goto-char (1- (cadr range))))))



(defun evil-ts-obj--jump-boundaries ()
  (interactive)
  (when-let* ((thing evil-ts-obj-conf-nav-things)
              (node (evil-ts-obj--thing-around (point) thing)))
    (cond
     ((= (treesit-node-start node) (point)) (goto-char (1- (treesit-node-end node))))
     ((= (1- (treesit-node-end node)) (point)) (goto-char (treesit-node-start node)))
     (t (goto-char (treesit-node-end node))))))


(defun evil-ts-obj--maybe-set-jump (init-enclosing-node node init-pos)
  (when (or (not (treesit-node-eq (treesit-node-parent init-enclosing-node)
                                  (treesit-node-parent node)))
            (and init-enclosing-node
                 (/= (treesit-node-start init-enclosing-node)
                     init-pos)))
    ;; we step up one or more times to parent,
    ;; so it won't be use to return to previous position.
    ;; Save position to jump list
    (when (fboundp 'evil-set-jump)
      (evil-set-jump))))

(defun evil-ts-obj--next-thing (thing init-enclosing-node init-pos spec)
  "Return node and range that represents the next `THING'.
When `INIT-ENCLOSING-NODE' is not nil, it is considered to be a
node that represents current thing. Next thing should be
associated with the node that is not equal to
`INIT-ENCLOSING-NODE' and that starts after `INIT-POS'. Create
range according to provided `SPEC'."

  (let ((parent init-enclosing-node)
        (pos (treesit-node-end init-enclosing-node))
        next range)
    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move forward
        (setq next (treesit-thing-next init-pos thing)
              range (evil-ts-obj--get-text-obj-range next thing spec))

      ;; Try to move to next thing from pos.
      ;; Just in case if no next thing is found, go to parent for
      ;; so we can make a progress in the next iteration.
      (while (and (or (null range)
                      ;; have to find a range that will move us to the right from
                      ;; the current position
                      (<= (car range) init-pos))
                  (< pos (point-max)))
        (setq next (treesit-thing-next pos thing)
              range (evil-ts-obj--get-text-obj-range next thing spec)
              parent (treesit-node-parent parent)
              pos (treesit-node-end parent))))

    (cons next range)))

(defun evil-ts-obj--goto-next-largest-thing (thing)
  "Go to the next largest `THING'.
At first, determine current THING at point. After that move to
the next largest `THING' that starts after the current thing
ends."
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         next
         range)

    (pcase-setq `(,next . ,range)
                (evil-ts-obj--next-thing thing init-enclosing-node init-pos spec))

    (when-let ((node next))
      (evil-ts-obj--maybe-set-jump init-enclosing-node node init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--goto-next-sibling-thing (thing)
  "Go to the next sibling `THING'.
At first, determine current THING at point. After that move to
its next sibling. If no next sibling move to the next largest thing."

  (let* ((init-pos (point))
         (spec (evil-ts-obj--make-nav-spec thing))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         next range)

    (when init-enclosing-node
      ;; simple case: try to find next sibling
      (setq next (evil-ts-obj--find-matching-sibling
                  init-enclosing-node
                  (evil-ts-obj--current-thing init-enclosing-node thing)
                  'next thing)
            range (evil-ts-obj--get-text-obj-range next thing spec)))

    (unless next
      (when (and init-enclosing-node evil-ts-obj--last-traversed-sibling)
              (setq init-enclosing-node evil-ts-obj--last-traversed-sibling))
      (pcase-setq `(,next . ,range)
                  (evil-ts-obj--next-thing thing init-enclosing-node init-pos spec)))

    (when next
      (evil-ts-obj--maybe-set-jump init-enclosing-node next init-pos)
      (goto-char (car range)))))


(defun evil-ts-obj--prev-thing (thing init-enclosing-node init-pos spec)
  "Return node and range that represents the previous `THING'.
When `INIT-ENCLOSING-NODE' is not nil, it is considered to be a
node that represents current thing. Previous thing should be
associated with the node that is not equal to
`INIT-ENCLOSING-NODE' and that starts before `INIT-POS'. Create
range according to provided `SPEC'. Jump to parent of
INIT-ENCLOSING-NODE if no previous thing exists."

  (let ((parent init-enclosing-node)
        (pos (treesit-node-start init-enclosing-node))
        prev range)

    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move backward
        (setq prev (treesit-thing-prev init-pos thing)
              range (evil-ts-obj--get-text-obj-range prev thing spec))

      (while (and (or (null range)
                      (treesit-node-eq init-enclosing-node prev)
                      ;; have to find a range that will move us to the left from
                      ;; the current position
                      (<= init-pos (car range)))
                  (< (point-min) pos))
        ;; trying to get previous thing
        (setq prev (treesit-thing-prev pos thing))

        (when (null prev)
          ;; try to step up till we move from the start position of current enclosing node
          ;; make one step per iteration
          (setq parent (treesit-parent-until parent thing)
                prev parent
                pos (or (treesit-node-start prev) 0)))
        (setq range (evil-ts-obj--get-text-obj-range prev thing spec))))

    (cons prev range)))

(defun evil-ts-obj--goto-prev-largest-thing (thing)
  "Go to the previous `THING'.
At first, determine current THING at point. After that move to
the `THING' that ends before the current thing starts. If no such
a THING exists jump to a parent THING."
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing))
         prev range)

    (pcase-setq `(,prev . ,range)
                (evil-ts-obj--prev-thing thing init-enclosing-node init-pos spec))

    (when range
      (evil-ts-obj--maybe-set-jump init-enclosing-node prev init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--goto-prev-sibling-thing (thing)
  "Go to the previous sibling `THING'.
At first, determine current THING at point. After that move to
its previous sibling. If no next sibling move to the parent of
the current thing thing."

  (let* ((init-pos (point))
         (spec (evil-ts-obj--make-nav-spec thing))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         prev range)

    (when init-enclosing-node
      ;; simple case: try to find next sibling
      (setq prev (evil-ts-obj--find-matching-sibling
                  init-enclosing-node
                  (evil-ts-obj--current-thing init-enclosing-node thing)
                  'prev thing)
            range (evil-ts-obj--get-text-obj-range prev thing spec)))

    (unless prev
      (when (and init-enclosing-node evil-ts-obj--last-traversed-sibling)
        (setq init-enclosing-node evil-ts-obj--last-traversed-sibling))
      (pcase-setq `(,prev . ,range)
                  (evil-ts-obj--prev-thing thing init-enclosing-node init-pos spec)))

    (when prev
      (evil-ts-obj--maybe-set-jump init-enclosing-node prev init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--get-nav-thing (&optional current)
  "Return thing for the motion for current language.
Language is decided based on current point position.
If `CURRENT' is t, detect current thing at point and return this thing."
  (when-let* ((lang (treesit-language-at (point)))
              (thing (or (plist-get evil-ts-obj-conf-nav-things lang)
                         'compound)))
    ;; try to guess to what thing to move
    (when-let* (current
                ((listp thing))
                (node (evil-ts-obj--thing-around (point) thing))
                (cur-thing (evil-ts-obj--current-thing node thing)))
      (setq thing cur-thing))
    thing))



(defun evil-ts-obj--search-subtree (node thing child-filter &optional backward match-fn)
  (let* ((filtered-children (seq-filter child-filter (treesit-node-children node t)))
         (children (if backward (nreverse filtered-children) filtered-children))
         (match-pred (or match-fn
                         (lambda (n) (and (funcall child-filter n)
                                          (treesit-node-match-p n thing t)))))
         result)

    (cl-loop for child in children
             do (setq result (treesit-search-subtree child match-pred backward))
             when result
             return result)))

(defun evil-ts-obj--search-subtree-forward (start-node thing spec init-pos)
  (let ((filter (lambda (c) (< init-pos (treesit-node-end c))))
        (match-pred (lambda (n) (and (treesit-node-match-p n thing t)
                                     (not (evil-ts-obj--child-identical? start-node n))
                                     (let ((range (evil-ts-obj--get-text-obj-range n thing spec)))
                                       (< init-pos (car range)))))))
    (evil-ts-obj--search-subtree start-node thing filter nil match-pred)))

(defun evil-ts-obj--search-subtree-backward (start-node thing spec init-pos)
  (let ((filter (lambda (c) (< (treesit-node-start c) init-pos)))
        (match-pred (lambda (n) (and (treesit-node-match-p n thing t)
                                     (not (evil-ts-obj--child-identical? start-node n))
                                     (let ((range (evil-ts-obj--get-text-obj-range n thing spec)))
                                       (< (car range) init-pos)))))
        (node start-node)
        child)
    ;; have to find the deepest node for that subtree
    (while (setq node (evil-ts-obj--search-subtree node thing filter t match-pred))
      (setq child node
            start-node node))

    child))

(defun evil-ts-obj--goto-next-thing (thing)
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         range)
    (if-let ((node init-enclosing-node)
             (next (evil-ts-obj--search-subtree-forward node thing spec init-pos)))
        (setq range (evil-ts-obj--get-text-obj-range next thing spec))
      (pcase-setq `(,next . ,range)
                  (evil-ts-obj--next-thing thing init-enclosing-node init-pos spec)))
    (when range
      (goto-char (car range)))))

(defun evil-ts-obj--goto-prev-thing (thing)
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         (cursor (or enclosing-node
                     (evil-ts-obj--node-at-or-around init-pos)
                     (treesit-node-at init-pos)))
         range)

    ;; search backwardly inside current thing
    (if-let ((node enclosing-node)
             (prev (evil-ts-obj--search-subtree-backward node thing spec init-pos)))
        (setq range (evil-ts-obj--get-text-obj-range prev thing spec))

      ;; no matching things inside the current thing
      ;; search backwardly starting from  enclosing node
      (while (and cursor
                  (or (null range)
                      ;; have to find a range that will move (<-) us from the
                      ;; current position
                      (<= init-pos (car range))))

        (setq prev (treesit-search-forward
                    cursor
                    (lambda (n) (treesit-node-match-p n thing t))
                    t)
              cursor prev
              range (evil-ts-obj--get-text-obj-range prev thing spec))))


    (when range
      (goto-char (car range)))))


;;; Common predicates


(defun evil-ts-obj--bool-expr-check-op (node bool-ops)
  "Return t if NODE has field operator and its value from BOOL-OPS.
BOOL-OPS is a list of possible valid boolean operators. If
node\\='s operator: field is a member of bool-ops list, then this
function returns t."

  (or
   (member (treesit-node-type
            (treesit-node-child-by-field-name node "operator"))
           bool-ops)
   ;; Bash does not have field operator in list structure.
   ;; Just search for anonymous nodes that equal to some of bool-ops.
   (seq-some (lambda (node)
               (member (treesit-node-type node) bool-ops))
             (treesit-node-children node))))

(defun evil-ts-obj--common-bool-expr-pred (node bool-op-type bool-ops)
  "Return t if NODE is a part of a boolean expression.
BOOL-OP-TYPE is a type of boolean expression node. BOOL-OPS is a
list of possible boolean operators."
  (when-let* ((parent (treesit-node-parent node))
              (parent-type (treesit-node-type parent))
              ((and (equal parent-type bool-op-type)
                    (member (treesit-node-field-name node) '("left" "right"))
                    ;; Some languages (c++/bash - binary_expression) use the same node type for boolean expression
                    ;; and the comparison/arithmetic operators.
                    ;; We do not want to match parts of comparison operators,
                    ;; only the whole operator. See also comments below.
                    (evil-ts-obj--bool-expr-check-op parent bool-ops))))

    ;; Match only left/right parts of an expression, not the whole boolean expression.
    ;; 1. If the current node is not bool expression type, we are good.
    ;; 2. If an operator that used in the current expresion is not in `bool-ops'
    ;; than it might be just comparison or arithmetic operation.
    (or (not (equal (treesit-node-type node) bool-op-type))
        (not (evil-ts-obj--bool-expr-check-op node bool-ops)))))

(defun evil-ts-obj-common-param-pred (parent-regex node)
  "Predicate for detecting param thing.
Return t if `NODE' is named and its parent is matching against
`PARENT-REGEX'."
  (when-let* (((treesit-node-check node 'named))
              ((not (equal (treesit-node-type node) "comment")))
              (parent (treesit-node-parent node)))
    (string-match-p parent-regex (treesit-node-type parent))))

(defun evil-ts-obj--by-field-name-pred (node fields)
  "Return t if NODE\\='s field name matches any field from FIELDS list.
FIELDS is a list of conses where the car is a parent type and the
cdr is a field name. Parent type may be nil, then match only by
field name."
  (let* ((parent (treesit-node-parent node))
         (parent-type (treesit-node-type parent)))
    (cl-some (pcase-lambda (`(,par . ,field))
               (and (or (null par)
                        (equal parent-type par))
                    (equal (treesit-node-field-name node) field)))
             fields)))


;;; Traversing Siblings

(cl-defstruct (evil-ts-obj-trav (:constructor evil-ts-obj-trav-create)
                                (:copier nil))
  "Structure that holds all information for traversing siblings.
FETCHER is a function that accepts DIR and NODE
parameters. It should return the immediate sibling of the NODE.
For example, see `evil-ts-obj--get-sibling-simple'.
KIND-FUNC is a function that classifies the passed node and returns its kind.
See `evil-ts-obj-conf-sibling-trav' for more information."
  fetcher kind-func)


(defun evil-ts-obj--get-sibling-simple (dir node)
  "Implementation of a node fetcher for `evil-ts-obj-conf-sibling-trav'.
Return a next or previous sibling for `NODE' based on value of
`DIR'."
  (pcase dir
    ('next (treesit-node-next-sibling node))
    ('prev (treesit-node-prev-sibling node))))

(defun evil-ts-obj--get-sibling-bin-op (node-expr-types node-expr-ops dir node)
  "Traverse typical tree structure of binary operators.
Return sibling of a given `NODE' in the specified direction `DIR'.
Parent of the `NODE' should have one of the type from the
`NODE-EXPR-TYPES'. NODE-EXPR-OPS is a list of operators that separate
statements in a tree. NODE-EXPR-OPS is used to distinguish between
boolean expressions and comparison/arithmetic operations in languages,
where there single node type is employed for all binary operations.
For a tree like below (Example 1), when `NODE' is (3) and `DIR'
is prev, jump to the the last child of its sibling (node (5)).
When node is (5) and `DIR' is next jump to its grandparent's
second child (node (3)). 
Example 1:
         (1) bin_op----------
               |            |
         (2) bin_op------  (3) and i < 9
               |        |
         (4) bin_op    (5) and j < 3
               |
    (6) i > 0 and j > 0
Example 2:
         (1) bin_op ----------
               |             |
         (2) i > 0 and  (3) bin_op
                             |
                        (4) j < 3 and (5) i < 9"


  (when-let* ((parent (treesit-node-parent node))
              ((member (treesit-node-type parent) node-expr-types)))
    (let ((sibling (evil-ts-obj--get-sibling-simple dir node)))
      (pcase (treesit-node-type sibling)
        ('()
         (when-let* ((grandparent (treesit-node-parent parent))
                     ((member (treesit-node-type grandparent) node-expr-types)))
           (if (eq dir 'next)
               ;;Example 1: Handle transition between (5) -> (3).
               ;;It also might be node (5) in Example 2, so check that
               ;;sibling-cand is actually next sibling.
               (when-let* ((sibling-cand (treesit-node-child grandparent 1))
                           ((< (treesit-node-start node) (treesit-node-start sibling-cand))))
                 sibling-cand)
             ;; Example 2: Handle transition between (2) <- (4)
             ;; Also check that it is not node (6) in Example 1.
             (when-let* ((sibling-cand (treesit-node-child grandparent -2))
                         ((< (treesit-node-end sibling-cand) (treesit-node-end node))))
               sibling-cand))))
        ((pred (seq-contains-p node-expr-types))
         (if (eq dir 'prev)
             ;;Example 1: handle transition (5) <- (3)
             (if (evil-ts-obj--bool-expr-check-op sibling node-expr-ops)
                 ;; This node is boolean expression so take the :right child of it.
                 (or (treesit-node-child-by-field-name sibling "right")
                     ;; Again bash...
                     ;; List node does not have fielts right/left...
                     (treesit-node-child sibling -1 t))
               sibling)
           ;;Example 2: handle transition (2) -> (4)
           (if (evil-ts-obj--bool-expr-check-op sibling node-expr-ops)
               ;; This node is boolean expression so take the :left child of it.
               (or (treesit-node-child-by-field-name sibling "left")
                   (treesit-node-child sibling 0 t))
             sibling)))
        (_ sibling)))))

(defun evil-ts-obj--common-get-statement-sibling (dir node bool-expr-types bool-expr-ops)
  "Implementation of a node fetcher for `evil-ts-obj-conf-sibling-trav'.
Return a next or previous sibling for `NODE' based on value of `DIR'.
This function handles traversing of statements in boolean expressions.
Boolean node names are passed in BOOL-EXPR-TYPE variable, as well as
list of boolean expression operators in BOOL-EXPR-OPS (see
`evil-ts-obj--get-sibling-bin-op'). Fallback to
`evil-ts-obj--get-sibling-simple', if NODE is not inside boolean
expression."
  (or (evil-ts-obj--get-sibling-bin-op bool-expr-types bool-expr-ops dir node)
      (evil-ts-obj--get-sibling-simple dir node)))

(defun evil-ts-obj--nodes-on-the-same-line (node other-node)
  "Return t if `NODE' and `OTHER-NODE' end on the same line."
  (save-excursion
    (goto-char (treesit-node-end node))
    (let ((other-end (treesit-node-end other-node)))
      (and
       (<= other-end (line-end-position))
       (>= other-end (line-beginning-position))))))

(defun evil-ts-obj--get-node-kind-strict (cur-node cur-kind node &optional seps)
  "Return sibling kind for `NODE'.
Implementation of a kind-func for
`evil-ts-obj-conf-sibling-trav'. The main difference from
`evil-ts-obj--get-node-kind' - this function returns sibling
symbol only if `NODE' is preceded by the separator.

Return sep if `NODE' type is a member of `SEPS'.
Otherwise return sibling if `NODE' is named and current node is
separator (`CUR-KIND'). Return term if `NODE' is anonymous and on
the same line as `CUR-NODE' (examples of possible termination
nodes: \) ,\] ,end ,fi, etc.)."


  (when (null seps)
    (user-error "You must pass seps to this function!"))

  (if (member (treesit-node-type node) seps)
      'sep
    (let ((named (treesit-node-check node 'named)))
      (pcase (cons named cur-kind)
        (`(t . ,'sep) 'sibling)
        ((and `(nil . ,_)
              (guard (evil-ts-obj--nodes-on-the-same-line node cur-node)))
         'term)))))

(defun evil-ts-obj--get-node-kind (cur-node _cur-kind node &optional seps)
  "Return sibling kind for `NODE'.
Implementation of a kind-func for
`evil-ts-obj-conf-sibling-trav'.

Return sep if `NODE' type is a member of `SEPS'. Otherwise
return sibling if `NODE' is named. Return term if `NODE' is
anonymous and on the same line as `CUR-NODE' (examples of
possible termination nodes: \) ,\] ,end ,fi, etc.)."

  (cond
   ((member (treesit-node-type node) seps)
    'sep)
   ((treesit-node-check node 'named)
    'sibling)
   ((when (evil-ts-obj--nodes-on-the-same-line node cur-node)
      'term))))

(iter-defun evil-ts-obj--iter-siblings (node node-thing dir match-thing &optional yield-all)
  "Return sibling of a NODE in a direction DIR.
DIR is either prev or next. NODE-THING is a thing that is represented by
the NODE. Returned sibling will match with the thing specified in
MATCH-THING. If YIELD-ALL is t yield every node that either is sibling
or separator. MATCH-THING is ignored"
  (let* ((lang (treesit-language-at (treesit-node-start node)))
         (trav-things (plist-get evil-ts-obj-conf-sibling-trav lang))
         (trav (plist-get trav-things node-thing))
         (fetcher (if trav (evil-ts-obj-trav-fetcher trav)
                    #'evil-ts-obj--get-sibling-simple))
         (kind-func (if trav (evil-ts-obj-trav-kind-func trav)
                      #'evil-ts-obj--get-node-kind))
         (seps (plist-get (plist-get evil-ts-obj-conf-seps lang)
                          node-thing))
         (kind-lambda (lambda (cn ck n) (funcall kind-func cn ck n seps)))
         (cur-node node)
         (cur-kind 'sibling))

    (setq evil-ts-obj--last-traversed-sibling nil)
    (while (and (setq node (funcall fetcher dir node))
                (setq cur-kind (funcall kind-lambda cur-node cur-kind node))
                (setq cur-node node)
                (memq cur-kind '(sibling sep)))
      (setq evil-ts-obj--last-traversed-sibling cur-node)
      (when (or yield-all
                (and (eq cur-kind 'sibling)
                     (treesit-node-match-p node match-thing t)))
        (iter-yield node)))))

(defun evil-ts-obj--find-matching-sibling (node node-thing dir match-thing &optional count)
  "Return Nth sibling of the NODE.
By default return first sibling. COUNT may be positive integer
specifying the number of a sibling to return. For description of
NODE-THING, DIR, MATCH-THING see `evil-ts-obj--iter-siblings'."
  (let ((iter (evil-ts-obj--iter-siblings node node-thing dir match-thing))
        (count (or count 1))
        result)
    (condition-case nil
        (progn
          (dotimes (_ count)
            (setq result (iter-next iter)))
          (iter-close iter))
      (iter-end-of-sequence nil))
    result))


;;; Modifiers
;;;
(defun evil-ts-obj-string-inner-py-style (node)
  "Return rnage for a string inner text object.
String is represented by a NODE. This function works when node has
string_start and string_end childs."
  (when-let* ((first-child (treesit-node-child node 0 t))
              ((equal "string_start" (treesit-node-type first-child)))
              (last-child (treesit-node-child node -1 t))
              ((equal "string_end" (treesit-node-type last-child)))
              (node-range (evil-ts-obj-node-range-create first-child last-child
                                                         :exclude-start t :exclude-end t)))

    node-range))

(cl-defun evil-ts-obj-string-inner-c-style (node &optional &key
                                                             (string-nodes '("string_literal")))
  "Return range for string inner text object represented by a NODE.
NODE should be of type passed in STRING-NODES (by default
string_literal) and has two anonymous children.
This function also supports or raw_string_literal nodes.
It expects to find a child of type either raw_string_content or string_content."
  (pcase (treesit-node-type node)
    ((pred (member _ string-nodes))
     ;; We can't use string_content node since there might be any number of
     ;; escape_sequence nodes inside string_literal.
     ;; Also when string is emty it usually does not contain any string_content nodes.
     ;; Assume that first and the last children are quote symbols.
     ;; We don't check their surface strings since there are many variants in different languages.
     ;; Just check that they are anonymous.
     (when-let* ((first-child (treesit-node-child node 0))
                 ((not (treesit-node-check first-child 'named)))
                 (last-child (treesit-node-child node -1))
                 ((not (treesit-node-check last-child 'named)))
                 (node-range (evil-ts-obj-node-range-create first-child last-child
                                                            :exclude-start t :exclude-end t)))

       node-range))
    ("raw_string_literal"
     (when-let ((child (seq-find
                        (lambda (n)
                          (member (treesit-node-type n) '("raw_string_content" "string_content")))
                        (treesit-node-children node t))))
       (evil-ts-obj-node-range-create child)))))

(defun evil-ts-obj--generic-find-sep-and-sibling (node node-kind-func next-node-func)
  "Return list (next-sibling next-sep next-term) for a `NODE'.
A function `NEXT-NODE-FUNC' should accept a `NODE' as an argument
and return next sibling node. `NODE-KIND-FUNC' is described in
`evil-ts-obj-conf-sibling-trav'."
  (let ((node-kind nil)
        next-sep next-sibling next-term)
    (when-let* ((next-node (funcall next-node-func node))
                (node-kind (funcall node-kind-func node node-kind next-node)))
      (pcase node-kind
        ('sep
         (setq next-sep next-node)
         (when-let* ((sibling (funcall next-node-func next-node))
                     (sibling-kind (funcall node-kind-func next-sep node-kind sibling)))

           (pcase sibling-kind
             ('sibling
              (setq next-sibling sibling))
             ('term
              (setq next-term sibling)))))
        ('sibling
         (setq next-sibling next-node))
        ('term
         (setq next-term next-node))))
    (list next-sibling next-sep next-term)))


(defun evil-ts-obj-generic-thing-with-sep-outer (node
                                                 node-kind-func node-fetcher
                                                 &optional strict extend-to-term)
  "Create a range for an outer text object that is associated with the `NODE'.
This function is mostly used to create outer text objects for
parameter or statement things. Things may be delimited by a
separator. For detailed description of `NODE-FETCHER' and
`NODE-KIND-FUNC' see `evil-ts-obj-conf-sibling-trav'.

This function works as follows: at first, it tries to find the
next sibling node. If such a node exists, start of this node
defines the outer range end. If `STRICT' is t, then it is
required that sibling is preceded by the separator, otherwise
range is not extended to the next sibling. If there is no next
sibling node, but `EXTEND-TO-TERM' is t and next node has kind
term, then start of term node becomes new end of the outer range.
If there is no next sibling, outer range is extended to the end
of the previous sibling. It does so only if no trailing
separators were found."
  (pcase-let* ((node-range (evil-ts-obj-node-range-create node))
               (`(,next-sibling ,next-sep ,next-term)
                (evil-ts-obj--generic-find-sep-and-sibling
                 node node-kind-func
                 (lambda (n) (funcall node-fetcher 'next n)))))

    (cond ((or (and (not strict) next-sibling)
               (and strict next-sep next-sibling))
           (evil-ts-obj-node-range-change-end node-range :node next-sibling :exclude t))
          ((and extend-to-term next-term)
           (evil-ts-obj-node-range-change-end node-range :node next-term :exclude t))
          (next-sep
           (evil-ts-obj-node-range-change-end node-range :node next-sep)))

    (unless next-sibling
      ;; this is the last thing in a sequence
      ;; determine how to extend to the previous separator
      (pcase-let ((`(,prev-sibling ,prev-sep ,prev-term)
                   (evil-ts-obj--generic-find-sep-and-sibling
                    node node-kind-func
                    (lambda (n) (funcall node-fetcher 'prev n)))))
        (cond ((and (null next-sep) prev-sibling
                    (or (not strict) (and strict prev-sep)))
               ;; There was not trailing separator.
               ;; So we can assume that the previous one should be included.
               (evil-ts-obj-node-range-change-start node-range :node prev-sibling :exclude t))
              ((and extend-to-term prev-term)
               (evil-ts-obj-node-range-change-start node-range :node prev-term :exclude t))
              ((and next-sep prev-sep)
               ;; There was trailing separator.
               ;; Extend to the end of the previous separator.
               (evil-ts-obj-node-range-change-start node-range :node prev-sep :exclude t)))))
    node-range))

(defun evil-ts-obj-thing-with-sep-outer (node seps &optional strict extend-to-term)
  "Create a range for an outer text object that is associated with the `NODE'.
It uses `evil-ts-obj-generic-thing-with-sep-outer'
underneath (examine this function for an explanation of `STRICT'
and `EXTEND-TO-TERM'). It uses `evil-ts-obj--get-node-kind' as
node-kind-func. See this function description for `SEPS'
meaning."

  (evil-ts-obj-generic-thing-with-sep-outer
   node
   (lambda (cn ck n) (evil-ts-obj--get-node-kind cn ck n seps))
   #'evil-ts-obj--get-sibling-simple
   strict
   extend-to-term))


(defun evil-ts-obj-generic-thing-upper (node node-kind-func node-fetcher &optional extend-to-next)
  "Create a range for an upper text object that is associated with `NODE'.
Description of `NODE-FETCHER' and `NODE-KIND-FUNC' see in
`evil-ts-obj-conf-sibling-trav' documentation.

This function works as follows: at first, it determines the end
position of an upper range. If `EXTEND-TO-NEXT' is t, then start
of the next sibling node becomes new end of the upper range. If
there is no next sibling node, and the next node has kind term,
then start of the term node becomes new end of the upper range.
Other possible values of `EXTEND-TO-NEXT': nil - never extend,
symbol if-sep-found - extend only if there is a separator between
current and the sibling node.

Then it iterates over all nodes returned by
the `NODE-FETCHER' in the prev direction. It stops on the first
node, which kind is not either sibling or sep (kind is obtained
from `NODE-KIND-FUNC'). The range start is a start of the last
sibling node returned by the `NODE-FETCHER'."
  (let ((node-range (evil-ts-obj-node-range-create node)))

    (when extend-to-next
      (pcase-let ((`(,next-sibling ,next-sep ,next-term)
          (evil-ts-obj--generic-find-sep-and-sibling
           node node-kind-func
           (lambda (n) (funcall node-fetcher 'next n)))))
        (when (or (eq extend-to-next t)
                  (and (eq extend-to-next 'if-sep-found) next-sep))
          (if next-sibling
              (evil-ts-obj-node-range-change-end node-range :node next-sibling :exclude t)
            (when next-term
              (evil-ts-obj-node-range-change-end node-range :node next-term :exclude t))))))

    (let ((final-sibling node)
          prev
          cur-kind)
      (while (and (setq prev (funcall node-fetcher 'prev node))
                  (setq cur-kind (funcall node-kind-func node cur-kind prev))
                  (setq node prev)
                  (memq cur-kind '(sibling sep)))
        (setq final-sibling node))
      (evil-ts-obj-node-range-change-start node-range :node final-sibling))

    node-range))

(defun evil-ts-obj-param-upper-mod (node seps)
"Create parameter upper text object.
This function invokes `evil-ts-obj-generic-thing-upper' with
extend-to-next set to t. See `evil-ts-obj--get-node-kind' for
information about `NODE' and `SEPS'."
 (evil-ts-obj-generic-thing-upper
   node
   (lambda (cn ck n) (evil-ts-obj--get-node-kind cn ck n seps))
   #'evil-ts-obj--get-sibling-simple
   t))

(defun evil-ts-obj-generic-thing-lower (node node-kind-func node-fetcher &optional extend-to-prev)
  "Create a range for an lower text object that is associated with `NODE'.
Description of `NODE-FETCHER' and `NODE-KIND-FUNC' see in
see in `evil-ts-obj-conf-sibling-trav' documentation.

This function works as follows: at first, it determines the start
position of a lower range. If `EXTEND-TO-PREV' is t, then the end
of the previous sibling node becomes new start of the lower
range. If there is no previous sibling node, and the previous
node has kind term, then end of the term node becomes new start
of the lower range. Other possible values of `EXTEND-TO-PREV':
nil - never extend, symbol if-sep-found - extend only if there is
a separator between current and the sibling node.

Then it iterates over all nodes returned by the `NODE-FETCHER' in
the next direction. It stops on the first node, which kind is not
either sibling or sep (the kind is obtained from
`NODE-KIND-FUNC'). The range end is an end of the last sibling
node returned by the `NODE-FETCHER'."
  (let ((node-range (evil-ts-obj-node-range-create node)))

    (when extend-to-prev
      (pcase-let ((`(,prev-sibling ,prev-sep ,prev-term)
                   (evil-ts-obj--generic-find-sep-and-sibling
                    node node-kind-func
                    (lambda (n) (funcall node-fetcher 'prev n)))))
        (when (or (eq extend-to-prev t)
                  (and (eq extend-to-prev 'if-sep-found) prev-sep))
          (if prev-sibling
              (evil-ts-obj-node-range-change-start node-range :node prev-sibling :exclude t)
            (when prev-term
              (evil-ts-obj-node-range-change-start node-range :node prev-term :exclude t))))))


    (let ((final-sibling node)
          next
          cur-kind)
      (while (and (setq next (funcall node-fetcher 'next node))
                  (setq cur-kind (funcall node-kind-func node cur-kind next))
                  (setq node next)
                  (memq cur-kind '(sibling sep)))
        (setq final-sibling node))
      (evil-ts-obj-node-range-change-end node-range :node final-sibling))
    node-range))

(defun evil-ts-obj-param-lower-mod (node seps)
  "Create parameter lower text object.
This function invokes `evil-ts-obj-generic-thing-lower' with
extend-to-prev set to t. See `evil-ts-obj--get-node-kind' for
information about `NODE' and `SEPS'."
  (evil-ts-obj-generic-thing-lower
   node
   (lambda (cn ck n) (evil-ts-obj--get-node-kind cn ck n seps))
   #'evil-ts-obj--get-sibling-simple
   t))



;;; Aux ranges

(defun evil-ts-obj-last-range ()
  "Return last used range."
  (copy-sequence evil-ts-obj--last-text-obj-range))

(provide 'evil-ts-obj-core)
;;; evil-ts-obj-core.el ends here
