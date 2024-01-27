;;; evil-ts-obj-conf.el --- Conf utils -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
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

(eval-when-compile
  (require 'cl-lib))




;;; Customs
(defgroup evil-ts-obj nil
  "Provide evil text-objects using tree-sitter."
  :group 'tools)

(defcustom evil-ts-obj-compound-thing-key "e"
  "Default key binding for compound text objects."
  :type 'string)

(defcustom evil-ts-obj-statement-thing-key "s"
  "Default key binding for statement text objects."
  :type 'string)

(defcustom evil-ts-obj-param-thing-key "a"
  "Default key binding for param text objects."
  :type 'string)

(defcustom evil-ts-obj-navigation-keys-prefix
  '((beginning-of . "(")
    (end-of . ")")
    (previous . "[")
    (next . "]")
    (previous-largest . "{")
    (next-largest . "}"))
  "Default bindings for movement commands."
  :type 'alist)


;;; Variables

(defvar-local evil-ts-obj-conf-thing-modifiers nil
  "Plist that holds an extension function for each language in the buffer.
A function returns a range (a list of two integers) of a text
object or nil. If nil is returned than default range is
calculated (see `evil-ts-obj--default-range'). The function
should accept two arguments: SPEC and NODE.

SPEC is a plist that contains a context for a current text
object. The possible fields are :thing, :mod, :act, :command and
:visual. :thing should contain the current thing that is
represented by the treesit NODE. :mod contains the current
modifier (inner/outer etc.). :act is a type of a current
operation with the text object. :act can be one of the following
symbols:

* op - any evil operator or other modification operations like
  avy actions.

* nav - any movement command like evil-ts-obj-next-thing. Also it
is used when selecting a text object for preview (e.g. when
collecting avy candidates)

:command contains that current command that will operate on
returned range. :visual is set to t if `evil-visual-state-p'
returns t, when spec is created. Also see
`evil-ts-obj--make-spec' and `evil-ts-obj--apply-modifiers'.")

(defvar-local evil-ts-obj-conf-range-finalizers nil
  "Finalizer function for each language.
Finalizer function should adjust text-object in language
indenpendent way. It accepts SPEC and RANGE parameters.
`evil-ts-obj--finalize-text-obj-range' is used by default.")

(defvar-local evil-ts-obj-conf-nav-things nil
  "This plist defines default things for movement for each language.
It can be single thing, e.g. compound or a list of things: \(or
param compound thing\). When list is provided navigating command
will search for the each thing from this list until matching
thing is found.")

(defvar-local evil-ts-obj-conf-sibling-trav nil
  "Plist that maps language to a plist that used for traversing things.
Nested plists are mappings from thing symbol to corresponding
traverse structures `evil-ts-obj-trav'.

For example:
\\=`(python
  (param ,(evil-ts-obj-trav-create
           :seps evil-ts-obj-python-param-seps-regex
           :fetcher #\\='evil-ts-obj--get-sibling-simple
           :kind-func #\\='evil-ts-obj--get-node-kind))))

`evil-ts-obj-trav-create' expects regexp as :seps keyword, to
match separators that delimit sibling things.

:fetcher is a function that accepts two arguments:
direction (next or prev) and node. It should return the immediate
sibling of the passed node. For example, see
`evil-ts-obj--get-sibling-simple'.

:kind-func is a function that accepts three arguments: current
node, current node kind, target node and optionally separators
regexp. It should classify the passed target node and return its
kind: sep, sibling, term or nil. sep - means that passed node is
a recognized separator. sibling - denotes named node that is a
sibling of the current node. term - signals that this node is a
terminator of a sequence of things. When nil returned, act as if
current-node is the last node in the sequence. See
`evil-ts-obj--get-node-kind' or
`evil-ts-obj--get-node-kind-strict'.")

(defvar-local evil-ts-obj-conf-sep-regexps nil
  "Plist that holds separator regexps for each language in the buffer.
Some outer text objects may extend to the nearest separator.
Special handling of separators is needed since they usually are
siblings to the node that represent a thing. Also there is
special behavior when two things are separated and the point is
on a separator. We prefer the previous thing in this case. Should
be set for each language in the appropriate file.")

(defvar-local evil-ts-obj-conf-raise-rules nil
  "This is a plist that maps language to a function that returns raise rules.
This function is invoked by `evil-ts-obj-edit--raise-operator'
and `evil-ts-obj-edit--raise-dwim' to determine what things they
should operate on. The function should accept RANGE-TYPE and
optionally TEXT-SPEC. RANGE-TYPE can be either text or place. If
RANGE-TYPE is text then function should return a text object,
content of which will be used in raise operator. If RANGE-TYPE is
place then function should return a text object that should be
replaced by the selected content. In both cases it should return
alist, for example \\='((statement . inner) (compound . outer)).
So it is possible to specify multiple potential text objects.
When the RANGE-TYPE is place, TEXT-SPEC stores specification of
previously selected text object. See
`evil-ts-obj-def-raise-rules' as an example of this function
implementation.")

(defvar-local evil-ts-obj-conf-drag-rules nil
  "This is a plist that maps language to a function that returns drag rules.
This function is invoked by `evil-ts-obj-edit--drag'to determine
what things it should operate on. The function should accept
RANGE-TYPE and optionally FIRST-SPEC. RANGE-TYPE is a symbol that
value is either first or second. If RANGE-TYPE is first then
function should return text object, which will be dragged. If
RANGE-TYPE is second then it returns the object that should be
swapped with the dragged one. The second object is searched
across the first object siblings. In both cases it should return
alist, for example \\='((statement . inner) (compound . outer)).
So it is possible to specify multiple potential text objects.
When the RANGE-TYPE is second, FIRST-SPEC stores specification of
previously selected text object. See `evil-ts-obj-def-drag-rules'
as an example of this function implementation.")

(defvar-local evil-ts-obj-conf-clone-rules nil
  "This is a plist that maps language to a function that returns clone DWIM rules.
This function is invoked by `evil-ts-obj-edit--clone-dwim-impl'to
determine what things it should operate on. The function should
accept RANGE-TYPE and optionally TEXT-SPEC. RANGE-TYPE is a
symbol that value is either text or place. If RANGE-TYPE is text
then function should return a text object, content of which will
be cloned. If RANGE-TYPE is place then function should return a
text object that denotes position to insert the selected content.
If clone-before then start of text object is the insert position,
if clone-after then text is inserted at the end of the range. In
both cases it should return alist, for example \\='((statement .
inner) (compound . outer)). So it is possible to specify multiple
potential text objects. When the RANGE-TYPE is place, TEXT-SPEC
stores specification of previously selected text object. See
`evil-ts-obj-def-clone-rules' as an example of this function
implementation.")

(defvar-local evil-ts-obj-conf-clone-indent-policy nil
  "Map a language to a symbol that determines clone policy on newlines indentation.
If its value is nil, then `indent-according-to-mode' function is
used. If value is symbol cur-indent, then indentation is equal to
the current indentation at the start of a place range. If its
value is the symbol column, then column number of the start of
the place range is used as an indentation level.")

(defvar-local evil-ts-obj-conf-extract-rules nil
  "This variable maps language to a function that returns extract DWIM rules.
This function is invoked by
`evil-ts-obj-edit--extract-operator-impl' and
`evil-ts-obj-edit--extract-dwim-impl' to determine what things
they should operate on. The function should accept RANGE-TYPE and
optionally TEXT-SPEC. RANGE-TYPE is a symbol that value is either
text or place. If RANGE-TYPE is text then function should return
a text object, content of which will be extracted. If RANGE-TYPE
is place then function should return a text object that denotes
position to insert the selected content. If extract-up then start
of text object is the insert position, if extract-down then text
is inserted at the end of the range. In both cases it should
return alist, for example \\='((statement . inner) (compound .
outer)). So it is possible to specify multiple potential text
objects. When the RANGE-TYPE is place, TEXT-SPEC stores
specification of previously selected text object. See
`evil-ts-obj-def-extract-rules' and
`evil-ts-obj-def-conf-lang-extract-rules' as examples of this
function implementation.")

(defvar-local evil-ts-obj-conf-statement-placeholder nil
  "List of placeholder statements for each language.
This variable is used in extract/inject operators. One of these
statements is inserted inside compound statement, in case it
becomes empty after the extract operation.")

(defvar-local evil-ts-obj-conf-compound-brackets nil
  "Compound brackets for each language.
If language uses some kind of bracktet to combine statements,
then their values shoulb set in this variable. They are set as a
string, for example \"{}\". This variable is used by
extract/inject operators. Purpose is the same as for
`evil-ts-obj-conf-statement-placeholder'.")

(defvar-local evil-ts-obj-conf-inject-rules nil
  "This variable maps language to a function that returns inject DWIM rules.
This function is invoked by
`evil-ts-obj-edit--inject-operator-impl' and
`evil-ts-obj-edit--inject-dwim-impl' to determine what things
they should operate on. The function should accept RANGE-TYPE and
optionally TEXT-SPEC. RANGE-TYPE is a symbol that value is either
text or place. If RANGE-TYPE is text then function should return
a text object, content of which will be injected into new place.
If RANGE-TYPE is place then function should return a text object,
to which text will be injected. Usually, compound inner text
objects is used as a place for injection. If inject-up then end
of text object is the insert position, if inject-down then text
is inserted at the start of the range. In both cases it should
return alist, for example \\='((statement . inner) (compound .
outer)). So it is possible to specify multiple potential text
objects. When the RANGE-TYPE is place, TEXT-SPEC stores
specification of previously selected text object. See
`evil-ts-obj-def-inject-rules' and
`evil-ts-obj-def-conf-lang-inject-rules' as examples of this
function implementation.")

(defvar-local evil-ts-obj-conf-slurp-rules nil
  "This variable maps language to a function that returns slurp rules.
This function is invoked by `evil-ts-obj-edit--slurp' to
determine what text objects it should operate on. It should
return alist, for example \\='((statement . inner) (compound .
outer)). So it is possible to specify multiple potential text
objects. The function should accept RANGE-TYPE. RANGE-TYPE is a
symbol that value is either text or place. Place text object
defines two aspects. Treesit node that represents text object:
siblings of that node will be \"slurped\". Start and end of a
text object determines the insert positions for slurped siblings.
Concrete insert position depends on the point position. Usually,
compound inner text objects is used as a place text object. If
RANGE-TYPE is text then function should return a text object,
content of which will be injected into new place. See
`evil-ts-obj-def-slurp-rules' and
`evil-ts-obj-def-conf-lang-slurp-rules' as examples of this
function implementation.")

(defvar-local evil-ts-obj-conf-barf-rules nil
  "This variable maps language to a function that returns barf rules.
This function is invoked by `evil-ts-obj-edit--barf' to determine
what text objects it should operate on. It should return alist,
for example \\='((statement . inner) (compound . outer)). So it
is possible to specify multiple potential text objects. The
function should accept RANGE-TYPE. RANGE-TYPE is a symbol that
value is either text or place. Place text object defines two
aspects. Start and end of a text object determines the insert
positions for barfed siblings. Concrete insert position depends
on the point position. Inner text object is used to find text
objects that will be \"barfed\". Usually, compound outer text
objects is used as a place text object. If RANGE-TYPE is text
then function should return a text object, content of which will
be extracted from the place place. See
`evil-ts-obj-def-barf-rules' and
`evil-ts-obj-def-conf-lang-barf-rules' as examples of this
function implementation.")

(defvar evil-ts-obj-conf-dont-extend-to-next-cmds '(evil-ts-obj-raise
                                                    evil-ts-obj-inject-down
                                                    evil-ts-obj-inject-up)
  "Do not extend upper/lower text object range for specified commands.
Upper/lower modifiers usually extend text object range to the
next/previous sibling. It is not desirable behavior for some
commands. This variable holds a list of commands, for which this
extending is disabled.")

;;; Helper functions

(defun evil-ts-obj-conf--make-nodes-regex (nodes)
  "Create regex from NODES."
  (format "^%s$" (regexp-opt nodes)))

(defun evil-ts-obj-conf-nodes-setter (sym val)
  "Setter for use with `evil-ts-obj-<lang>-<thing>[suffix]?-nodes` defcustoms.
It sets SYM to VAL and updates variable that holds regex built
from node names."
  (set-default sym val)
  (if-let* ((sym-name (symbol-name sym))
            ((string-match "evil-ts-obj-\\([a-z]+\\)-\\([a-z]+\\)\\([-a-z]*\\)-nodes" sym-name))
            (lang (match-string 1 sym-name))
            (thing (match-string 2 sym-name))
            (suffix (match-string 3 sym-name))
            (sym (intern (format "evil-ts-obj-%s-%s%s-regex" lang thing suffix))))
      (if (boundp sym)
          (set sym (evil-ts-obj-conf--make-nodes-regex val))
        (user-error "Variable %s is not defined!" sym))
    (user-error "Unknown pattern for a nodes symbol %s" (symbol-name sym))))

(defun evil-ts-obj-conf-seps-setter (sym val)
  "Setter for use with `evil-ts-obj-<lang>-<thing>-seps` defcustoms.
It sets SYM to VAL and updates variable that holds regex built
from node names. It also updates the variable
evil-ts-obj-<lang>-all-seps-regex if it is bound."
  (set-default sym val)
  (if-let* ((sym-name (symbol-name sym))
            ((string-match "evil-ts-obj-\\([a-z]+\\)-\\([a-z]+\\)-seps" sym-name))
            (lang (match-string 1 sym-name))
            (thing (match-string 2 sym-name))
            (sym (intern (format "evil-ts-obj-%s-%s-seps-regex" lang thing)))
            (all-sym (intern (format "evil-ts-obj-%s-all-seps-regex" lang)))
            (val (or (and (listp val) val)
                     (list val))))
      (progn
        (if (boundp sym)
            (set sym (evil-ts-obj-conf--make-nodes-regex val))
          (user-error "Variable %s is not defined!" sym))
        (when (boundp all-sym)
          (let ((all-seps nil))
            (dolist (thing '(statement param))
              (when-let* ((sym-other (intern (format "evil-ts-obj-%s-%s-seps" lang thing)))
                         ((boundp sym-other))
                         (val (ensure-list (symbol-value sym-other))))
                (setq all-seps (append all-seps val))))
            (set all-sym (evil-ts-obj-conf--make-nodes-regex all-seps)))))

    (user-error "Unknown pattern for a nodes symbol %s" (symbol-name sym))))


(provide 'evil-ts-obj-conf)
;;; evil-ts-obj-conf.el ends here
