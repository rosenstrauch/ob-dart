;;; ob-dart.el --- Evaluate Dart source blocks in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Milan Zimmermann
;; Maintainer: Milan Zimmermann
;; Created: July 7, 2016
;; Modified: Dec 1, 2022
;; Version: 2.0.0
;; Keywords: languages
;; Homepage: http://github.org/mzimmerm/ob-dart
;; Package-Requires: ((emacs "24.4"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;    - See README.org for features, including features added in Version 2.0.0.
;;
;;  Todo:
;;    - Support variable passing from Babel header :var to code.
;;    - Support :results value also when Babel source block contains main().
;;    - Session support
;;
;;; Requirements:
;; - Dart language installed - An implementation can be downloaded from
;;                             https://www.dartlang.org/downloads/
;; - The dart executable is on the PATH
;; - (Optional) Dart major mode from MELPA
;;
;; Notes:
;;   - Code follows / inspired by these previously supported org-languages,
;;     roughly in this order:
;;     - ob-io.el
;;     - ob-scala.el
;;     - ob-groovy.el
;;     - ob-R.el
;;     - ob-python.el
;;
;;; Code:

(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("dart" . "dart"))
(defvar ob-dart-default-header-args '())

(defvar ob-dart-command "dart"
  "Name of the command to use for executing Dart code.
Windows support is pending.")


(defun ob-dart-variable-assignments (params)
  "Return a list of Dart statements assigning the block's variables.
The assignments are defined in PARAMS."
  (mapcar
   (lambda (pair)
     (format "var %s = %s;"
             (car pair)
             (ob-dart-var-to-dart (cdr pair))))
   (org-babel--get-vars params)))

(defun ob-dart-var-to-dart (var)
  "Convert an elisp value to a Dart variable.
Convert an elisp value, VAR, into a string of Dart source code
specifying a variable of the same value."
  (cond
   ((listp var)
    (concat "[" (mapconcat #'ob-dart-var-to-dart var ", ") "]"))
   ((stringp var)
    (format "\"%s\"" (substring-no-properties var)))
   ((numberp var)
    (number-to-string var))
   ((booleanp var)
    (if var "true" "false"))
   (t (format "%s" var))))

(defun org-babel-execute:dart (body params)
  "Execute a block of Dart code with org-babel.
This function is called by `org-babel-execute-src-block'.

Args:
  BODY   - String - Dart code from org file, between #+begin_src and #+end_src
  PARAMS - List   - Org Babel code block args after #+begin_src, converted
                    to plist.  Some plist values may be multi-valued,
                    for example for the key `:var', (varname varvalue)
                    from Babel args `:var varname=varvalue`,
                    or for the key `:results', (value raw)
                    from Babel args `:results value raw'."
  (message "executing Dart source code block")
  (let* ((ob-dart-command
          (or (cdr (assq :dart params))
              ob-dart-command))
         (vars (ob-dart-variable-assignments params))
         (result-params (nth 2 (org-babel-process-params params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
                     body params
                     vars))
         (result (ob-dart-evaluate
                  nil full-body result-type result-params)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defun ob-dart-evaluate (session body &optional result-type result-params)
  "Evaluate BODY in external Dart process.
If RESULT-TYPE equals `output' then return standard output as a string.
If RESULT-TYPE equals `value' then return the value of the last statement
in BODY as elisp.

Args:
  SESSION       - `val' from Org source block header arg `:session val'.
                  Not supported yet.
  BODY          - String from org file, between #+begin_src and #+end_src
                  - should be named: dart-src
  RESULT-TYPE   - `val' from Org source block header argument `:results vals'.
                  `val' is one of (output|value).
                  It defaults to `value' if neither is found among `vals'.
                  - should be named: results-collection
  RESULT-PARAMS - Symbol likely the `format' type from docs
                  - should be named: results-format."
  (when session (error "Session is not (yet) supported for Dart"))

  ;; Set the name generated-filename='generated-filename=/tmp/dart-RAND.dart'
  (let* ((generated-filename (org-babel-temp-file "dart-")))

    ;; Create 'temp-file' named 'generated-filename',
    ;;   and insert into it the Dart code generated from body.
    (ob-dart-write-dart-file-from body generated-filename)

    ;; Run org-babel-eval
    (let ((raw (org-babel-eval
                (concat ob-dart-command " " generated-filename " " (symbol-name result-type)) "")))
      ;; result-type: both 'value and 'output formats results as table, unless raw is specified
      (org-babel-result-cond result-params
        raw
        (ob-dart-table-or-string raw)))))

(defun ob-dart-write-dart-file-from (body generated-filename)
  "From BODY, create the Dart code to run, and save it in GENERATED-FILENAME."
  (let*
      ;; Split body into parts, and form the `generated-dart-str'
      ((parts (ob-dart-split-body-depending-on-structure body))
       (wrapper (nth 0 parts))        ;; ob-dart-wrapper or "%s"
       (body-part-top (nth 1 parts))  ;; ob-dart-wrapper-imports or "import dart:async"
       (body-part-main (nth 2 parts)) ;; part of body with main code or full body
       (generated-dart-str (ob-dart-generate-dart-full-code-from
                            body-part-top
                            wrapper
                            body-part-main)))
    (with-temp-file
        generated-filename
      (insert generated-dart-str))))

(defun ob-dart-split-body-depending-on-structure (body)
  "Split the passed BODY into BODY-PART-TOP and BODY-PART-MAIN.

Return a list with WRAPPER, the BODY-PART-TOP and BODY-PART-MAIN
which were split from BODY.

The result of the split depends on the BODY structure (presence of main).

The WRAPPER is either the standard ob-dart wrapper, or `%s'
depending on the BODY structure.

Comments:

1. The parts which are split from BLOCK are:
   - BODY-PART-TOP  = imports, top classes, top functions.
                      Never wrapped.
   - BODY-PART-MAIN = if the main exists, the contents of the main,
                      otherwise BLOCK except imports, top classes,
                      top functions.  Caller wrap it in WRAPPER,
                      or it becomes the WRAPPER.

2. We assume that any supported structure of the Org Babel Dart source block
can be split into 2 parts, then processed with a common wrapper
to generate the Dart source that is executed.

3. The WRAPPER contains format symbols as %s, %w, %a.
Currently only BODY-PART-MAIN is inserted.

4. If main function exists in BODY:
    - the BODY effectively becomes the generated Dart code
      by returning  `%s' in WRAPPER
    - only  `:results output` is supported
   Else main function does not exist in BODY:
    -  WRAPPER is either the standard ob-dart wrapper
    - `:results output' and `:results value' are supported"

  (let* (
         (main-function-line-index (string-match "^ *void *main *\\(\\([A-Za-z0-9_<>]*\\)\\)? *\\(.*\\)" body)))

    (if main-function-line-index
        ;; If main function exists in body,
        ;;   ob-dart uses this Org Babel source block unchanged,
        ;;   with imports and all code.
        ;; Only :results output are supported in this branch.
        ;; Dart code is  \"import 'dart:async';\n\", appended with body.
        ;; The wrapper becomes \"%s\", effectively becomes the body.

        (list "%s" "import 'dart:async';\n" body)
      ;; main function NOT in body, passed wrapper MUST be ob-dart-wrapper
      (list ob-dart-wrapper ob-dart-wrapper-imports body))))

(defun ob-dart-generate-dart-full-code-from (body-part-top wrapper body-part-main)
  "Create and return full Dart code as string from Org Babel source block.

The created string starts with unchanged string BODY-PART-TOP,
appended with WRAPPER string which contains format symbols as %s, %w, %a.
The WRAPPER is inserted with contents of format-specs at appropriate symbols,
and BODY-PART-MAIN at WRAPPER's symbol %s.

The wrapper can be just \"%s\" then the BODY-PART-MAIN
becomes the WRAPPER, and is appended after BODY-PART-TOP.

Comments:

This method functions the same irrespective whether the
full source block body contained the Dart `main()' method or not.

Assumes the passed BODY-PART-TOP and BODY-PART-MAIN were extracted
from the Org Babel Dart full source block,

The logic of splitting the  source block body into
BODY-PART-TOP and BODY-PART-MAIN depends on whether the
source block body contained the Dart `main()' method or not
is not part of this method."
  (concat
   body-part-top
   (format-spec
    wrapper
    `((?a . "async ") (?w . "await ") (?s . ,body-part-main))
    nil
    nil)))

(defvar ob-dart-wrapper-imports
  "
import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:core';

import 'dart:io';
import 'dart:isolate';

import 'dart:math';
import 'dart:mirrors';
"
  "Documentation: Variable which returns Dart wrapper imports as String.

Created from wrapper-imports.drap.

Used when the Dart snippet in Org source block
does NOT contain a `main' method.  See ob-dart-wrapper."
  )

(defvar ob-dart-wrapper
  "
/// Helper class allows to run Dart code in Org Babel source block.
/// This is a test version which is eventually embedded in elisp ob-dart.el
/// as `ob-dart-wrapper`.
class Gen {
  /// Wrapped code from the org file:
  ///   if org file has NO MAIN, code is from between #+begin_src and #+end_src.
  ///   if org file has MAIN,    code is copied from main.
  /// Either way, async is added if the code block contains await.
  /// That is theory: for files with MAIN, we add async here if the main is async.
  runBlock(Map<String, dynamic> vars) %a {
    //   - Org code block from begin_src .. end_src inserted here by elisp format.
    //   - See `ob-dart-wrapper` and `format-spec` in wrap-body.esh and ob-dart.el
    %s
  }

  /// Run the potentially async block asynchronously, and mark this method async,
  ///   for caller to wait at the point we need the result - just before the [print]
  ///   in the flow.
  ///
  /// See the [runBlockResultsValue] for description how the async propagation
  /// and await-ing result just before print.
  runBlockResultsOutput(Map<String, dynamic> vars) %a {
    runBlock(vars);
  }
  /// Runs the BEGIN_SRC .. END_SRC source block.
  ///
  /// Uses [ZoneSpecification] to skip the print to stdout,
  /// then return the value of the last expression
  /// of the source block, which MUST have an explicit 'return' such as:
  ///
  ///   `return returnedValue;`
  ///
  /// Should be invoked in the branch of
  ///    'results_collection_type == 'value'',
  /// where a [returnedValue.toString()] is called.
  ///
  /// The [returnedValue.toString] result pops up in elisp code
  /// in the 'ob-dart.el' function
  ///    'ob-dart-evaluate'.
  /// This function parses and manipulates the [returnedValue.toString] returned from here.
  ///
  /// The elisp parsing and manipulation of the [returnedValue.toString] in
  /// the 'ob-dart-evaluate' depends on the parameters passed in
  ///    `:results [output|value] [raw]`
  /// for example, for:
  ///   - [value raw] there is no parsing or manipulation of the result.
  ///   - [value]     converts the [returnedValue.toString] to table if the string
  ///                 looks like a table (grouped using () or {} or [], delimited by comma),
  ///                 otherwise behaves as [value raw].
  ///   - [output raw] and [output] just return the [returnedValue.toString]
  /// So, most situations, the [returnedValue.toString] shows up in
  /// the org RESULTS block.
  ///
  ///
  /// Note: ASYNC DECLARATION IS NOT REQUIRED TO BE PROPAGATED AND MARKED ON CALLER.
  ///         BUT AWAIT CALL MUST BE MARKED ASYNC ON CALLER.
  ///
  ///       In other words, a call to async method:
  ///           1. 'await-marked     async method call MUST mark caller async'
  ///           2. 'await-non-marked async method call has CHOICE to mark caller async'
  ///
  ///   What does this note mean:
  ///         Calling ASYNC `method` inside `methodCaller`, ONLY requires
  ///       to declare
  ///            `methodCaller async {...}`
  ///        if we use await on method call:
  ///             `await method();`
  ///
  ///        - If caller calls `await method()`, flow waits at this point.
  ///        - Otherwise, caller calls `method():`
  ///             the flow continues TO CALLERS, WHO NEVER AGAIN
  ///             ARE REQUIRED to AWAIT OR DECLARE ASYNC.
  ///             THE DISADVANTAGE OF THE 'Otherwise' IS THE
  ///             FLOW CONTINUES, AND THE 'AWAIT' MAY NOT FINISH UNTIL
  ///             AFTER THE PROGRAM FINISHES. THAT MEANS, WHATEVER
  ///             RESULT WAS EXPECTED FROM THE LOWEST LEVEL ASYNC FUNCTION,
  ///             WILL BE NULL.
  ///
  ///  Continue flow after the call to async `runBlock`,
  ///  without wait to caller(s).
  ///
  ///   The [runBlock] runs async,
  ///  but BEFORE WE PRINT IN CALLER, this thread WAITs, making async to resolve
  ///  the future [runBlock] returnedValue BACK INTO this FLOW (THREAD) before print.
  runBlockResultsValue(List<String> args) %a {
    var returnedValue;
    /// Runs it's [body], the function in the first argument,
    /// in a new [Zone], based on [ZoneSpecification].
    ///
    /// The [ZoneSpecification] is defined such that any [print] statement
    /// in the [body] is ignored.
    runZoned(() {
      // If we used 'await runBlock()', we would also be forced to 'runZoned(() async {'.
      // Even if this code did not propagate the above async up to runBlockResultsValue(),
      // or further, the 'await runBlock()' causes to wait here,
      // but the code flow continue,
      // This means, that the async operation immediately reaches the caller
      //    print('${ Gen().runBlockResultsValue()}');
      // the [returnedValue] is not copied from it's Future,
      // by the time of print, so the print would output [null]
      // rather then the [returnedValue].
      returnedValue = runBlock(args);
    }, zoneSpecification:
        ZoneSpecification(print: (self, parent, zone, message) {
      // Ignore argument message passed to print.
      // This causes any print invocations inside runZoned
      // to do nothing.  This achieves the goals of 'result: value'
      // not printing anything to stdout
    }));

    // Return the returnedValue from 'runBlock()' .
    return returnedValue;
  }
}

void main(List<String> args) %a {
  var results_collection_type = null;
  if (args.length > 0) {
    results_collection_type = args[0];
  }

  if (results_collection_type == 'output') {
    // For [:results output rest], [runBlock] runs non-zoned,
    // all [print] methods execute.
    %w Gen().runBlockResultsOutput(args);
  } else if (results_collection_type == 'value') {
    // For [:results value rest] [runBlock] runs in the print-avoid zone.
    // This ignores all [print] in [runBlock].
    // The result is passed to [print] below, which is already out of
    // the zone, and prints [runBlockResultsValue] converted [toString].
    print('${ %w Gen().runBlockResultsValue(args)}');
  } else {
    throw Exception(
        'Invalid collection type in results: ${results_collection_type}. Only one of [output/value] allowed.');
  }
}
"
  "Documentation: Variable which returns Dart wrapper code as String.

Created from wrapper.drap

Used when the Dart snippet in Org source block
does NOT contain a `main' method.  The returned Dart code wraps Dart
source between #+begin_src and #+end_src, into a Dart main() method.

If the passed argument to

  `org-babel-eval dart-code argument'

is:
  - `output':
    - The stdout from Dart print() in the snippet is send
      to the standard output, and becomes the #+RESULT.
  - `value':
    - The stdout from Dart print() is blocked by the `runZoned'
      method, and the `toString()' of the last `return lastExpression'
      becomes the  #+RESULT.")


(defun ob-dart-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.

The default core implementation `org-babel-script-escape' behaves as follows:

If RESULTS look like a table (grouped using () or {} or [] and
delimited by commas), then convert them into an Emacs-lisp
table (list of lists),

Otherwise, return the results unchanged as a string.

Args:
  RESULTS - String - String resulting from Dart invocation, and printed to stdio
                     by stdout.write() or print()"
  (org-babel-script-escape results))


(defun org-babel-prep-session:dart (_session _params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Session is not (yet) supported for Dart"))

(defun ob-dart-initiate-session (&optional _session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session.  Sessions are not supported in Dart."
  nil)

(provide 'ob-dart)

;;; ob-dart.el ends here
