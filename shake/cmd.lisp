;;; Console command system

(in-package #:shake)

(defstruct (console (:constructor %make-console))
  (text (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t))
  ;; TODO: Set maximum number of stored lines.
  (reversed-lines nil)
  (active-p nil :type boolean)
  (display-from-line 0 :type fixnum))

(defun console-print (console control-string &rest format-arguments)
  (check-type console console)
  (let ((string (apply #'format nil control-string format-arguments))
        (start-pos 0))
    (loop for c across string and end-pos from 1
       when (or (= end-pos (length string)) (char= #\Newline c))
       do (push (subseq string start-pos (if (char= #\Newline c)
                                             (1- end-pos)
                                             end-pos))
                (console-reversed-lines console))
         (setf start-pos end-pos)))
  ;; Return an empty result as this can be run through GUI console which
  ;; prints the resulting values of evaluated commands.
  nil)

(defun printf (control-string &rest format-arguments)
  (declare (special *console*))
  ;; Log to stdout
  (apply #'format t control-string format-arguments)
  ;; Echo to our console
  (when (boundp '*console*)
    (check-type *console* console)
    (apply #'console-print *console* control-string format-arguments)))

(defun print-error (control-string &rest format-arguments)
  (declare (special *last-printed-error*))
  (let ((msg (apply #'format nil control-string format-arguments)))
    (prog1
        (when (or (not *last-printed-error*) (string/= msg *last-printed-error*))
          (printf "ERROR: ~A" msg))
      (setf *last-printed-error* msg))))

(defun print-warning (control-string &rest format-arguments)
  (declare (special *last-printed-warning*))
  (let ((msg (apply #'format nil control-string format-arguments)))
    (prog1
        (when (or (not *last-printed-warning*) (string/= msg *last-printed-warning*))
          (printf "WARNING: ~A" msg))
      (setf *last-printed-warning* msg))))

(defun console-clear (console)
  (check-type console console)
  (setf (console-reversed-lines console) nil)
  (setf (console-display-from-line console) 0))

(defun console-append (console text)
  (check-type console console)
  (check-type text (or string character))
  (if (characterp text)
      (vector-push-extend text (console-text console))
      (loop for c across text do (console-append console c))))

;; TODO: This should be in shake-utils
(defun package-shortest-name (package)
  "Get the shortest name or nickname for PACKAGE. If PACKAGE has no name, returns NIL."
  (check-type package package)
  (when-let ((name (package-name package)))
    (dolist (nick (package-nicknames package) name)
      (when (< (length nick) (length name))
        (setf name nick)))))

;; TODO: This should be in shake-utils
(defun symbol-exported-p (symbol)
  (check-type symbol symbol)
  (do-external-symbols (exported-sym (symbol-package symbol))
    (when (eq exported-sym symbol)
      (return-from symbol-exported-p t))))

(defun console-commit (console)
  (printf "> ~A~%" (console-text console))
  (setf (console-display-from-line console) 0)
  (let ((form (handler-case
                  (read-command-from-string (console-text console))
                (error (e) (print-error "~A~%" e)))))
    (setf (fill-pointer (console-text console)) 0)
    (when form
      (let ((res (handler-case (command-eval-form form)
                   (error (e) (print-error "~A~%" e)))))
        (when res (printf "~S~%" res))))))

(defvar *console-font-size* :small
  "Size of console font, may be :SMALL, :MEDIUM or :LARGE")

(defun make-console ()
  (let ((console (%make-console)))
    (add-variable '*console-font-size* :type '(member :small :medium :large))
    (add-command 'quit #'sdl2:push-quit-event)
    (add-command 'exit #'sdl2:push-quit-event)
    (add-command 'echo (lambda (&rest args)
                         (printf "~{~A~^ ~}~%" args)))
    (add-command '+ #'+)
    (add-command '- #'-)
    (add-command '* #'*)
    (add-command '/ #'/)
    (add-command 'clear (lambda () (console-clear console)))
    (add-command 'exec (lambda (filename)
                         (with-data-file (file filename)
                           (printf "Execing ~A...~%" filename)
                           (loop for line = (read-line file nil :eof)
                                 until (eq line :eof) do
                                   (command-eval-form (read-command-from-string line)))
                           (printf "Finished exec of ~A~%" filename))))
    console))


(defun console-backspace (console)
  (if (/= 0 (length (console-text console)))
      (vector-pop (console-text console))))

;; TODO: This depends on shake.render, we should probably load it after that.
(defun console-draw (console render-system)
  (check-type console console)
  (check-type *console-font-size* (member :small :medium :large))
  ;; TODO: Use virtual screen height
  (let ((win-height (srend:render-system-rend-height render-system))
        (scale (ecase *console-font-size* (:small 1) (:medium 2) (:large 4))))
    (let ((start-y (+ (floor win-height 2) (* 16 scale))))
      (when (/= 0 (console-display-from-line console))
        (srend:draw-gui-text "^ ^ ^ SCROLLBACK ^ ^ ^" :x 2 :y start-y)
        (incf start-y (* 16 scale)))
      (loop for line in (nthcdr (console-display-from-line console)
                                (console-reversed-lines console))
            and y from start-y to win-height by (* 16 scale) do
              (srend:draw-gui-text line :x 2 :y y :scale scale)))
    (srend:draw-gui-text (format nil "> ~A" (console-text console))
                         :x 2 :y (floor win-height 2) :scale scale)))

(defun console-handle-keydown (console keysym)
  (assert (console-active-p console))
  (cond
    ((and (eq :scancode-l (sdl2:scancode keysym))
          (sdl2:mod-value-p (sdl2:mod-value keysym) :lctrl :rctrl))
     (console-clear console))
    ((eq :scancode-return (sdl2:scancode keysym))
     (console-commit console))
    ((eq :scancode-backspace (sdl2:scancode keysym))
     (console-backspace console))
    ((eq :scancode-pageup (sdl2:scancode keysym))
     (when (console-reversed-lines console)
       (zap (lambda (line)
              (min (+ line 2) (1- (list-length (console-reversed-lines console)))))
            (console-display-from-line console))))
    ((eq :scancode-pagedown (sdl2:scancode keysym))
     (zap (lambda (line) (max 0 (- line 2)))
          (console-display-from-line console)))
    ((and (eq :scancode-home (sdl2:scancode keysym))
          (sdl2:mod-value-p (sdl2:mod-value keysym) :lctrl :rctrl))
     (when (console-reversed-lines console)
       (setf (console-display-from-line console)
             (1- (list-length (console-reversed-lines console))))))
    ((and (eq :scancode-end (sdl2:scancode keysym))
          (sdl2:mod-value-p (sdl2:mod-value keysym) :lctrl :rctrl))
     (setf (console-display-from-line console) 0))))

(defstruct cmd
  (symbol nil :type symbol :read-only t)
  (function nil :type function :read-only t)
  (setter nil :type (or null function) :read-only t))

(defun cmd-documentation (cmd)
  (check-type cmd cmd)
  (documentation (cmd-function cmd) 'function))

(defun call-with-cmd-system (function)
  (let ((*commands* nil))
    (declare (special *commands*))
    (funcall function)))

(defmacro with-cmd-system (&body body)
  `(call-with-cmd-system (lambda () ,@body)))

(defun add-command (symbol function)
  (declare (special *commands*))
  (check-type *commands* list)
  (check-type symbol symbol)
  (check-type function function)
  (when (find symbol *commands* :key #'cmd-symbol)
    (print-warning "redefining command '~S'~%" symbol))
  (push (make-cmd :symbol symbol :function function) *commands*))

(defun add-variable (symbol &key getter setter (type t) documentation)
  (declare (special *commands*))
  (check-type *commands* list)
  (check-type symbol (and (not null) symbol))
  (check-type getter (or null function))
  (check-type setter (or null function))
  (check-type documentation (or null string))
  (when (find symbol *commands* :key #'cmd-symbol)
    (print-warning "redefining command '~S'~%" symbol))
  (let ((getter (or getter (lambda () (symbol-value symbol)))))
    (when (and (boundp symbol) (documentation symbol 'variable))
      (setf (documentation getter 'function) (documentation symbol 'variable)))
    (when documentation
      (setf (documentation getter 'function) documentation))
    (push (make-cmd :symbol symbol
                    :function getter
                    :setter (or setter (lambda (val)
                                         (unless (typep val type)
                                           (error 'type-error :datum val :expected-type type))
                                         (setf (symbol-value symbol) val))))
          *commands*)))

(defun find-command (symbol)
  (declare (special *commands*))
  (check-type *commands* list)
  (check-type symbol symbol)
  (find symbol *commands* :key #'cmd-symbol))

(defun read-command-from-string (string)
  "Return a form parsed from STRING."
  (check-type string string)
  (let ((form (let ((*package* (find-package :shake))
                    (*read-eval* nil)
                    (*readtable* (copy-readtable)))
                (set-macro-character #\# nil)
                (set-macro-character #\| nil)
                ;; Read all of the string so as to allow forms without top
                ;; level parentheses. E.g. "echo arg1 arg2" will be valid
                ;; as if "(echo arg1 arg2)".
                (loop for (form pos) = (multiple-value-list
                                        (read-from-string string nil :eof))
                        then (multiple-value-list
                              (read-from-string string nil :eof :start pos))
                      until (eq :eof form) collect form))))
    (unless (cdr form) (setf form (car form)))
    form))

(defun command-eval-form (form)
  (declare (special *commands*))
  (check-type *commands* list)
  (flet ((apply-command (cmd-sym &optional args)
           (check-type cmd-sym symbol)
           (if-let ((cmd (find-command cmd-sym)))
             (apply (cmd-function cmd) (mapcar #'command-eval-form args))
             (error "unkown command '~A'" cmd-sym)))
         (set-command (cmd-sym &rest vals)
           (check-type cmd-sym symbol)
           (if-let ((cmd (find-command cmd-sym)))
             (if (cmd-setter cmd)
                 (apply (cmd-setter cmd) (mapcar #'command-eval-form vals))
                 (error "'~A' is not modifiable" cmd-sym))
             (error "unknown variable '~A'" cmd-sym)))
         (help-command (&optional cmd-sym)
           (if cmd-sym
               (if-let ((cmd (find-command cmd-sym)))
                 (printf "~A" (cmd-documentation cmd)))
               (let ((*package* (find-package :shake)))
                 (flet ((cmd< (cmd1 cmd2)
                          (let ((pkg1 (symbol-package cmd1))
                                (pkg2 (symbol-package cmd2)))
                            (if (eq pkg1 pkg2)
                                (string< cmd1 cmd2)
                                (string< (package-shortest-name pkg1)
                                         (package-shortest-name pkg2))))))
                   (dolist (cmd (stable-sort (mapcar #'cmd-symbol *commands*) #'cmd<))
                     (let ((cmd-package (symbol-package cmd))
                           (cmd-exported-p (symbol-exported-p cmd)))
                       (cond
                         ((or (eq *package* cmd-package)
                              (and cmd-exported-p
                                   (member cmd-package (package-use-list *package*))))
                          (printf "~A~%" (symbol-name cmd)))
                         (cmd-exported-p
                          (printf "~A:~A~%" (package-shortest-name cmd-package) (symbol-name cmd)))
                         (t
                          (printf "~A::~A~%" (package-shortest-name cmd-package) (symbol-name cmd)))))))))))
    (cond
      ((consp form)
       (case (car form)
         (begin (dolist (subform (cdr form))
                  (command-eval-form subform)))
         (quote (cadr form))
         (help (apply #'help-command (cdr form)))
         (set (apply #'set-command (cdr form)))
         (t
          (apply-command (car form) (cdr form)))))
      ((atom form)
       (cond
         ((or (keywordp form) (stringp form) (numberp form)
              (null form) (eq t form))
          form)
         ((eq 'help form) (help-command))
         ;; Treat other symbols as single argument command.
         (t (apply-command form)))))))

(defmacro command-progn ((&key (errorp nil)) &body body)
  (let ((forms `(progn ,@(mapcar (lambda (form) `(command-eval-form ',form)) body))))
    (if errorp
        forms
        `(handler-case ,forms
           (error (e) (print-error "~A~%" e))))))
