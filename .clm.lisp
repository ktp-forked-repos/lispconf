(in-package :clim-user)
(setq 
  *print-pretty* t
  *print-escape* nil
  *print-circle* nil
  *print-right-margin* 110 
  *read-default-float-format* 'double-float
  *readtable* (copy-readtable nil))
;;*break-on-signals* nil)

(if (not (member :rune-is-character *features*))
  (push :rune-is-character *features*))


(defun dohash (table)
  (let (result)
    (maphash (lambda (k v) (push (list k v) result)) table)
    (nreverse result)))

(defun nil-as-list () 
  (set-pprint-dispatch
    '(eql nil)
    (lambda (srm el)
      (cond ((null (cdr el))
              (format srm "()"))
	(t
	  (pprint-fill srm el t))))
    2))

(defun remove-nil-as-list ()
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::entries)))
    (dolist (x dispatch-table)
      (cond
	((equal '(eql ()) (slot-value x 'sb-pretty::type))
	  (setf (slot-value *print-pprint-dispatch* 'sb-pretty::entries)
	    (remove x dispatch-table)))))))

(defun pprint-dispatch-entries (&optional p)
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::entries)))
    (if p
      (dolist (x dispatch-table)
	(print x))
      dispatch-table)))

(defun pprint-dispatch-cons-entries ()
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::cons-entries)))
    (loop for key being the hash-keys of dispatch-table
      using (hash-value value)
      collect (list key value))))

(defun pprint-dispatch-find (term) ;; (pprint-dispatch-find '(eql ())) after (nil-as-list) for example
  (let*
    ((*print-pretty* nil)
      (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::entries)))
    (dolist (x dispatch-table)
      (cond
        ((equal term (slot-value x 'sb-pretty::type))
	  (return 
	    (values :entry x 
	      :type (slot-value x 'sb-pretty::type)
	      :priority (slot-value x 'sb-pretty::priority)
	      :function (slot-value x 'sb-pretty::fun))))))))

(defun pprint-dispatch-cons-find (term)
  (let* ((*print-pretty* nil) (dispatch-table (slot-value *print-pprint-dispatch* 'sb-pretty::cons-entries)))
    (labels ((dohash (table)
               (let (result)
                 (maphash (lambda (k v) (push (list k v) result)) table)
                 (nreverse result))))
      (dolist (x (dohash dispatch-table))
        (cond
	  ((equal term (first x))
	    (return
	      (values :entry x 
		:type (slot-value (second x) 'type) 
		:function (slot-value (second x) 'sb-pretty::fun) 
		:priority (slot-value (second x) 'sb-pretty::priority)))))))))

(defun pprint-dispatch-remove-quote ()
  (let ((dispatch-table (slot-value *print-pprint-dispatch* (quote sb-pretty::cons-entries))))
    (loop for key being the hash-keys of dispatch-table using (hash-value hash-value)
      collect (if (equal (quote (cons (eql quote))) (slot-value hash-value (quote type)))
		(remhash key dispatch-table)))))


(defun remove-from-pprint-dispatch (term)
  (let ((dispatch-table (slot-value *print-pprint-dispatch* (quote sb-pretty::cons-entries))))
    (loop for key being the hash-keys of dispatch-table using (hash-value hash-value)
      collect (if (equal term (slot-value hash-value (quote type)))
		(remhash key dispatch-table)))))

(defmacro do-hash ((key-var val-var hash-expr &optional result-form) &body body)
  (let ((hash-var (gensym "HASH-")))
    `(loop with ,hash-var = ,hash-expr
       for ,key-var being the hash-keys of ,hash-var
       for ,val-var being the hash-values of ,hash-var
       do (progn ,@body)
       finally (return ,result-form))))

;;; usage like...
;;;(do-hash (k v table (dohash table))
;;;  (terpri)
;;;  #+:clim
;;;  (with-drawing-options (t :text-size 16 :text-face :bold)
;;    (format t "key: ~s, value: ~s" k v)))


;; don't forget if you want your 'a stuff to expand to (quote a) properly set *print-pretty* to nil (disable)
;; else lookups are made via the pprint dispatch tables and the pprinter has its own ways of printing stuff!
;; especially don't forget to disable it when using/defining macro character functions!
;; else expansions might not be the same as expected which will confuse just more....
;; http://www.lispworks.com/documentation/HyperSpec/Body/22_ab.htm

;;;; and tho *print-pretty* is a special var disabling/enabling it from the listener won't affect it's value
;;;; in the sbcl repl! (threads ?)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,;;;;;;;;;;;;;;;;;

;;(setq *clocc-root* "/mnt/sdc1/home/clocc/")
;;(load "clocc/src/ytools/ytload/ytload.lisp")
;;(setq ytools::config-directory* "/mnt/sdc1/home/")
;;(setq ytools::ytload-directory* "/mnt/sdc1/home/clocc/src/ytools/ytload/")

(defvar *last-package* nil)
(defvar *cached-prompt* nil)
(defvar *prompt* nil)

(defun normrepl ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (sb-ext:without-package-locks
      
      (defun package-prompt (stream)
	(unless (eq *last-package* *package*)
	  (setf *cached-prompt*
	    (concatenate 'string (or (first (package-nicknames *package*))
				   (package-name *package*))
	      "> "))
	  (setf *last-package* *package*))
	(terpri)
	(princ *cached-prompt* stream))))

  (setf sb-int:*repl-prompt-fun* #'package-prompt))

(defun linrepl ()
  (when (interactive-stream-p *standard-input*)
    (funcall (intern "INSTALL-REPL" :linedit)
      ;; Keeps toplevel commands intact, etc.
      :wrap-current t
      ;; Should EOF quit after verification?
      :eof-quits t
      ;; Persistent history
      :history "~/.linedit_history")))

#+clim
(let ((*print-pretty* nil))
  (defmacro in-listener (&rest body)
    `(clim-user::with-drawing-options (*standard-output* :text-size :very-large) ,@body)))

#+clim
(let ((*print-pretty* nil))
  (defmacro in-clim (&rest body)
    `(if (and
	   (equal *package* (find-package :clim-user))
	   (find :clim *features*))
       (clim-user::with-drawing-options (*standard-output* :text-size :huge) (progn ,@body))
       (progn ,@body))))

(defun clme ()
  (setq climacs-gui::*default-external-format* 'utf-8)
  (setq climacs-gui::*climacs-text-style* (clim:make-text-style :fix :bold :large))
  (climacs:climacs-rv :new-process t))

(defun clmi ()
  #+clim
  (progn
    (setf *debugger-hook* #'clim-debugger:debugger)
    ;;(setf *invoke-debugger-hook* #'clim-debugger:debugger)
    (let* ((*read-default-float-format* 'double-float))
      ;; pixie doesn't work, segfaults at tiffcp in libtiff
      ;;(setf clim:*default-frame-manager* (make-instance 'climi::pixie/clx-look :port (clim:find-port)))

      ;;(setq drei::*highlight-strokes* nil)
      ;;(setq drei::*use-tabs-for-indentation* t)
      ;;(setq drei::*show-mark* t)

      (sb-sys:without-interrupts
	(sb-sys:with-local-interrupts
	  (unwind-protect
	    (values
	      (sleep 0.01)
	      (clim-listener:run-listener :new-process t)
	      (sleep 0.01))))))))

#+clim
(let ((climi::*default-text-style* (climi::make-text-style :fix :roman :large)))
  (defun clm ()
    (progn 
      (clmi)
      (clme))))

(defun mbrc ()
  #+clim
  (progn
    (setf *debugger-hook* #'clim-debugger:debugger)
    (setf *invoke-debugger-hook* #'clim-debugger:debugger)
    (let* ((*read-default-float-format* 'double-float))
      ;; pixie doesn't work, segfaults at tiffcp in libtiff
      ;;(setf clim:*default-frame-manager* (make-instance 'climi::pixie/clx-look :port (clim:find-port)))
      
      ;;(setq drei::*highlight-strokes* nil)
      (setq drei::*use-tabs-for-indentation* t)
      (setq drei::*show-mark* t)

      (sb-sys:without-interrupts				      
	(sb-sys:with-local-interrupts	
	  (unwind-protect
	    (values
	      (sleep 0.01)
	      (let ((*read-eval* nil))
		(beirc:beirc :new-process t))
	      (sleep 0.01))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun thread-list ()
    (funcall
      (let ()
	(lambda ()
	  (sb-thread:list-all-threads)))))

  (defun current-threads ()
    (let ()
      (lambda () (thread-list))))

  (setf (symbol-value 'thread-list) (funcall (symbol-function 'thread-list)))
  (setf (symbol-value 'current-threads) (current-threads))

  (defun kill-first-of ()
    (sb-thread:terminate-thread (first (sb-thread:list-all-threads))))

  (defun kill-last-of ()
    (sb-thread:terminate-thread (first (last (sb-thread:list-all-threads)))))

  (defun kill-nth-of (n)
    (sb-thread:terminate-thread (nth n (sb-thread:list-all-threads)))))

(defun kill-listener ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal "Listener" (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun kill-climacs ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal (or "Climacs-RV" "Climacs") (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun kill-beirc ()
  (let ((thread-list (sb-thread:list-all-threads)))
    (dolist (x thread-list)
      (cond ((equal "BEIRC GUI process" (sb-thread:thread-name x))
	      (sb-thread:terminate-thread x))))))

(defun match (s l &optional p)
  (block main
    (labels  ((finds (s l &optional p)
		
		(declare (list s l))
		(labels ((part (n l) (elt l n)))
		  (labels ((makelist (fn start stop &optional (incr 1))
			     (do ((ans)
				   (start start (+ incr start))
				   (stop stop stop))
			       ((> start stop) (nreverse ans))
			       (push (funcall fn start) ans))))
		    (let* ((parts
                             (makelist
			       (lambda (m)
				 (makelist (lambda (n) (part (1- n) l)) (1+ m) (+ (length s) m)))
			       0 (- (length l) (length s))))
                            (ans
			      (loop for n from 1 to (length parts)
				collect (mapcar
					  (lambda (x)
					    (if (equal (second x) (third x))
					      (second x)))
					  (mapcar (lambda (x y) (list '= x y))
					    (part (1- n) parts) s))))
                            (count
			      (length
				(remove nil
				  (mapcar
				    (lambda (x)
				      (if (member 'nil x)
					nil
					x))
				    ans))))
                            pos rep final)
		      
		      (loop for i from 1 to (length ans)
			do (if (not (member nil (elt ans (1- i))))
			     (setf pos
			       (append pos
				 (list
				   (list i
				     (+ (1- i)
				       (length (elt ans (1- i))))))))))
		      (loop for i in pos
			do (setf rep
			     (append rep
			       (makelist (lambda (x) x) (apply 'min i)
				 (apply 'max i)))))
		      (setf final
			(mapcar
			  (lambda (x)
			    (if (member x rep)
			      (elt l (1- x))
			      "_"))
			  (makelist (lambda (x) x) 1 (length l))))
		      (if p
			final
			(values (list '|count:| count)
			  (list '|pos:| pos))))))))
      (finds s l (if p p nil)))))

(defun search-package (m &optional p end-wise)
  (labels ((pull-out (list)
	     (let ((result))
	       (dolist (x list (nreverse result))
		 (cond ((atom x) (push x result))
		   ((listp x) (push (car x) result)) (t nil))))))
    (let ((ans
	    (remove nil
	      (loop for x in (list-all-packages)
		collect (remove nil
			  (loop for k from 0 to (length
						  (package-name
						    x))
			    collect (find (string m)
				      (list
					(package-name x))
				      :test
				      (lambda (x y)
					(equalp
					  (subseq x 0)
                                          (subseq y (if (>= k (length x)) (- k (length x)) 0) k)))
                                          :from-end end-wise)))))))
      (if p (values-list (pull-out ans))
      (pull-out ans)))))

(defun search-package (m &optional p end-wise)
  (let ((ans
	  (remove-if 'null
	    (loop for x in (list-all-packages)
	      collect (find-if (lambda (x) (not (null x)))
			(loop for k from 0 to (length
						(package-name
						  x))
			  collect (find (string m)
				    (list
				      (package-name x))
				    :test
				    (lambda (x y)
				      (equalp
					(subseq x 0)
					(subseq y (if (>= k (length x)) (- k (length x)) 0) k))) 
                                        :from-end end-wise)))))))
    (if p (values-list ans)
      ans)))


(defun paip ()
  (ignore-errors
    (progn
      (load "/mnt/sdc1/home/prg/lisp/paip/norvig-old/auxmacs.lisp")
      (load "/mnt/sdc1/home/prg/lisp/paip/norvig-old/auxfns.lisp"))))

(defun paip-new ()
  (ignore-errors
    (progn 
      (load "/mnt/sdc1/home/prg/lisp/paip-pjb/norvig-paip-pjb.lisp"))))

(defun make-loops (var-list-pairs last-loop-body)
  (if (endp var-list-pairs)
    last-loop-body
    (destructuring-bind ((var list) . therest) var-list-pairs
      `(loop for ,var in ,list do ,(make-loops therest last-loop-body)))))

(defmacro make-comprehension (expr vars-in-lists guards)
  (let ((accum (gensym)))
    `(let ((,accum (list)))
       (progn 
	 ,(make-loops vars-in-lists
	    `(if (and ,@guards)
	       (push ,expr ,accum)))
	 (nreverse ,accum)))))

(defun bubble-sort (array cmp-fun) 
  "Bubble sort implementation in common lisp. Using the extended loop facility."
  (let ((result (copy-seq array)))
    (loop for i from (1- (length result)) downto 0 do
      (loop for j from 0 to i
	when (funcall cmp-fun (aref result i) (aref result j))
	do (rotatef (aref result i) (aref result j)) ))
    result))

(defun insertion-sort (cmp-fun array-to-sort) 
  "Implementation of a destructive insertion sort on ARRAY-TO-SORT.
The CMP-FUN is used to parametrize the order conditions. 
This sort is generic, that means it can sort all kinds of objects for which
one can run the CMP-FUN"

  (flet ((insert-into-sorted (index)
           ;; example of a local function all outer variables are 
           ;; accessible from within this local function
           (let ((el (aref array-to-sort index)))
             ;; save the element for later insertion
             (loop for j = (1- index) then (1- j)
	       while (and (>= j 0) (not (funcall cmp-fun (aref array-to-sort j) el)))
	       ;; the not is needed because the following should move all elements
	       ;; not in order to the right
	       do (setf (aref array-to-sort (1+ j)) (aref array-to-sort j))
	       finally  (setf (aref array-to-sort (1+ j)) el)))))
    ;; now we can add el at the proper place
    (loop for i from 0 upto (1- (length array-to-sort))
      do (insert-into-sorted i)))
  array-to-sort)

(defun small (list)
  (or (null list) (null (cdr list))))

(defun right-half (list)
  (last list (ceiling (/ (length list) 2))))

(defun left-half (list)
  (ldiff list (right-half list)))

(defun merge-lists (list1 list2)
  (merge 'list list1 list2 #'<))

(defun merge-sort (list)
  (if (small list) list
    (merge-lists
      (merge-sort (left-half list))
      (merge-sort (right-half list)))))

(defgeneric lt (some other))

(defmethod lt ((some number) (other number)) (< some other))

(defmethod lt ((some string) (other string)) (string< some other))

(defun qsort (l)
  (when l (destructuring-bind (p . xs) l
            (append (qsort (@ x x xs (lt x p))) (list p)
	      (qsort (@ x x xs (not (lt x p))))))))

(defun qsort-alt (list) 
  (when list 
    (destructuring-bind (p . xs) list 
      (loop for x in xs if (lt x p) 
	collect x into lesser 
	else collect x into greater 
	finally (return (append (quicksort-alt lesser) (list p) (quicksort-alt greater)))))))

(defun gen-tuples-m (lst)
  (reduce (lambda (b rest)
            (loop for xs in rest
	      append (loop for i from 1 to b
		       collecting (cons i xs))))
    lst
    :from-end t
    :initial-value '(())))

(defun string-to-number (str &optional (base *read-base*) &rest rest)
  (read-from-string str base))

(defun number-to-string (num &optional (base *read-base*) &rest rest)
  (write-to-string num :base base))

(defun read-char-by-name (stream)
  "blabla"
  (let* ((input stream))
    (cond
      ((equal "^[0-9a-fA-F]+s" input)
	(string-to-number input 16))
      ((equal "^#" input)
	(read input))
      (t (read stream)))))

(defun insert (&rest args)
  args)

(defun pds ()
  (ignore-errors
    (progn
      (load "/mnt/sdc1/home/prg/lisp/lisp/ppmx.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/dtrace.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/sdraw.lisp"))))

(defun lold ()
  (ignore-errors
    (progn
      (load "/mnt/sdc1/home/prg/lisp/lisp/package.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/onlisp-util.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/onlisp-app.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/lol-working.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/lol-book.lisp")
      (load "/mnt/sdc1/home/prg/lisp/lisp/generators.lisp"))))

(defun acl2 ()
  (load "/mnt/sdc1/home/prg/lisp/lisp/acl2.lisp"))

(declaim (optimize (safety 3) (debug 3) (space 0) (speed 0) (compilation-speed 0) (inhibit-warnings 0)))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
(declaim (sb-ext:muffle-conditions sb-ext:code-deletion-note))

(defun compiler-policy () (funcall (lambda () (sb-ext:describe-compiler-policy))))

(defun unlock-my-packages ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    #+sbcl
    (dolist (pkg '(:sb-ext :sb-debug :sb-alien :common-lisp :common-lisp-user))
      (sb-ext:unlock-package pkg))))

(defun load-sdl ()
  (progn
    #+:quicklisp
    (ql:quickload :trivial-garbage)
    (asdf:oos 'asdf:load-op :trivial-garbage)
    
    #+:quicklisp
    (ql:quickload :trivial-features)
    (asdf:oos 'asdf:load-op :trivial-features)
    
    #+:quicklisp
    (ql:quickload :babel)
    (asdf:oos 'asdf:load-op :babel)
    
    #+:quicklisp
    (ql:quickload :cffi)
    (asdf:oos 'asdf:load-op :cffi)
    
    #+:quicklisp
    (ql:quickload :lispbuilder-sdl)
    (asdf:oos 'asdf:load-op :lispbuilder-sdl)))

(defun load-png-stuff ()
  (progn
    
    #+:quicklisp
    (ql:quickload :iterate)
    (asdf:oos 'asdf:load-op :iterate)

    #+:quicklisp
    (ql:quickload :chipz)
    (asdf:oos 'asdf:load-op :chipz)

    #+:quicklisp
    (ql:quickload :png-read)
    (asdf:oos 'asdf:load-op :png-read)

    #+:quicklisp
    (ql:quickload :mcclim-png-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-png-bitmaps)))

(defun load-jpeg-stuff ()
  (progn
    
    #+:quicklisp
    (ql:quickload :cl-jpeg)
    (asdf:oos 'asdf:load-op :cl-jpeg)

    #+:quicklisp
    (ql:quickload :mcclim-jpeg-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-jpeg-bitmaps)))

(defun load-gif-stuff ()
  (progn

    #+:quicklisp
    (ql:quickload :skippy)
    (asdf:oos 'asdf:load-op :skippy)

    #+:quicklisp
    (ql:quickload :mcclim-gif-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-gif-bitmaps)))

(defun load-tiff-stuff ()
  (progn

    #+:quicklisp
    (ql:quickload :ieee-floats)
    (asdf:oos 'asdf:load-op :ieee-floats)

    #+:quicklisp
    (ql:quickload :com.gigamonkeys.binary-data)
    (asdf:oos 'asdf:load-op :com.gigamonkeys.binary-data)

    #+:quicklisp
    (ql:quickload :retrospectiff)
    (asdf:oos 'asdf:load-op :retrospectiff)

    #+:quicklisp
    (ql:quickload :mcclim-tiff-bitmaps)
    (asdf:oos 'asdf:load-op :mcclim-tiff-bitmaps)))

(defun load-pic-support ()
  (progn (load-png-stuff) (load-jpeg-stuff) (load-gif-stuff) (load-tiff-stuff)))

(defun current-view (&optional (pane-name *standard-output*))
  (funcall
    (lambda ()
      (stream-default-view pane-name))))

(defun current-frame-name (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (or
	(type-of frame)
	(slot-value frame 'climi::name)))))

(defun current-frame (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value (frame-top-level-sheet frame) 'climi::frame))))

(defun current-frame-class (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (class-of frame))))

(defun current-frame-class-description ()
  (funcall
    (lambda ()
      (describe (current-frame-class)))))

(defun current-frame-instance (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      frame)))

(defun current-frame-instance-description ()
  (funcall
    (lambda ()
      (describe (current-frame-instance)))))

(defun current-frame-panes (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value frame 'climi::named-panes))))

(defun current-frame-layouts (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value frame 'climi::layouts))))

(defun current-frame-layout (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value frame 'climi::current-layout))))

(defun current-frame-layout-panes (&optional (frame *application-frame*))
  (funcall
    (lambda ()
      (slot-value frame 'climi::panes-for-layout))))

;;(defparameter *default-font-family-name* "-*-unifont-*-*-*-*-*-180-*-*-*-*-iso10646-1")
(defparameter *default-font-family-name* "-*-dejavu sans mono-bold-r-normal-*-*-180-*-*-*-*-iso10646-1")
;;  (setq *default-font-family-name* (climi::make-text-style "misc-fixed" "medium-r" 18))

(defun update-map-for-face-with-name (map family name)
  (let ((current-face (getf map family)))
    (unless current-face
      (error "family ~A not found!" family))
    (substitute `(,name ,@(cdr current-face)) current-face map)))

(defun set-fix ()
  (let ((*default-font-family-name* "-*-unifont-*-*-*-*-*-180-*-*-*-*-iso10646-1"))
    (setf clim-clx::*clx-text-family+face-map*
      (clim-user::update-map-for-face-with-name
  	clim-clx::*clx-text-family+face-map* :fix clim-user::*default-font-family-name*))))

(defun change-directory (pathname)
  "Ensure that the current directory seen by RUN-PROGRAM has changed, and update *default-pathname-defaults*"
  #+CMU (unix:unix-chdir (namestring pathname))
  #+scl (unix:unix-chdir (ext:unix-namestring pathname))
  #+clisp (ext:cd pathname)
  #+sbcl (sb-posix:chdir (pathname (namestring pathname)))
  (setf *default-pathname-defaults* (pathname (namestring pathname))))

(defun list-dir (pathname)
  ;; (list-dir '/mnt/sdc1/home) for example
  (loop for f in 
    (directory (make-pathname :directory (string-downcase pathname) :name :wild :type :wild)) 
    collect f))

(defun print-dir (pathname)
  ;; (print-directory '/mnt/sdc1/home) for example
  (loop for f in 
    (directory (make-pathname :directory (string-downcase pathname) :name :wild :type :wild)) 
    do (print f)))
(defun ascii-table ()
  (let ((i -1))
    (format t "~&ASCII characters 32 thru 127.~&~%")
    (format t "   Dec  Hex  Char         |   Dec  Hex   Char         |   Dec  Hex   Char         |   Dec  Hex   Char~%")
    (loop while (< i 31) do
      (princ (format nil "~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s~%"
	       (setq i (+ 33  i)) i (code-char i)
	       (setq i (+ 32 i)) i (code-char i)
	       (setq i (+ 32 i)) i (code-char i)
	       (setq i (+ 1 i)) i (code-char i)))
      (setq i (- i 95)))) (values))

(defun ascii-table-s ()
  (let ((i -1))
    (format t "~&ASCII characters 32 thru 127.~&~%")
    (format t "   Dec  Hex  Char         |   Dec  Hex   Char         |   Dec  Hex   Char         |   Dec  Hex   Char~%")
    (loop while (< i 31) do
      (princ (format nil "~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s | ~4d ~4x    ~12s~%"
	       (setq i (+ 33  i)) i (string (code-char i))
	       (setq i (+ 32 i)) i (string (code-char i))
	       (setq i (+ 32 i)) i (string (code-char i))
	       (setq i (+ 1 i)) i (string (code-char i))))
      (setq i (- i 95)))) (values))

(defun extended-table ()
  (let ((i 128))
    (format t "~&extended ASCII characters (unicode) 128 thru 256.~&~%")
    (format t " Dec   Hex   Char  |  Dec   Hex   Char~%")
    (loop while (< i 256) do
      (princ (format nil "~4d ~4x ~50s  |  ~4d ~4x ~50s~%"
	       i i (code-char i)
	       (incf i) i (code-char i)))
      (incf i))) (values))

(defun extended-table-s ()
  (let ((i 128))
    (format t "~&extended ascii characters (unicode) 128 thru 256.~&~%")
    (format t " dec   hex   char  |  dec   hex   char~%")
    (loop while (< i 256)
      do (princ
	   (format nil "~4d ~4x ~50s  |  ~4d ~4x ~50s~%" 
	     i i (string (code-char i)) 
	     (incf i) i (string (code-char i))))
      (incf i))) (values))

(defun ucs-codes-t (start row col) ;; terminal version
  (let ((x start) (somechars nil))
    (do ((i 1 (1+ i)))
      ((> i row))
      (terpri)
      (do ((j 1 (1+ j)))
	((> j col))
	(format t "~s " (string (code-char x)))
	(incf x)))))

(defun ucs-codes-tl (start row col) ;; terminal-list version
  (let ((x start) (somechars nil))
    (do ((i 1 (1+ i)))
      ((> i row))
      (do ((j 1 (1+ j)))
	((> j col))
	(setq somechars (append somechars (list (string (code-char x)))))
	(incf x))) somechars))

(defun ma (args) (macroexpand args))
(defun ma-1 (args) (macroexpand-1 args))

(defun configure-maxima ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cl-user::change-directory "/mnt/sdc1/home/maxima-code/")
    (load "configure.lisp")
    (configure)
    (cl-user::change-directory "/mnt/sdc1/home/")))

(defun compile-maxima ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cl-user::change-directory "/mnt/sdc1/home/maxima-code/src/")
    (load "maxima-build.lisp")
    (maxima-compile)
    (cl-user::change-directory "/mnt/sdc1/home/")))

(defun load-maxima ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cl-user::change-directory "/mnt/sdc1/home/maxima-code/src/")
    (load "maxima-build.lisp")
    (maxima-load)
    (cl-user::change-directory "/mnt/sdc1/home/")))

(load "/mnt/sdc1/home/clocc/clocc.lisp")
(setf *clocc-root* "/mnt/sdc1/home/clocc/")
(compile-file (concatenate 'string *clocc-root* "clocc"))
(load (compile-file (concatenate 'string *clocc-root* "clocc")))

(load "/mnt/sdc1/home/clocc/src/ytools/ytload/ytload.lisp")
(setq ytools::config-directory* "/mnt/sdc1/home")
(setq ytools::ytload-directory* "/mnt/sdc1/home/clocc/src/ytools/ytload/")

(defun load-sys (sys)
  (load (concatenate 'string *clocc-root* "clocc"))
  (load (translate-logical-pathname (concatenate 'string "clocc:src;" sys))))

(load-sys "cllib;base")
(load-sys "cllib;autoload")
(load-sys "cllib;simple")
(load-sys "cllib;clhs")

(setf (logical-pathname-translations "norvig")
  `(("norvig:**;*.*.*" "/mnt/sdc1/home/prg/lisp/paip-pjb/norvig/**/*.*")))

(setq *default-pathname-defaults*
  (merge-pathnames
    *default-pathname-defaults*
    (make-pathname :directory '(:relative "prg/lisp/paip-pjb/"))))

(defun subclasses (class) (clim-listener::com-show-class-subclasses class))
(defun superclasses (class) (clim-listener::com-show-class-superclasses class))
(defun gfs (class) (clim-listener::com-show-class-generic-functions class))
(defun slots (class) (clim-listener::com-show-class-slots class))
(defun info (obj) (describe
		    (or
		      (find-class (find obj (apropos-list obj nil t)) nil)
		      (find-class (first (apropos-list obj nil t)) nil))))

(defun gf (gf) (clim-listener::com-show-generic-function
		 (sb-pcl::find-generic-function gf)))

(defun walk-tree (fun tree)
  (subst-if t
    (constantly nil)
    tree
    :key fun))

(defun walk-tree-atoms (fun tree)
  (tree-equal tree tree
    :test (lambda (element-1 element-2)
	    (declare (ignore element-2))
	    (funcall fun element-1)
	    t)))

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (index (integer-length integer) bits)
      (push (if (logbitp index integer) 1 0) bits))))

(defun list-of-bits (integer)
  (let ((i integer)
	 (bits '()))
    (dotimes (j (integer-length integer) bits)
      (push (logand i 1) bits)
      (setf i (ash i -1)))))

(defun list-of-bits (integer)
  (let ((mask 1)
	 (bits '()))
    (dotimes (i (integer-length integer) bits)
      (push (if (logtest mask integer) 1 0) bits)
      (setf mask (ash mask 1)))))

(defun list-of-bits (integer)
  (let ((bits '()))
    (dotimes (position (integer-length integer) bits)
      (push (ldb (byte 1 position) integer) bits))))

(defun :bin (value &optional (size 8))
  (format nil "#b~v,'0B" size value))

(defun :bin (value &key (size 64) (byte 8))
  (loop for position from (- size byte) downto -1 by byte

    with result = (ldb (byte byte position) value)
    and left-shift = (ash (ldb (byte byte position) value) 1)  
    and right-shift = (ash (ldb (byte byte position) value) -1)

    do
    (format t "~%~70<~v,'0b~>~&~70<~v,'0b~>~&~70<~v,'0b~>~% " byte result byte left-shift byte right-shift)
    ))

(defun :oct (value &optional (size 3))
  (format nil "#o~v,'0O" size value))

(defun :hex (value &optional (size 3))
  (format nil "#x~v,'0X" size value))

(defun :dec (value &optional (size 3))
  (format nil "#d~v,'0d" size value))

(defun bin->hex (bin)
  (:hex (values (read-from-string (:bin bin) t nil :start 2))))

(defun hex->bin (hex)
  (:bin (values (read-from-string (:hex hex) t nil :start 2))))

(defun oct->bin (oct)
  (:bin (values (read-from-string (:oct oct) t nil :start 2))))

(defun bin->oct (bin)
  (:oct (values (read-from-string (:bin bin) t nil :start 2))))

(defun bin->dec (bin)
  (:dec (values (read-from-string (:bin bin) t nil :start 2))))

(defun dec->bin (dec)
  (:bin (values (read-from-string (:dec dec) t nil :start 2))))

(defun hex->dec (hex)
  (:dec (values (read-from-string (:hex hex) t nil :start 2))))

(defun dec->hex (dec)
  (:hex (values (read-from-string (:dec dec) t nil :start 2))))

(defun oct->dec (oct)
  (:dec (values (read-from-string (:oct oct) t nil :start 2))))

(defun dec->oct (dec)
  (:oct (values (read-from-string (:dec dec) t nil :start 2))))

(defun hex->oct (hex)
  (:oct (values (read-from-string (:hex hex) t nil :start 2))))

(defun oct->hex (oct)
  (:hex (values (read-from-string (:oct oct) t nil :start 2))))

(defun bits (value &optional (size 8))
  (cond 
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'x))
      (values (read-from-string (format nil "~v,'0B" size (hex->bin value)) t nil :start 2)))
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'o))
      (values (read-from-string (format nil "~v,'0B" size (oct->bin value)) t nil :start 2)))
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'd))
      (values (read-from-string (format nil "~v,'0B" size (dec->bin (read-from-string value t nil :start 2))) t nil :start 2)))
    ((and (stringp value) (equal (values (read-from-string value nil nil :start 1 :end 2)) 'b))
      (values (read-from-string (format nil "~v,'0B" size value) t nil :start 2)))
    ((numberp value) (values (read-from-string (format nil "~v,'0B" size value))))
    (t
      (values (read-from-string (format nil "~v,'0B" size value) t nil :start 2)))))

(defun tokenize-sentence (string)
  (macrolet ((add-word (wvar svar)
	       `(when ,wvar
		  (push (coerce (nreverse ,wvar) 'string) ,svar)
		  (setq ,wvar nil))))
    (loop with word = '() and sentence = '() and endpos = nil
      for i below (length string)
      do (let ((char (aref string i)))
	   (case char
	     (#\Space (add-word word sentence))
	     ((and #\, (let ((endpos (1+ i))) (alphanumericp char)))
	       (setf endpos (1+ i)))
	     ;;((or #\. #\, #\:) (add-word word sentence) (push (string char) sentence) (setq endpos (1+ i)))
	     (otherwise (push char word))))
      finally (add-word word sentence)
      (return (values (nreverse sentence) endpos)))))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
      for old-pos = 0 then (+ pos part-length)
      for pos = (search part string
		  :start2 old-pos
		  :test test)
      do (write-string string out
	   :start old-pos
	   :end (or pos (length string)))
      when pos do (write-string replacement out)
      while pos)))

(defun split-seq-by-n (seq n)
  (labels ((seq-split (seq n &optional acc orig-n)
	     (cond ((zerop (length seq)) (nreverse acc))
	       ((zerop n) (seq-split seq 
			    orig-n
			    (cons (subseq seq 0 0) acc)
			    orig-n))
	       (t (seq-split (subseq seq 1)
		    (1- n) 
		    (cons (concatenate (class-of seq) 
			    (if acc (car acc) (subseq seq 0 0))
			    (list (elt seq 0)))
		      (cdr acc))
		    orig-n)))))
    (seq-split seq n nil n)))

(defun space-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\Space string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun period-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\. string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\, string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun colon-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\: string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun lparen-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\( string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun rparen-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\) string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun lbracket-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\[ string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun rbracket-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\] string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun rbracket-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\] string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun lbrace-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\{ string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun rbrace-split (string)
  (loop for start = 0 then (1+ finish)
    for finish = (position #\} string :start start)
    collecting (subseq string start finish)
    until (null finish)))

(defun join-strings (strings) (with-output-to-string (s) (format s "~{~a~^~}" strings)))
(defun space-join (strings) (with-output-to-string (s) (format s "~{~a~^ ~}" strings)))
(defun period-join (strings) (with-output-to-string (s) (format s "~{~a~^.~}" strings)))
(defun comma-join (strings) (with-output-to-string (s) (format s "~{~a~^,~}" strings)))
(defun colon-join (strings) (with-output-to-string (s) (format s "~{~a~^:~}" strings)))
(defun lparen-join (strings) (with-output-to-string (s) (format s "~{~a~^(~}" strings)))
(defun rparen-join (strings) (with-output-to-string (s) (format s "~{~a~^)~}" strings)))
(defun paren-join (strings) (with-output-to-string (s) (format s "~{(~a~^)~})" strings)))
(defun lbracket-join (strings) (with-output-to-string (s) (format s "~{~a~^[~}" strings)))
(defun rbracket-join (strings) (with-output-to-string (s) (format s "~{~a~^]~}" strings)))
(defun bracket-join (strings) (with-output-to-string (s) (format s "~{[~a~^]~}]" strings)))
(defun lbrace-join (strings) (with-output-to-string (s) (format s "~{~a~^{~}" strings)))
(defun rbrace-join (strings) (with-output-to-string (s) (format s "~{~a~^}~}" strings)))
(defun brace-join (strings) (with-output-to-string (s) (format s "~{{~a~^}~}}" strings)))

(in-package :cl-user)
(defun next-epsi (epsi) (/ epsi 2))

(defun epsi-sig-single-p (epsi) (> (+ 1.0f0 epsi) 1.0f0))
(defun epsi-sig-double-p (epsi) (> (+ 1.0d0 epsi) 1.0d0))

(defun is-epsi-single-p (epsi) 
  (and (epsi-sig-single-p epsi) 
    (not (epsi-sig-single-p (next-epsi epsi)))))

(defun is-epsi-double-p (epsi) 
  (and (epsi-sig-double-p epsi) 
    (not (epsi-sig-double-p (next-epsi epsi)))))

(defun find-epsi-single (&OPTIONAL (epsi 1.0f0)) 
  (if (is-epsi-single-p epsi)  ; if the next epsi candidate isn't significant
    epsi  ; we have found epsilon
    (find-epsi-single (next-epsi epsi)))) ; otherwise, go smaller

(defun find-epsi-double (&OPTIONAL (epsi 1.0d0)) 
  (if (is-epsi-double-p epsi)  ; if the next epsi candidate isn't significant
    epsi  ; we have found epsilon
    (find-epsi-double (next-epsi epsi)))) ; otherwise, go smaller

(defun date ()
  (progn (terpri t) (run-program "/usr/bin/date" '() :output t) (values)))

(defun datetime (&key (as-list nil) (time nil) (date nil))
  (multiple-value-bind (second minute hour day month year day-of-week dst-p tz)
    (get-decoded-time)
    (let* ((day-names
	     '("Monday" "Tuesday" "Wednesday"
		"Thursday" "Friday" "Saturday"
		"Sunday"))
	    (now (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second))
	    (today (format nil "~a, ~2,'0d.~2,'0d.~d, GMT~@d" (nth day-of-week day-names) day month year (- tz)))
	    (result
	      (format nil "time: ~2,'0d:~2,'0d:~2,'0d~%date: ~a, ~2,'0d.~2,'0d.~d, GMT~@d" hour minute second
		(nth day-of-week day-names) day month year (- tz))))
      (cond (as-list
	      (multiple-value-list
		(with-input-from-string (s result) (values (intern (read-line s)) (intern (read-line s))))))
	(time now)
	(date today)
	(t
	  (with-input-from-string (s result) (values (intern (read-line s)) (intern (read-line s)))))))))

(let ()
  (format t "~% machine-epsilon-single: ~a ~% machine-epsilon-double: ~a ~% epsi-sig-single-p? ~a ~% epsi-sig-double-p? ~a ~%"(find-epsi-single) (find-epsi-double) (epsi-sig-single-p (find-epsi-single)) (epsi-sig-double-p (find-epsi-double))) (values))

(let ()
  (terpri t)
  (let () (format t "Happy lisping!~%") (values))
  ;;(write-char #\Newline t) ;;is identical to (terpri t)
  (terpri t)
  (let () (format t "Machine: ~a~&" (machine-version)) (values))
  (let () (format t "OS: ~a ~a~&" (software-type) (software-version)) (values))
  (let () (format t "Host: ~a~&" (machine-instance)) (values))
  (let () (format t "Implementation: ~a~&" (lisp-implementation-type)) (values))
  (let () (format t "Type: ~a~&" (machine-type)) (values))
  (let () (format t "Version: ~a~&" (lisp-implementation-version)) (values))
  (terpri t)
  (let () (format t "Time: ~a Date: ~a" (datetime :time t) (datetime :date t)) (values))
  (terpri)
  (values))
