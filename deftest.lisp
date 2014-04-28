(defpackage :net.mwatters.deftest
  (:nicknames :deftest)
  (:use :common-lisp)
  (:export
   :deftest
   :test-passed-p
   :test-failed-p
   :abort-failing-test
   :abort-passing-test
   :test-error
   :run-test
   :run-package-tests
   :run-all-tests))

(in-package :net.mwatters.deftest)


(defvar *tests* (make-hash-table :test 'equalp)
  "mapping from package names to lists of test descriptions
\(test-desc . function-designator\)")


(defun package-tests (&optional (package *package*))
  (gethash (package-name package) *tests*))

(defun (setf package-tests) (value &optional (package *package*))
  (setf (gethash (package-name package) *tests*) value))


(defun find-test (desc &optional (package *package*))
  (find desc (package-tests package)
        :key #'car
        :test #'string-equal))

(defun (setf find-test) (value desc &optional (package *package*))
  "add the test described by DESC having the function designator VALUE
to the set of tests for PACKAGE, or remove the test with that
description if VALUE is nil.  the list of tests is sorted by the
string representation of the function designator."
  (if value
      (progn
        (setf (package-tests package)
              (sort
               (acons desc value
                      (remove desc (package-tests package)
                              :key #'car
                              :test #'string-equal))
               (lambda (a b)
                 (string-lessp (if (symbolp a)
                                   a
                                 (prin1-to-string a))
                               (if (symbolp b)
                                   b
                                 (prin1-to-string b))))
               :key #'cdr))
        value)
    (progn
      (remove-test desc package)
      value)))


(defun remove-test (desc &optional (package *package*))
  (symbol-macrolet ((cur (package-tests package)))
    (setf cur (remove desc cur
                      :key #'car
                      :test #'string-equal))))



(defun run-test (desc &optional (package *package*))
  (let ((test (find-test desc package)))
    (unless test
      (error "test not found in ~A: ~S" package desc))
    (funcall (cdr test))))


;; fixme; count of success/failure
(defun run-package-tests (&optional (package *package*))
  (mapc (lambda (test)
          (funcall (cdr test)))
        (package-tests package))
  t)

(defun run-all-tests ()
  (let ((kvs (list)))
    (maphash (lambda (k v) ; k: package name, v: test specs in pkg
               (push (cons k v) kvs))
             *tests*)
    (setq kvs (sort kvs #'string-lessp :key #'car))
    (dolist (spec kvs)
      (destructuring-bind (name . test-specs) spec
        (declare (ignore name))
        (map nil #'funcall (mapcar #'cdr test-specs))))
    t))


(defmacro with-test-error-wrapper ((tdesc tfun tpkg) &body forms)
  (let ((c (gensym "C")))
    `(handler-bind
         ((error (lambda (,c)
                   (error 'test-error
                          :test-desc ,tdesc
                          :test-fun ,tfun
                          :test-pkg ,tpkg
                          :original-error ,c))))
       ,@forms)))


(eval-when (:load-toplevel :compile-toplevel :execute)
(defvar *test-counter* 0) ; fixme; package-specific test counters
(defvar *tests-passing* 0)
(defvar *tests-failing* 0)
(defvar *test-noisy-p* t)

(defun shortest-nickname (p)
  (or (car (sort (copy-seq (package-nicknames p))
                 #'< :key #'length))
      (package-name p)))
) ; eval-when


(defmacro with-test-rerun-wrapper ((fn desc pkg) &body forms)
  (let ((start (gensym "START"))
        (end (gensym "END"))
        (c (gensym "C")))
    `(tagbody
      ,start
      (setq test-passed-p nil
            test-failed-p nil)
      (handler-bind
          ((test-error (lambda (,c)
                         (declare (ignore ,c))
                         (when test-passed-p
                           (go ,end)))))
        (restart-case
            (progn ,@forms)
          (retry ()
            :report ,(format nil "Run test ~A again" fn)
            (go ,start))
          (retry-current ()
            :report ,(format nil "Try current definition of ~S" desc)
            (run-test ,desc ,pkg))
          (abort-pass ()
            :report "Abort test, treating as PASS"
            (setq test-passed-p t
                  test-failed-p nil))
          (abort-fail ()
            :report "Abort test, treating as FAIL"
            (setq test-passed-p nil
                  test-failed-p t))))
      ,end)))


(defun abort-passing-test ()
  (invoke-restart 'abort-pass))

(defun abort-failing-test ()
  (invoke-restart 'abort-fail))


#+without-unit-tests
(defmacro deftest (&rest args)
  (declare (ignore args)))
#-without-unit-tests
(defmacro deftest (description lambda-list &body forms)
  (let* ((description (string description))
         (fn (format nil "TEST-~D" (incf *test-counter*)))
         (fs (intern fn))
         (pn (shortest-nickname *package*)))
    (when (and (plusp (length lambda-list))
               (not (member (first lambda-list) '(&rest &optional &key))))
      (error "lambda-list ~S does not allow for ~
              automatic testing in ~S"
             lambda-list description))
    `(progn
       (setf (find-test ,description) ',fs)
       (defun ,fs ,lambda-list
         ;; body may set these to force pass/fail:
         (let ((test-passed-p nil)
               (test-failed-p nil))
           (unwind-protect
               (with-test-rerun-wrapper (,fn ,description ,*package*)
                 (when *test-noisy-p*
                   (format *trace-output*
                           "~&;; ~A:~A \(~A\)" ,pn ,fn ,description))
                 (prog1 (with-test-error-wrapper
                            (,description ,fn ,pn)
                          ,@forms)
                   (setq test-passed-p (not test-failed-p))))
             (if test-passed-p
                 (incf *tests-passing*)
               (incf *tests-failing*))
             (when *test-noisy-p*
               (format *trace-output*
                       "...~:[FAIL~;PASS~]~%" test-passed-p))))))))


(define-condition test-error (error)
  ((desc :initarg :desc
         :initform nil)
   (test-desc :initarg :test-desc
              :initform nil)
   (test-fun :initarg :test-fun
             :initform nil)
   (test-pkg :initarg :test-pkg
             :initform nil)
   (original-error :initarg :original-error
                   :initform nil))
  (:report
   (lambda (c s)
     (with-slots (desc test-desc test-fun test-pkg original-error) c
       (format s "failure while performing test ~S \(~A:~A\)~
                  ~@[: ~A~]~
                  ~@[~&Original error: ~A~]"
               test-desc test-pkg test-fun
               desc original-error)))))
;; fixme; assert-raises, etc
;; fixme; compile-time side effects of macros?


(deftest "a test should be findable after being added, and not after being removed" ()
  (let ((d "TEST-DESC-asdfhasdljhasdgasdg"))
    (assert (not (find-test d)))
    (unwind-protect
        (progn
          (setf (find-test d) 'foobazzle)
          (assert (eq 'foobazzle (cdr (find-test d))))
          (remove-test d)
          (assert (not (find-test d))))
      (remove-test d))))


(deftest "this test should fail" ()
  (abort-failing-test)
  (error "fixme"))

(deftest "this test should pass" ()
  (setq test-passed-p t)
  (error "fixme"))
