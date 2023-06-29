;;; retrying-test.el --- unit test -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'retrying)

(defmacro test-retry--called (target &optional ignore-orig)
  (let* ((target-name (symbol-name target))
         (advice-name (format "test-retry--called-around-%s" target-name))
         (var-name (format "test-retry--called-count-%s" target-name))
         (cleanup-name (format "test-retry--called-cleanup-%s" target-name)))
    `(progn
       (defvar ,(read var-name) 0)
       (defun ,(read advice-name) (orig-func &rest args)
         (setq ,(read var-name) (+ 1 ,(read var-name)))
         ,(unless ignore-orig
            '(apply orig-func args)))
       (advice-add ',(read target-name) :around ',(read advice-name))
       (defun ,(read cleanup-name) ()
         (advice-remove ',(read target-name) ',(read advice-name))
         ,(read var-name)))))

(ert-deftest test-retry-do-retry ()
  (test-retry--called retrying--sleep t)
  (defvar test-retry-do-retry-called nil)
  (defun test-retry-do-retry-f ()
    (if (not test-retry-do-retry-called) (progn
                                           (setq test-retry-do-retry-called t)
                                           t)
      nil))
  (defvar test-retry-do-retry-callback-called nil)
  (defun test-retry-do-retry-callback (result)
    (if (not test-retry-do-retry-callback-called)
        (progn
          (should (retrying-result-result result))
          (should (not (retrying-result-complete result)))
          (should (retrying-result-abend result))
          (should (not (retrying-result-exhausted result)))
          (setq test-retry-do-retry-callback-called t)
          t)
      (should (not (retrying-result-result result)))
      (should (retrying-result-complete result))
      (should (not (retrying-result-abend result)))
      (should (not (retrying-result-exhausted result)))
      nil))
  (retrying-retry-do (make-retrying-retry :attempts 1
                                          :max-attempts 3
                                          :factor 2
                                          :max-backoff 10
                                          :init-backoff 1)
                     'test-retry-do-retry-f
                     'test-retry-do-retry-callback)
  (should test-retry-do-retry-called)
  (should (equal (test-retry--called-cleanup-retrying--sleep) 1)))

(ert-deftest test-retry-do-give-up ()
  (test-retry--called retrying--sleep t)

  (defun test-retry-do-give-up-f ()
    t)
  (defun test-retry-do-give-up-callback (result)
    (should (retrying-result-result result))
    (should (not (retrying-result-complete result)))
    (should (retrying-result-abend result))
    (should (not (retrying-result-exhausted result)))
    nil)
  (retrying-retry-do (make-retrying-retry :attempts 1
                                          :max-attempts 3
                                          :factor 2
                                          :max-backoff 10
                                          :init-backoff 1)
                     'test-retry-do-give-up-f
                     'test-retry-do-give-up-callback)
  (should (equal (test-retry--called-cleanup-retrying--sleep) 0)))

(ert-deftest test-retry-do-exhausted ()
  (defun test-retry-do-exhausted-f ()
    t)
  (defun test-retry-do-exhausted-callback (result)
    (should (retrying-result-result result))
    (should (retrying-result-complete result))
    (should (retrying-result-abend result))
    (should (retrying-result-exhausted result)))
  (retrying-retry-do (make-retrying-retry :attempts 1
                                          :max-attempts 0)
                     'test-retry-do-exhausted-f
                     'test-retry-do-exhausted-callback))

(ert-deftest test-retry-do-success ()
  (defun test-retry-do-success-f ()
    nil)
  (defun test-retry-do-success-callback (result)
    (should (not (retrying-result-result result)))
    (should (not (retrying-result-exhausted result)))
    (should (not (retrying-result-abend result)))
    (should (retrying-result-complete result)))
  (retrying-retry-do (make-retrying-retry :attempts 1
                                          :max-attempts 3
                                          :factor 2
                                          :max-backoff 10
                                          :init-backoff 1)
                     'test-retry-do-success-f
                     'test-retry-do-success-callback))

(defmacro test-retry-backoff
    (name want retry)
  (let ((testname (format "test-retry-backoff-%s" name)))
    `(progn
       (ert-deftest ,(read testname)
           ()
         (should (equal ,want
                        (retrying-retry-backoff ,retry)))))))

(test-retry-backoff "attempt-1"
                    1
                    (make-retrying-retry :attempts 1
                                         :max-attempts 3
                                         :factor 2
                                         :max-backoff 10
                                         :init-backoff 1))
(test-retry-backoff "attempt-2"
                    2
                    (make-retrying-retry :attempts 2
                                         :max-attempts 3
                                         :factor 2
                                         :max-backoff 10
                                         :init-backoff 1))
(test-retry-backoff "attempt-max"
                    4
                    (make-retrying-retry :attempts 3
                                         :max-attempts 3
                                         :factor 2
                                         :max-backoff 10
                                         :init-backoff 1))
(test-retry-backoff "over-max-attempts"
                    nil
                    (make-retrying-retry :attempts 4
                                         :max-attempts 3
                                         :factor 2
                                         :max-backoff 10
                                         :init-backoff 1))
(test-retry-backoff "over-max-backoff"
                    10
                    (make-retrying-retry :attempts 1
                                         :max-attempts 3
                                         :factor 2
                                         :max-backoff 10
                                         :init-backoff 100))

(provide 'retrying-test)
;;; retrying-test.el ends here
