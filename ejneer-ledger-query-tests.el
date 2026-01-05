;;; ejneer-ledger-test.el --- Tests for ejneer-ledger -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for ejneer-ledger.el

;;; Code:

(require 'ert)
(require 'ejneer-ledger-query)

;; Basic filters
(ert-deftest ejneer-ledger-test-account-filter ()
  (should (equal (ejneer-ledger-query-compile-filter '(account "Expenses"))
                 '(:limit "account =~ /Expenses/" :flags nil))))

(ert-deftest ejneer-ledger-query-test-payee-filter ()
  (should (equal (ejneer-ledger-query-compile-filter '(payee "Amazon"))
                 '(:limit "payee =~ /Amazon/" :flags nil))))

(ert-deftest ejneer-ledger-query-test-period-both ()
  (should (equal (ejneer-ledger-query-compile-filter '(period "2024-01" "2024-12"))
                 '(:limit "[from 2024-01 until 2024-12]" :flags nil))))

(ert-deftest ejneer-ledger-query-test-period-start-only ()
  (should (equal (ejneer-ledger-query-compile-filter '(period "2024-01" nil))
                 '(:limit "[from 2024-01]" :flags nil))))

(ert-deftest ejneer-ledger-query-test-period-end-only ()
  (should (equal (ejneer-ledger-query-compile-filter '(period nil "2024-12"))
                 '(:limit "[until 2024-12]" :flags nil))))

(ert-deftest ejneer-ledger-query-test-tag-no-value ()
  (should (equal (ejneer-ledger-query-compile-filter '(tag "work"))
                 '(:limit "has_tag(/work/)" :flags nil))))

(ert-deftest ejneer-ledger-query-test-tag-with-pattern ()
  (should (equal (ejneer-ledger-query-compile-filter '(tag "Imported" "2025/12/.*"))
                 '(:limit "tag(/Imported/) =~ /2025/12/.*/" :flags nil))))

;; Combinators
(ert-deftest ejneer-ledger-query-test-combinator-and-basic ()
  (should (equal (ejneer-ledger-query-filter-combinator "and" 
							'((account "Expenses") (payee "Amazon")))
                 '(:limit "(account =~ /Expenses/ and payee =~ /Amazon/)" :flags nil))))

(ert-deftest ejneer-ledger-query-test-combinator-or-basic ()
  (should (equal (ejneer-ledger-query-filter-combinator "or"
							'((account "Expenses") (account "Assets")))
                 '(:limit "(account =~ /Expenses/ or account =~ /Assets/)" :flags nil))))

(ert-deftest ejneer-ledger-query-test-combinator-single-filter ()
  (should (equal (ejneer-ledger-query-filter-combinator "and"
							'((account "Expenses")))
                 '(:limit "account =~ /Expenses/" :flags nil))))

(ert-deftest ejneer-ledger-query-test-combinator-three-filters ()
  (should (equal (ejneer-ledger-query-filter-combinator "or"
							'((payee "Amazon") (payee "Walmart") (payee "Target")))
                 '(:limit "(payee =~ /Amazon/ or payee =~ /Walmart/ or payee =~ /Target/)" :flags nil))))

(ert-deftest ejneer-ledger-query-test-filter-and ()
  (should (equal (ejneer-ledger-query-compile-filter '(and (account "Expenses") 
                                                     (payee "Amazon")))
                 '(:limit "(account =~ /Expenses/ and payee =~ /Amazon/)" :flags nil))))

(ert-deftest ejneer-ledger-query-test-filter-or ()
  (should (equal (ejneer-ledger-query-compile-filter '(or (account "Expenses") 
                                                    (account "Assets")))
                 '(:limit "(account =~ /Expenses/ or account =~ /Assets/)" :flags nil))))

(ert-deftest ejneer-ledger-query-test-filter-nested ()
  (should (equal (ejneer-ledger-query-compile-filter '(and (account "Expenses")
							   (or (payee "Amazon")
                                                               (payee "Walmart"))))
                 '(:limit "(account =~ /Expenses/ and (payee =~ /Amazon/ or payee =~ /Walmart/))" :flags nil))))

;; Full query tests
(ert-deftest ejneer-ledger-query-test-query-full ()
  (should (equal (ejneer-ledger-query 'balance
                                      '(and (account "Expenses")
                                            (period "2024-01" "2024-12"))
                                      :depth 2
                                      :monthly)
                 "ledger balance --depth 2 --monthly --limit \"(account =~ /Expenses/ and [from 2024-01 until 2024-12])\"")))

(ert-deftest ejneer-ledger-query-test-query-simple ()
  (should (equal (ejneer-ledger-query 'register
                                      '(payee "Amazon")
                                      :monthly)
                 "ledger register --monthly --limit \"payee =~ /Amazon/\"")))

(ert-deftest ejneer-ledger-query-test-query-no-filter ()
  (should (equal (ejneer-ledger-query 'balance nil :depth 1)
                 "ledger balance --depth 1")))

(provide 'ejneer-ledger-query-test)
;;; ejneer-ledger-query-test.el ends here
