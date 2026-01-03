;;; ejneer-ledger-query.el --- DSL for ledger CLI reporting -*- lexical-binding: t -*-
;; Version: 0.1.0

;;; Commentary:
;; Personal utility for constructing ledger CLI queries with a small
;; DSL inspired by `rx`.
;;
;;
;; flags and limits:
;; ledger balance --begin 2024-01 --limit "account =~ /Expenses/"
;;                ^^^^^^^^^^^^^^^         ^^^^^^^^^^^^^^^^^^^^^^^
;;                flags                      limit expression
;;
;; Usage:
;;   (ejneer-ledger-query 'balance
;;                 '(and (account "Expenses")
;;                       (period "2024-01" nil))
;;                 :depth 2
;;                 :file "finances.ledger")
;;; Code:

(require 'seq)

(defun ejneer-ledger-query-filter-combinator (op filters)
  "Compile filters combined with OP (\"and\" or \"or\")."
  (let* ((compiled (seq-map #'ejneer-ledger-query-compile-filter filters))
         (limits (seq-filter #'identity
                             (seq-map (lambda (c) (plist-get c :limit)) compiled)))
         (flags (seq-mapcat (lambda (c) (plist-get c :flags)) compiled)))
    `(:limit ,(when limits
                (let ((joined (string-join limits (format " %s " op))))
                  (if (> (length limits) 1)
                      (format "(%s)" joined)
                    joined)))
	     :flags ,flags)))

(defun ejneer-ledger-query-compile-filter (filter)
  "Compile FILTER s-expression to ledger query components.
Returns a plist with :limit and :flags keys."
  (pcase filter
    ('nil '(:limit nil :flags nil))

    ;; Combinators
    (`(and . ,filters) (ejneer-ledger-query-filter-combinator "and" filters))
    (`(or . ,filters) (ejneer-ledger-query-filter-combinator "or" filters))
    ;; Basic filters
    (`(account ,pattern)
     `(:limit ,(format "account =~ /%s/" pattern) :flags nil))
    (`(payee ,pattern)
     `(:limit ,(format "payee =~ /%s/" pattern) :flags nil))
    ;; Period with both start and end
    (`(period ,from ,to)
     `(:limit nil :flags ,(seq-remove #'null (list (when from (format "--begin %s" from))
						   (when to (format "--end %s" to))))))
    ;; Tag filters
    (`(tag ,tag)
     `(:limit ,(format "has_tag(/%s/)" tag) :flags nil))
    (`(tag ,tag ,pattern)
     `(:limit ,(format "tag(/%s/) =~ /%s/" tag pattern) :flags nil))
    (_ (error "Unknown filter: %s" filter))))

(defun ejneer-ledger-query-compile-options (options)
  "Compile OPTIONS plist to CLI flags."
  (let (flags)
    (while options
      (let ((key (pop options))
            (val (pop options)))
        (push (pcase key
		(:file (format "-f %s" val))
                (:depth (format "--depth %d" val))
                (:monthly "--monthly")
                (_ (error "Unknown option: %s" key)))
              flags)))
    (nreverse flags)))

(defun ejneer-ledger-query (report-type filter &rest options)
  "Execute ledger REPORT-TYPE with FILTER and OPTIONS.
Returns the ledger command string."
  (let* ((compiled (ejneer-ledger-query-compile-filter filter))
         (limit (plist-get compiled :limit))
         (filter-flags (plist-get compiled :flags))
         (option-flags (ejneer-ledger-query-compile-options options))
         (parts (list "ledger" (symbol-name report-type))))
    (when filter-flags
      (setq parts (append parts filter-flags)))
    (when option-flags
      (setq parts (append parts option-flags)))
    (when limit
      (setq parts (append parts (list "--limit" (format "\"%s\"" limit)))))
    (string-join parts " ")))

(provide 'ejneer-ledger-query)

;;; ejneer-ledger-query.el ends here
