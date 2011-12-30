#!/usr/bin/emacs --script

(require 'json)

(defun diego-clockserve-collect-data-for-entry ()
  (org-back-to-heading t)
  (let ((heading (nth 4 (org-heading-components))))
    (let ((end-of-drawers (save-excursion (org-end-of-meta-data-and-drawers))))
      (diego-clockserve-find-clocks '() heading end-of-drawers))))

(defun diego-clockserve-read-clock ()
  (save-excursion
    (unless (org-at-date-range-p t)
      (goto-char (point-at-bol))
      (re-search-forward org-tr-regexp-both (point-at-eol) t))
    (if (not (org-at-date-range-p t))
	(error "Not at a time-stamp range, and none found in current line")))
  (let ((ts1 (match-string 1))
	(ts2 (match-string 2)))
    (cons ts1 (cons ts2 nil))))

(defun diego-clockserve-find-clocks (so-far heading end-item)
  (if (re-search-forward
       "^[ \t]*CLOCK:.*$" end-item t)
      (let ((data (diego-clockserve-read-clock)))
	(diego-clockserve-find-clocks (cons (cons heading (cons data ())) so-far) heading end-item))
    so-far))

(defun flatten (list-of-lists)
  (apply #'append list-of-lists))

(defun diego-clockserve-convert-clock-data (single-record)
  (let ((flattened (flatten (cdr single-record))))
    (cons (cons 'lane (car single-record))
	  (cons (cons 'from (car flattened))
		(cons (cons 'to (nth 1 flattened)) nil)))))
  
(defun diego-clockserve-convert-clock-list-to-plist (data)
  (if data 
      (cons (diego-clockserve-convert-clock-data (car data)) 
	    (diego-clockserve-convert-clock-list-to-plist (cdr data)))))

(defun diego-clockserve-in-range (day entry)
  (let ((dt (substring (car (nth 1 entry)) 0 10)))
    (string= dt day)))

(defun diego-clockserve-filter-by-day (day entries)
  (loop for e in entries if (diego-clockserve-in-range day e) collect e))

(defun diego-clockserve-collect-data (&optional day)
  (diego-clockserve-convert-clock-list-to-plist 
   (let ((initial (flatten (org-map-entries 'diego-clockserve-collect-data-for-entry))))
     (if day
	 (diego-clockserve-filter-by-day day initial)
       initial))))
     
(defun diego-clockserve-extract-to-new-buffer ()
  (interactive)
  (let ((data (diego-clockserve-collect-and-print))
	(newBuf (generate-new-buffer-name "org table output" t)))
    (set-buffer (get-buffer-create newBuf))
    (switch-to-buffer (get-buffer-create newBuf))
    (insert data)))

(defun diego-clockserve-extract-table-data ()
  (interactive)
  (let ((data (diego-clockserve-collect-data))
	(newBuf (generate-new-buffer-name "org table output" t)))
    (set-buffer (get-buffer-create newBuf))
    (switch-to-buffer (get-buffer-create newBuf))
    (insert (json-encode-array data))))

(defun diego-clockserve-table-data-from-file (filename &optional day)
  (find-file filename)
  (let ((data (diego-clockserve-collect-data day)))
	(json-encode-array data)))

(setq query (getenv "QUERY_STRING"))

(setq query-list (mapcar (lambda (x) (split-string x "=")) (split-string query "&")))

(let ((dt (nth 1 (car query-list))))
  (princ
   (concat
    "Content-type: application/json\n\n"
    (diego-clockserve-table-data-from-file 
     "/home/d/Code/time_tracking_visualisation/data/data.org"
     dt)
    "\n\n")))


