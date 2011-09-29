;;; cw-webjumps.el --- 

;; Copyright © 2011 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2011-09-29
;; Last changed: 2011-09-29 20:54:27
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(defcustom cw-webjumps-alist
  '((google . "http://www.google.com/#q=%s")

    (allocine . "http://www.allocine.fr/recherche/?q=%s")

    (synonymes . "http://www.crisco.unicaen.fr/cgi-bin/trouvebis2?requete=%s")
    (synonymes-com . "http://www.synonymes.com/synonyme.php?mot=%s")

    (using-english . "http://www.usingenglish.com/reference/idioms/search.php?q=%s")

    (wordreference-enfr . "http://www.wordreference.com/enfr/%s")
    (wordreference-fren . "http://www.wordreference.com/fren/%s")
    
    (wikipedia . "http://www.wikipedia.org/wiki/%s")
    (wikipedia-fr . "http://fr.wikipedia.org/wiki/%s"))
  
  "List of webjumps")


(dolist (type (mapcar 'car cw-webjumps-alist))
  (fset (intern (concat "cw-webjump-" (symbol-name type)))
	`(lambda (&optional uarg query)
	   "Search QUERY on appropriate search engine defined in `cw-webjumps-alist'.

See `cw-webjump' for further information."
	   (interactive)
	   (cw-webjump uarg ,(symbol-name type) query))))


;;;###autoload
(defun cw-webjump (&optional uarg jump query)
  "Open a browser using `browse-url' for JUMP and search QUERY.

Prompt for JUMP if not defined.

If QUERY is not defined, try to search in order:
  - text in active region,
  - `word-at-point'
  - read from minibuffer.

QUERY is read from minibuffer if called with
`universal-argument' (C-u) or UARGS is defined."
  (interactive "P")
  (let* ((jump (or jump
		   (completing-read
		    "Use jump: "
		    (mapcar '(lambda(x)
			       (symbol-name (car x)))
			    cw-webjumps-alist)
		    nil t)))
	 (query (or
		 (unless current-prefix-arg
		   query
		   (when (region-active-p)
		     (message "Region")
		     (buffer-substring-no-properties
		      (mark) (point)))
		   (let ((wap (word-at-point)))
		     (message "word-at-point")
		     (when wap (substring-no-properties wap))))
		 (read-from-minibuffer "Query: ")))
	 (jump-info (assoc (intern jump) cw-webjumps-alist))
	 (url (cdr jump-info)))
    (unless url
      (error "No jump info found for %" jump))
    
    (browse-url (format url query))))
   


(provide 'cw-webjumps)
