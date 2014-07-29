(defun setup-ensime ()
  (defvar ensime-slick-prefix "^scala\\.slick\\.")

  (defvar ensime-slickdoc-url-base
    "http://slick.typesafe.com/doc/2.0.0-M3/api/index.html#"
    "URL base for constructing slick links.")

  (defun ensime-make-slick-doc-url-helper
    (url-base type &optional member)
    "Given a scala type, and optionally a type member, construct the
   corresponding slick url.  Currently does not narrow down to member"
    (concat url-base (ensime-type-full-name type)))

  (defun ensime-make-slick-doc-url (type &optional member)
    (ensime-make-slick-doc-url-helper
     ensime-slickdoc-url-base type member))

  (unless (assoc ensime-slick-prefix ensime-doc-lookup-map)
    (add-to-list 'ensime-doc-lookup-map `(,ensime-slick-prefix . ensime-make-slick-doc-url))))

(add-hook 'ensime-connected-hook 'setup-ensime)
