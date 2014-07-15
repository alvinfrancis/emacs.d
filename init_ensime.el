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

(let ((slick-prefix "^scala\\.slick\\." ))
  ;; Add slick doc lookup to lookup-map
  (when (assoc slick-prefix ensime-doc-lookup-map)
    (defvar ensime-doc-lookup-map
      (cons '("^scala\\.slick\\." . ensime-make-slick-doc-url) ensime-doc-lookup-map))))
