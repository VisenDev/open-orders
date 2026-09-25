(defpackage #:open-orders.html-generator
  (:use #:cl)
  (:export #:deftag
           #:tag
           #:+doctype-html+
           #:deftags
           ;; Tags
           #:h1 #:h2 #:h3 #:h4 #:h5 #:p #:a
           #:abbreviation #:acronym #:address #:anchor
           #:applet #:area #:article #:aside
           #:audio #:base #:basefont #:bdi
           #:bdo #:bgsound #:big #:blockquote
           #:body #:bold #:html-break #:button
           #:caption #:canvas #:center #:cite
           #:code #:colgroup #:col #:comment
           #:data #:datalist #:dd #:define
           #:html-delete
           #:details #:dialog #:dir
           #:div #:dl #:dt #:embed
           #:fieldset #:figcaption #:figure #:font
           #:footer #:form #:frame #:frameset
           #:head #:header #:heading #:hgroup
           #:hr #:html #:iframe #:image
           #:input #:ins #:isindex #:italic
           #:kbd #:keygen #:label #:legend
           #:link #:html-list #:html-main #:mark
           #:marquee #:menuitem #:meta #:meter
           #:nav #:nobreak #:noembed #:noscript
           #:object #:optgroup #:option #:output
           #:paragraph #:param #:em #:pre
           #:progress #:q #:rp #:rt
           #:ruby #:s #:samp #:script
           #:section #:small #:source #:spacer
           #:span #:strike #:strong #:style
           #:sub #:sup #:summary #:svg
           #:html-table #:tbody #:td #:template
           #:tfoot #:th #:thead #:html-time
           #:title #:tr #:track #:tt
           #:underline #:var #:video #:wbr
           #:xmp
           #:doctype
           #:br
           #:select))
(in-package #:open-orders.html-generator)

(eval-when (:compile-toplevel :load-toplevel)
  (defun concatenate-string-p (form)
    (and (listp form)
         (eq 'concatenate (first form))
         (equalp (quote (quote string)) (second form))))

  (defun deduplicate-concatenate (forms)
    (loop :for form :in forms
          :appending
          (if (concatenate-string-p form)
              (cddr form)
              (list form))))

  (defun compress-adjacent-strings (forms)
    (let ((result nil))
      (dolist (form forms)
        (if (and (stringp (first result)) (stringp form))
            (setf (first result)
                  (concatenate 'string (first result) form))
            (push form result)))
      (nreverse result)))
  (defun format-attributes-plist (attributes-plist)
    (loop :for (name value) :on attributes-plist :by #'cddr
          :collect (if (and (or (stringp name) (keywordp name))
                            (or (stringp value) (keywordp value)))

                       ;; create the attributes string at compile time if possible
                       (string-downcase
                        (format nil " ~a=\"~a\"" name value))

                       ;; otherwise just create the code to do so at runtime
                       `(string-downcase
                         (format nil " ~a=\"~a\"" ,name ,value))))))

(defmacro doctype (attributes-plist &body contents &environment env)
  "Special doctype tag"
  (declare (ignore attributes-plist))
  `(concatenate 'string "<!DOCTYPE html>"
                ,@(deduplicate-concatenate
                   (mapcar (lambda (form) (macroexpand form env)) contents))))


(defmacro self-closing-tag (html-name attributes-plist)
  (compress-adjacent-strings
   `(concatenate
     'string
     ;; Tag Open
     ,(format nil "<~a" html-name)
     ,@(format-attributes-plist attributes-plist)
     ">")))

(defmacro tag (html-name attributes-plist &rest contents &environment env)
  (compress-adjacent-strings
   `(concatenate
     'string

     ;; Tag Open
     ,(format nil "<~a" html-name)
     ,@(format-attributes-plist attributes-plist)
     ">"

     ;; Tag body, with nest (concatenate 'string) forms collapsed
     ,@(deduplicate-concatenate
        (mapcar (lambda (form)

                  ;; macroexpand body to so that we can optimize
                  (let ((expanded (macroexpand form env)))

                    ;; if the form is a string, we can just return it as is
                    (cond ((or (stringp expanded) (concatenate-string-p expanded))
                           expanded)

                          ;; Otherwise the form needs to be formatted at runtime
                          (t (let ((result (gensym)))
                               `(let ((,result ,expanded))
                                  (if (listp ,result)
                                      (format nil "~{~a~}" ,result)
                                      (format nil "~a" ,result))))))))
                contents))

     ;; Tag Close
     ,(format nil "</~a>" html-name))))

(defmacro deftag (name &key self-closing-p html-name)
  (unless html-name
    (setf html-name (string-downcase (symbol-name name))))
  (if self-closing-p
      `(defmacro ,name (attributes-plist)
         `(self-closing-tag ,,html-name ,attributes-plist))
      `(defmacro ,name (attributes-plist &body body)
         `(tag ,,html-name ,attributes-plist ,@body))))

(defmacro deftags (&body forms)
  `(progn
    ,@(mapcar (lambda (form)
              `(deftag ,@(if (listp form) form (list form))))
              forms)))

(deftags
  h1 h2 h3 h4 h5 p a
  abbreviation acronym address anchor
  applet (area :self-closing-p t)
  article aside
  audio (base :self-closing-p t)
  basefont bdi
  bdo bgsound big blockquote
  body bold (html-break :html-name "break")
  (br :self-closing-p t)
  button caption canvas center cite
  code colgroup (col :self-closing-p t)
  comment data datalist dd define
  (html-delete :html-name "delete")
  details dialog dir
  div dl dt (embed :self-closing-p t)
  fieldset figcaption figure font
  footer form frame frameset
  head header heading hgroup
  (hr :self-closing-p t) html iframe
  (image :self-closing-p t)
  (img :self-closing-p t)
  (input :self-closing-p t)
  ins isindex italic
  kbd keygen label legend
  (link :self-closing-p t)
  (html-list :html-name "list")
  (html-main :html-name "main") mark
  marquee menuitem (meta :self-closing-p t) meter
  nav nobreak noembed noscript
  object optgroup option output
  paragraph (param :self-closing-p t) em pre
  progress q rp rt
  ruby s samp script
  section select small (source :self-closing-p t)
  spacer span strike strong style
  sub sup summary svg
  (html-table :html-name "table") tbody td template
  tfoot th thead (html-time :html-name "time")
  title tr (track :self-closing-p t) tt
  underline var video (wbr :self-closing-p t)
  xmp)


;; test
#+nil
(html ()
  (head ()
    (title () "Testy Test"))
  (body ()
    (h1 (:class "header" :id "primary header"))
    (h2 () (if (boundp 'foo)
               (h3 () (a () "hello there")) "no foo"))
    (p () "hi")))









