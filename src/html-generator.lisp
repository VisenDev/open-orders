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
           #:select
           #:str))
(in-package #:open-orders.html-generator)

(declaim (optimize (speed 3)))

(defvar *output-stream* nil)
(defun format-attributes-plist (attributes-plist)
  (loop :for (raw-name value) :on attributes-plist :by #'cddr
        :for name = (if (and (not (null raw-name)) (symbolp raw-name))
                        (string-downcase (symbol-name raw-name))
                        raw-name)
        :when (and name value)
          :do (format *output-stream* " ~a=\"~a\"" name value)))

(defun str (&rest things)
  "Converts things to a string, folding nested lists"
  (labels ((emit (thing)
             (cond
               ((null thing))
               ((listp thing)
                (mapc #'emit thing))
               (t
                (format *output-stream* "~a" thing)))))
    (mapc #'emit things)))

(defmacro with-output-to-html (&body body)
  `(with-output-to-string (*output-stream*)
    ,@body))

(defmacro tag (html-name attributes-plist self-closing-p &rest contents)
  (cond (self-closing-p
         (assert (null contents))
         `(str "<" ,html-name
               (format-attributes-plist (list ,@attributes-plist)) "/>"))

        (t
         `(str "<" ,html-name
               (format-attributes-plist (list ,@attributes-plist)) ">"
               ,@contents
               "</" ,html-name ">"))))

(defmacro deftag (name &key self-closing-p html-name)
  `(defmacro ,name (attributes-plist &body body)
     (assert (listp attributes-plist))
     `(tag ,,(or html-name (string-downcase (symbol-name name)))
           ,attributes-plist ,,self-closing-p ,@body)))

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
(not
 (time
  (dotimes (j 100)
    (with-output-to-html
      (html ()
        (head ()
          (title () "Testy Test"))
        (body ()
          (h1 (:class "header" :id "primary header")
            (list (list 1 2 3) 4)
            (list 1 2 3 4))
          (h2 () (if (boundp 'foo)
                     (h3 () (a () "hello there")) "no foo"))
          (p () "hi")))))))



