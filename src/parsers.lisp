;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/parsers.lisp $

;;; Copyright (c) 2010, Andrea Chiumenti.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :clonsigna)

(defun %parse-tagged-line (line message-id)
  (let ((scan-pattern (format nil "(^~a)[\\W](BAD|OK|NO)[\\W](.*)" message-id)))
    (multiple-value-bind (string result)
        (cl-ppcre:scan-to-strings scan-pattern line)
      (declare (ignore string))
      (let ((result-list (coerce result 'list)))
        (list (second result-list)
              (third result-list))))))

(defun %parse-reply-result (line-separator result)
  (loop for line in result
     for reply = nil then reply
     do (if (starts-with "*" line)
            (push line reply)
            (setf reply (push
                         (format nil "~a~a~a" (first reply) line-separator line)
                         (rest reply))))
     finally (return (reverse reply))))

(defun parse-capability (reply)
  "Function used to parse the result of CMD-CAPABILITY that returns a plist like the following:

\(:ACL2 \(\"UNION\") :ACL \(T) :IDLE \(T) :QUOTA \(T) :SORT \(T) :THREAD
 \(\"ORDEREDSUBJECT\" \"REFERENCES\") :NAMESPACE \(T) :CHILDREN \(T) :UIDPLUS \(T)
 :IMAP4REV1 \(T))

The result should be assigned to the CAPABILITIES slot of an IMAP-SOCKET object."
  (let ((start "* CAPABILITY ")
        (result ()))
    (loop for line in reply
          when (starts-with start line)
          return (loop for capability-str in (split-sequence:split-sequence 
                                              #\Space 
                                              (subseq line (length start)))
                    for capability = (split-sequence:split-sequence #\= capability-str)
                    for key = (intern (string-upcase  (first capability)) :keyword)
                    for value-str = (second capability)
                    do (if value-str
                           (setf (getf result key)
                                 (append (getf result key) (list value-str)))
                           (setf (getf result key) (list t)))))
    result))

(defun parse-list (reply)
  "This function is used to parse the result of CMD-LIST and produces a plist like the following:

\(\(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.Trash\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.maildir\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.Drafts\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.sent-mail\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.Sent\"))"
  (let ((start "* LIST ")
        (*read-eval* nil))
    (loop for line in reply
          when (starts-with start line)
       collect (read-from-string (cl-ppcre:regex-replace 
                                  "^\\*\\sLIST\\s\\((.*?)\\)\\s(\\\"[^\\\"]*\\\")\\s(\\\"[^\\\"]*\\\")"
                                  line
                                  #'(lambda (match &rest registers)
                                      (declare (ignore match))
                                      (let ((start-matches (coerce (nth 4 registers) 'list))
                                            (end-matches (coerce (nth 5 registers) 'list)))
                                        (if (and (equal (length start-matches) (length start-matches))
                                                 (equal (length start-matches) 3))
                                            (format nil 
                                                    "(:name-attributes (~{\"~a\"~^ ~}) :hierarchy-delimiter ~a :name ~a)"
                                                    (split-sequence:split-sequence #\Space (subseq line 
                                                                                                   (first start-matches)
                                                                                                   (first end-matches)))
                                                    (subseq line 
                                                            (second start-matches)
                                                            (second end-matches))
                                                    (subseq line 
                                                            (third start-matches)
                                                            (third end-matches)))
                                            "()"))))))))

(defun parse-select (reply)
  "This function is used to parse the result of CMD-SELECT and produces a plist like the one
produced by CMD-STATUS so a plist with keywords:
:EXISTS :RECENT :UNSEEN :PERMANENTFLAGS :UIDNEX :UIDVALIDITY :FLAGS
is generated"
  (let ((result nil))
    (loop for line in reply
          do (cond
              ((and (starts-with "*" line) (ends-with "EXISTS" line))
               (let ((value (first (cl-ppcre:all-matches-as-strings  "([0-9]+)" 
                                                                     line))))
                 (setf (getf result :exists) (if value
                                                 (parse-integer value)
                                               0))))
              ((and (starts-with "*" line) (ends-with "RECENT" line))
               (let ((value (first (cl-ppcre:all-matches-as-strings  "([0-9]+)" 
                                                                     line))))
                 (setf (getf result :recent) (if value
                                                 (parse-integer value)
                                               0))))
              ((starts-with "* OK [UNSEEN" line)
               (let ((value (first (cl-ppcre:all-matches-as-strings  "([0-9]+)" 
                                                                     line))))
                 (setf (getf result :unseen) (if value
                                                 (parse-integer value)
                                               0))))
              ((starts-with "* OK [PERMANENTFLAGS" line)
               (let ((flags-str
                      (first (cl-ppcre:all-matches-as-strings  "([\\(])(.*)([\\)])" 
                                                               line))))
                 (let ((flags-str-len (length flags-str))) 
                   (when (> flags-str-len 2)
                     (setf (getf result :permanentflags)
                           (split-sequence:split-sequence #\Space 
                                                          (subseq flags-str 1 (- flags-str-len 1))))))))
              ((starts-with "* OK [UIDNEXT" line)
               (let ((value (first (cl-ppcre:all-matches-as-strings  "([0-9]+)" 
                                                                     line))))
                 (setf (getf result :uidnext) (if value
                                                  (parse-integer value)
                                                0))))
              ((starts-with "* OK [UIDVALIDITY" line)
               (let ((value (first (cl-ppcre:all-matches-as-strings  "([0-9]+)" 
                                                                     line))))
                 (setf (getf result :uidvalidity) (if value
                                                      (parse-integer value)
                                                      0))))
              ((starts-with "* FLAGS" line)
               (let ((flags-str
                      (first (cl-ppcre:all-matches-as-strings  "([\\(])(.*)([\\)])" 
                                                               line))))
                 (let ((flags-str-len (length flags-str))) 
                   (when (> flags-str-len 2)
                     (setf (getf result :flags)
                           (split-sequence:split-sequence #\Space 
                                                          (subseq flags-str 1 (- flags-str-len 1))))))))))
    result))

(defun parse-examine (reply)
  "This function behaves exactly the same as PARSE-SELECT"
  (parse-select reply))

(defun %get-id-list (reply)
  (loop for line in reply
       when (starts-with "*" line)
       return (mapcar #'parse-integer (cl-ppcre:all-matches-as-strings "[\\d]+" line))))

(defun parse-search (reply)
  "This function parses the result of a CMD-SEARCH and produces a list of
ID or UID depending by the kind of the command."
  (%get-id-list reply))

(defun parse-sort (reply)
  "This function parses the result of a CMD-SORT and produces a list of
ID or UID depending by the kind of the command."
  (%get-id-list reply))

(defun %read-namespace-list (line)
  (let* ((tokens (reverse (cl-ppcre:all-matches-as-strings "(\\\")([^\\\"]*)(\\\")" line)))
         (key (first tokens))
         (value (second tokens)))
    (list (list (list key value))
          (list (list key value))
          (list (list key value)))))

(defun %read-namespace-namespace (line)
  (let ((*read-eval* nil)
        (*package* (find-package :clonsigna))
        (namespace-list (split-sequence:split-sequence #\Space (subseq line (length "* NAMESPACE ")))))
    (list (ignore-errors (read-from-string (first namespace-list)))
          (ignore-errors (read-from-string (second namespace-list)))
          (ignore-errors (read-from-string (third namespace-list))))))

(defun parse-namespace (reply)
  "This functions is used to parse the result of CMD-NAMESPACE.
It returns a plist where keywords are
:PERSONAL :OTHERS and :SHARED
values are hashtables where key is the reference mailbox name and value is the delimiter."
  (let ((*read-eval* nil)
        (ns-start  "* NAMESPACE ")
        (personal-namespaces (make-hash-table :test 'equal))
        (others-namespaces (make-hash-table :test 'equal))
        (shared-namespaces (make-hash-table :test 'equal)))
    (loop for line in reply
       when (starts-with "*" line)
       do (let* ((ns (if (starts-with ns-start line)
                         (%read-namespace-namespace line)
                         (%read-namespace-list line)))
                 (personal-namespace-kv (first ns))
                 (others-namespace-kv (first ns))
                 (shared-namespace-kv (first ns)))
            (when personal-namespace-kv
              (loop for namespace in personal-namespace-kv
                 do (setf (gethash (first namespace) personal-namespaces) (rest namespace))))
            (when others-namespace-kv
              (loop for namespace in others-namespace-kv
                 do (setf (gethash (first namespace) others-namespaces) (rest namespace))))
            (when shared-namespace-kv
              (loop for namespace in shared-namespace-kv
                 do (setf (gethash (first namespace) shared-namespaces) (rest namespace))))))
    (list :personal personal-namespaces
          :others others-namespaces
          :shaerd shared-namespaces)))

(defun parse-status (reply)
  "This functions is used to parse the result of CMD-STATUS.
It returns a plist that will be then used to update the status of the IMAP-SOCKET object.
A parsed status plist is something like:
\(:MESSAGES 192 :RECENT 2 :UIDNEXT 6793 :UIDVALIDITY 1219826147 :UNSEEN 16)"
  (loop for line in reply
       when (starts-with "* STATUS " line)
       return (let ((*read-eval* nil)
                    (*package* (find-package :keyword)))
                (ignore-errors (read-from-string
                                (first (cl-ppcre:all-matches-as-strings 
                                        "([\\(])(.*)([\\)])" 
                                        line)))))))

(defun %id-uid-flags (line)
  (let ((*read-eval* nil)
        (*package* (find-package :clonsigna))
        (scanner (cl-ppcre:create-scanner 
                  "^\\*\\s([\\d]+)\\sFETCH.*UID\\s([\\d]+).*FLAGS\\s(\\(.*?\\)).*BODYSTRUCTURE\\s(\\(.*\\))\\sBODY.*" 
                  :single-line-mode t)))
    (let ((sentence (cl-ppcre:regex-replace scanner line ":ID \\1 :UID \\2 :FLAGS \\3 :BODYSTRUCTURE \\4")))
      (cl-ppcre:regex-replace "\\((.*)\\)"
                              sentence
                              #'(lambda (match &rest registers)
                                    (declare (ignore match))
                                    (format nil "(~{~a~^ ~})" ;"(~{\"~a\"~^ ~})" 
                                            (split-sequence:split-sequence #\Space;" "
                                                                           (subseq sentence 
                                                                                   (elt (nth 4 registers) 0) 
                                                                                   (elt (nth 5 registers) 0)))))))))

(defun %scan-line-for-headers (line)
  (let* ((scanner (cl-ppcre:create-scanner 
                         (format nil "~a\\s" #\Linefeed)
                         :single-line-mode t)) 
         (sentences (split-sequence:split-sequence #\Linefeed 
                                                   (cl-ppcre:regex-replace-all scanner
                                                                               (remove-if #'(lambda (x) (equal #\Return x)) line)
                                                                               (if (and (starts-with "=?" line) (ends-with "?=" line))
                                                                                   ""
                                                                                   " ")))))
    (loop for sentence in sentences
       for key-val = (multiple-value-bind (ignore scans)
                         (cl-ppcre:scan-to-strings "^([A-Z,a-z,\\-]*:){1}(.*)" sentence)
                       (declare (ignore ignore))
                       (coerce scans 'list))
       when key-val
       collect (intern (let ((keystr (format nil ":~a" (string-upcase (string-trim " " (first key-val)))))) 
                         (subseq keystr 0 (- (length keystr) 1))) :keyword)
       when key-val
       collect (format nil "~s" (string-trim " " (decode-header-value (second key-val)))))))

(defun parse-fetch-fields (reply)
  "This function is used to parse CMD-FETCH-FIELDS result.
See CMD-FETCH-FIELDS documentation for more details."
  (let ((*read-eval* nil))
    (loop for line in reply
       when (starts-with "* " line)
       collect (ignore-errors (read-from-string (format nil "(~a :HEADERS ~a)" 
                                                        (%id-uid-flags line)
                                                        (%scan-line-for-headers line)))))))

(defun parse-thread (reply)
  "This function is used to parse a CMD-THREAD result. a list of threads ID or
UID is returned in descending order."
  (let ((*read-eval* nil)
        (start "* THREAD "))
    (loop for line in reply
       when (starts-with start line)
       return (sort (ignore-errors (read-from-string (format nil "(~a)" (subseq line (length start)))))
                    #'(lambda (a b)
                        (> (reduce #'max (alexandria:flatten a))
                           (reduce #'max (alexandria:flatten b))))))))

(defun most-recents-from-parsed-thread (parsed-thread)
  "This function returns the most recent message in a thread (by greatest ID or UID).
See PARSE-THREAD documentation for more details."
  (mapcar  #'(lambda (x) (reduce #'max x))
                  (mapcar #'alexandria:flatten parsed-thread)))

(defun %multipart-subtype (bodystructure)
  (when (listp (first bodystructure))
    (loop for item in bodystructure
       when (stringp item)
       return item)))

(defun %goto-section (bodystructure section)
  (if section
      (%goto-section (nth (- (first section) 1) bodystructure)
                     (rest section))
      bodystructure))

(defun %parse-body-parameters (parameters)
  (loop for (k v) on parameters by #'cddr
     collect (intern (string-upcase k) :keyword)
     collect v))

(defun %alternative-p (bodystructure)
  (and (listp bodystructure)
       (string-equal "alternative" (loop for item in bodystructure
                                      when (stringp item)
                                      return item))))

(defun %make-text-bodystructure (bodystructure-section position)
  (list (list :mime-type (format nil "~a/~a" 
                                 (nth 0 bodystructure-section) 
                                 (nth 1 bodystructure-section))
              :body-parameters (nth 2 bodystructure-section)
              :body-id (nth 3 bodystructure-section)
              :body-description (nth 4 bodystructure-section)
              :body-encoding (nth 5 bodystructure-section)) 
        position))

(defun %text-position (bodystructure)
  (if (stringp (first bodystructure))
      '(1)
      (loop for item = (first bodystructure) then (first item)
         while (not (stringp item))
         collect 1)))

(defun %alternative-from-text-position (bodystructure text-position)
  (if (%alternative-p bodystructure)
      (list bodystructure '())
      (loop for position on text-position
           for section = (%goto-section bodystructure position)
           when (%alternative-p section)
           return (list section position))))

(defun %text-section-from-alternative (alt-bodystructure pos)
  (if (stringp (first (nth (- pos 1) (first alt-bodystructure))))
      (append (second alt-bodystructure) (list pos))
      (append (second alt-bodystructure) 
              (list pos)
              (loop for sub-bodystructure = (nth (- pos 1) (first alt-bodystructure)) then 
                   (rest sub-bodystructure)
                 collect 1
                 while (listp (first (first sub-bodystructure)))))))

(defun %text-sections-from-bodystructure (bodystructure)
  (let ((result (if (stringp (first bodystructure)) 
                    (%make-text-bodystructure bodystructure '(1))
                    (let* ((first-text-position (%text-position bodystructure))
                           (alternative (%alternative-from-text-position bodystructure first-text-position)))
                      (if alternative
                          (loop for alt-section in (first alternative)
                             while (listp alt-section)
                             for counter = 1 then (+ counter 1)
                             for position = (%text-section-from-alternative alternative counter)
                             collect (%make-text-bodystructure (%goto-section bodystructure position)
                                                               position))
                          (%make-text-bodystructure (%goto-section bodystructure first-text-position)
                                                    first-text-position))))))
    (if (not (listp (caar result)))
        (list result)
        result)))



(defun %cid-sections-from-bodystructure (bodystructure body-list-section)
  (when (listp (first bodystructure))
    (let ((bodystructure-section (%goto-section bodystructure body-list-section)))
      (when (string-equal "related" (%multipart-subtype bodystructure-section))
        (loop for section in (rest bodystructure-section)
           and prev-section = nil then section
           for counter = 1 then (+ counter 1)
           while (listp prev-section)
           when prev-section
           collect (list :data (list :mime-type (format nil "~a/~a" 
                                                        (nth 0 prev-section) 
                                                        (nth 1 prev-section))
                                     :body-parameters (%parse-body-parameters (nth 2 prev-section))
                                     :body-id (nth 3 prev-section)
                                     :body-description (nth 4 prev-section)
                                     :body-encoding (nth 5 prev-section))
                         :section (append body-list-section
                                          (list counter))) into collector
           finally (return (values collector prev-section body-list-section)))))))

(defun %attachment-p (bodystructure)
  (string-equal "attachment" (first (nth 8 bodystructure))))

(defun %attachment-sections-from-bodystructure (bodystructure body-list-section)
  (let ((result ()))
    (when (listp (first bodystructure))
      (loop for current-list-section = body-list-section then (cdr current-list-section)
         for current-bodystructure = (if current-list-section
                                         (%goto-section bodystructure current-list-section)
                                         bodystructure)
         when (string-equal "mixed" (%multipart-subtype current-bodystructure))
         do (loop for attachment-bodystructure in (rest current-bodystructure)
               and prev-attachment-bodystructure = nil then attachment-bodystructure
               for counter = 0 then (+ counter 1)
               while (listp prev-attachment-bodystructure)
               when (and prev-attachment-bodystructure (%attachment-p prev-attachment-bodystructure))
               do (push (list :data (list :mime-type (format nil "~a/~a" 
                                                             (nth 0 prev-attachment-bodystructure) 
                                                             (nth 1 prev-attachment-bodystructure))
                                          :body-parameters (%parse-body-parameters (nth 2 prev-attachment-bodystructure))
                                          :body-id (nth 3 prev-attachment-bodystructure)
                                          :body-description (nth 4 prev-attachment-bodystructure)
                                          :body-encoding (nth 5 prev-attachment-bodystructure))
                              :section (append current-list-section
                                               (list counter)))
                        result))
         while current-list-section))
    result))

(defun %report-sections-from-bodystructure (bodystructure body-list-section)
  (let ((result ()))
    (when (listp (first bodystructure))
      (loop for current-list-section = body-list-section then (cdr current-list-section)
         for current-bodystructure = (if current-list-section
                                         (%goto-section bodystructure current-list-section)
                                         bodystructure)
         when (string-equal "report" (%multipart-subtype current-bodystructure))
         do (loop for report-bodystructure in (rest current-bodystructure)
               and prev-report-bodystructure = nil then report-bodystructure
               for counter = 0 then (+ counter 1)
               while (listp prev-report-bodystructure)
               when prev-report-bodystructure
               do (push (list :data (list :mime-type (format nil "~a/~a" 
                                                             (nth 0 prev-report-bodystructure) 
                                                             (nth 1 prev-report-bodystructure))
                                          :body-parameters (%parse-body-parameters (nth 2 prev-report-bodystructure))
                                          :body-id (nth 3 prev-report-bodystructure)
                                          :body-description (nth 4 prev-report-bodystructure)
                                          :body-encoding (nth 5 prev-report-bodystructure))
                              :section (append current-list-section
                                               (list counter)))
                        result))
         while current-list-section))
    result))

(defun parse-bodystructure (bodystructure)
  "This function parses a body structure returning a plist where keys are:
:BODY :CID :ATTACHMENT :REPORT
See CMD-FETCH-FIELDS form more details"
  (multiple-value-bind (body-list multipart-subtype body-list-section)
      (%text-sections-from-bodystructure bodystructure)
    (declare (ignore multipart-subtype body-list-section))
    (let ((sections-to-search ())
          (cid-list ())
          (attachment-list ())
          (report-list ()))
      (loop for body in body-list
         do (loop for section = (second body) then (butlast section)
               while section
               do (push section sections-to-search)))
      (setf sections-to-search (remove-duplicates sections-to-search :test #'equal))
      (loop for section in sections-to-search
           do (setf cid-list (append cid-list
                                     (%cid-sections-from-bodystructure bodystructure section))
                    attachment-list (append attachment-list
                                            (%attachment-sections-from-bodystructure bodystructure section))
                    report-list (append report-list
                                            (%report-sections-from-bodystructure bodystructure section))))
      (list :body body-list 
            :cid cid-list
            :attachment attachment-list
            :report report-list))))

(defun parse-fetch-body (line &key charset encoding (stream nil))
  "This function parses the result of a CMD-FETCH-BODY.
It may be used to return the decoded body content string or to write it to the STREAM parameter.
See CMD-FETCH-BODY fore more info.


Parameters:
LINE a string contained the body in encoded form.
CHARSET when not null and when STREAM parameter is null it's the default charset for the output
        result. This parameter can be retrieved from the bodystructure of the message.
ENCODING the encoding format parameter. This is usually retrieved from the bodystructure
         of the message.
STREAM when not nil the parsing result is directed to this stream and functiono output
       becomes undefined.

Result:
When STREAM parameter is null, the decoded message string is returned.

See CMD-FETCH-BODY for more info."
  (let* ((scanner (cl-ppcre:create-scanner "(^*.*\\])(.*)(\\))" :single-line-mode t))
         (external-format (and charset (if (stringp charset)
                                           (intern (string-upcase charset) :keyword)
                                           charset)))
         (result (cl-ppcre:regex-replace scanner line "\\2")))
    (if stream
        (%base64-decode result :external-format nil :stream stream)
        (cond ((and (string-equal "base64" encoding) (not stream)) 
               (setf result (%base64-decode result :external-format charset)))
              ((and (or (string-equal "7bit" encoding) (string-equal "8bit" encoding))
                    (not stream))
               (setf result (octets-to-string (string-to-octets result :external-format :latin-1)
                                                     :external-format (or external-format :us-ascii))))
              ((not stream) (setf result (%quoted-decode result 
                                                         :external-format external-format
                                                         :attribute-p nil)))))
    result))