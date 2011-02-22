;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/commands.lisp $

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

(defgeneric cmd-connect (imap-socket) 
  (:documentation "This function is required to connect an IMAP-SOCKET object to an IMAP server.
Depending on the passed parameter it can connect via IMAP or IMAP over ssl.
After the connection has been succeded. The capabilities slot of the parameter object passed is filled.

Return: Returns plist of capabilities got from the server like the following:
\(:STARTTLS \(T) :ACL2 \(\"UNION\") :ACL \(T) :IDLE \(T) :QUOTA \(T) :SORT \(T) :THREAD
 \(\"ORDEREDSUBJECT\" \"REFERENCES\") :NAMESPACE \(T) :CHILDREN \(T) :UIDPLUS \(T)
 :IMAP4REV1 \(T))"))

(defgeneric cmd-logout (imap-socket)
  (:documentation "Performs an IMAP LOGOUT \(rfc3501) command and so after the connection will be 
closed.

Returns a triplet of values: 
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply"))

(defgeneric cmd-login (imap-socket login password)
  (:documentation "Performs an IMAP LOGIN \(rfc3501) command.

On success the client will be on an authenticated state and the return will be a triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-select (imap-socket mailbox-name)
  (:documentation "Performs an IMAP SELECT \(rfc3501) command.
MAILBOX-NAME parameter is the mailbox name to select\(can be a pathname).
The result can be parsed by the function PARSE-SELECT.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-examine (imap-socket mailbox-name)
  (:documentation "Performs an IMAP EXAMINE \(rfc3501) command.
MAILBOX-NAME parameter is the mailbox name to examine\(can be a pathname).

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-create (imap-socket mailbox-name)
  (:documentation "Performs an IMAP CREATE \(rfc3501) command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-delete (imap-socket mailbox-name)
  (:documentation "Performs an IMAP DELETE \(rfc3501) command.
MAILBOX-NAME parameter is the mailbox name to delete\(can be a pathname).

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-rename (imap-socket mailbox-name new-mailbox-name)
  (:documentation "Performs an IMAP RENAME \(rfc3501) command.
MAILBOX-NAME parameter is the original mailbox name \(can be a pathname).
NEW-MAILBOX-NAME is the new mailbox name \(can be a pathname).

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-capability (imap-socket)
  (:documentation "Performs an IMAP CAPABILITY \(rfc3501) command.
The result should usually be passed to the PARSE-CAPABILITY method that transforms it into 
a plist that will be then used to update the capabilities slot of the IMAP-SOCKET object.
A parsed capability plist is something like:
\(:ACL2 \(\"UNION\") :ACL \(T) :IDLE \(T) :QUOTA \(T) :SORT \(T) :THREAD
 \(\"ORDEREDSUBJECT\" \"REFERENCES\") :NAMESPACE \(T) :CHILDREN \(T) :UIDPLUS \(T)
 :IMAP4REV1 \(T))

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-list (imap-socket reference-name mailbox-name)
  (:documentation "Performs an IMAP LIST \(rfc3501) command.
The result should usually be passed to the PARSE-LIST method that transforms it into a 
list of plist like the following:

\(\(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.Trash\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.maildir\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.Drafts\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.sent-mail\")
 \(:NAME-ATTRIBUTES \(\"HasNoChildren\") :HIERARCHY-DELIMITER \".\" :NAME
  \"INBOX.Sent\"))

REFERENCE-NAME the reference mailbox name \(can be a pathname).
MAILBOX-NAME the mailbox name \(can be a pathname), or wildcards.

If a parameter is null, it's converted to an empty string.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-namespace (imap-socket)
  (:documentation "Performs an IMAP NAMESPACE \(rfc2342) command.
The result should usually be passed to the PARSE-NAMESPACE method that transforms it 
into a plist where keywords are
:PERSONAL :OTHERS and :SHARED
values are hashtables where key is the reference mailbox name and value is the delimiter.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-subscribe (imap-socket mailbox-name)
  (:documentation "Performs an IMAP SUBSCRIBE \(rfc3501) command.
MAILBOX-NAME parameter is the mailbox name to subscribe to\(can be a pathname).

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-unsubscribe (imap-socket mailbox-name)
  (:documentation "Performs an IMAP UNSUBSCRIBE \(rfc3501) command.
MAILBOX-NAME parameter is the mailbox name to unsubscribe to \(can be a pathname).

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-append (imap-socket mailbox-name message &key flags date)
  (:documentation "Performs an IMAP APPEND \(rfc3501) command.
MAILBOX-NAME The mailbox name where to append the message \(can be a pathname)
MASSAGE The message string in rfc2822 format.
FLAGS A list of strings that are the flags for the message to append.
DATE The date of the message (rfc2822)

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-status (imap-socket mailbox-name &optional status-data-item-names)
  (:documentation "Performs an IMAP STATUS \(rfc3501) command.
The result should usually be passed to the PARSE-STATUS method that transforms it into
a plist that will be then used to update the status of the IMAP-SOCKET object.
A parsed status plist is something like:
\(:MESSAGES 192 :RECENT 2 :UIDNEXT 6793 :UIDVALIDITY 1219826147 :UNSEEN 16)

STATUS-DATA-ITEM-NAMES List of keyword for status data item names.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-check (imap-socket)
  (:documentation "Performs an IMAP CHECK \(rfc3501) command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-close (imap-socket)
  (:documentation "Performs an IMAP CLOSE \(rfc3501) command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-expunge (imap-socket)
  (:documentation "Performs an IMAP EXPUNGE \(rfc3501) command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-search (imap-socket &key charset criteria uid-p)
  (:documentation "Performs an IMAP SEARCH or UID SEARCH \(rfc3501) command.
The result should usually be passed to the PARSE-SEARCH that returns a list of message numbers
that match the CRITERIA

CAHRSET An optional parameter specifying the charset ot the CRITERIA
CRITERIA The searching criteria as specified in rfc3501.
UID-P When not null performs an UID SEARCH instead of a simple SEARCH command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-fetch (imap-socket sequence-number &key criteria uid-p)
  (:documentation "Performs an IMAP FETCH or UID FETCH \(rfc3501) command.

SEQUENCE-NUMBER Sequence set as defined in rfc3501 ir a number or a list of numbers
CRITERIA The fetching criteria as specified in rfc3501.
UID-P When not null performs an UID FETCH instead of a simple FETCH command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-fetch-fields (imap-socket sequence-number &key fields uid-p)
  (:documentation "Performs an IMAP FETCH \(FIELDS param) or UID FETCH \(FIELDS param) \(rfc3501) command.
The result is usually passed to the PARSE-FETCH-FIELDS function that produces a plist like the follofing.
\(\(:ID 1 :UID 7 :FLAGS \(SEEN) :BODYSTRUCTURE
  \(\"text\" \"plain\" \(\"charset\" \"utf-8\") NIL NIL \"8bit\" 298 11 NIL NIL NIL)
  :HEADERS
  \(:MESSAGE-ID \"<200808270928.m7R9SDvK008347@apache-5.foo.com>\" :DATE
   \"Wed, 27 Aug 2008 12:28:08 +0300\" :FROM
   \"\\\"StartCom CertMaster\\\" <certmaster@foo.com>\" :TO \"destination@yourserver.com\"
   :SUBJECT \"Your Authentication Code, 27 Aug 2008 12:28\")))

where :HEADERS is a plist of headers fields keywords and values where values are parsed into plain strings
\(following rfc2822 specs).

The :BODYSTRUCTURE value can pe passed to PARSE-BODYSTRUCETURE function that produces a simplification of the 
bodystructure as a plist like this:
\(:BODY body-list :CID cid-list :ATTACHMENT attachment-list :REPORT report-list)
each property value is in turn a plist.
After the bodystructure has been parsed, the result can be passed to function MAKE-BODYSTRUCTURE function that
instantiates a bodystructure instance.

SEQUENCE-NUMBER Sequence set as defined in rfc3501 ir a number or a list of numbers
CRITERIA The fetching criteria as specified in rfc3501.
UID-P When not null performs an UID FETCH instead of a simple FETCH command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-fetch-body (imap-socket sequence-number &key section uid-p)
  (:documentation "Performs an IMAP FETCH BODY[<section>] or UID FETCH BODY[<section>] \(rfc3501) command.
The element result list is usually passed to the PARSE-FETCH-BODY function that decodes the reply.

SEQUENCE-NUMBER Sequence set as defined in rfc3501 ir a number or a list of numbers
SECTION The section to fetch that can passed in list of number form and internally transformed to 
the section \(as described in rfc3501).
UID-P When not null performs an UID FETCH instead of a simple FETCH command.

Both for CMD-FETCH-BODY and for PARSE-FETCH-BODY a calls to 
CMD-FETCH-FIELDS -> PARSE-FETCH-FIELDS -> PARSE-BODYSTRUCTURE -> MAKE-BODYSTRUCTURE usually happen.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-thread (imap-socket &key thread criteria uid-p charset)
  (:documentation "Performs an IMAP THREAD or UID THREAD \(rfc5256) command.
The result list is usually passed to the PARSE-THREAD function that decodes the reply.
The parsed result is arranged in descending order by ID or UID.

THREAD is the threading algorithm that you can find inspecting the server capabilities.
CRITERIA is the searching criteria as specified in rfc5256.
UID-P when not nil performs an UID THREAD command.
CHARSET is the charset specification for the criteria.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-sort (imap-socket &key sort criteria uid-p charset)
  (:documentation "Performs an IMAP SORT or UID SORT \(rfc5256) command.
The result list is usually passed to the PARSE-SORT function that decodes the reply
and returns a list of ID or UID with the given sort program.

SORT is the sort program described in rfc5256.
CRITERIA is the searching criteria as specified in rfc5256.
UID-P when not nil performs an UID THREAD command.
CHARSET is the charset specification for the criteria.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-noop (imap-socket)
  (:documentation "Performs an IMAP NOOP \(rfc3501) command.
This command is usually used as a sort of `keep alive` and should be 
periodically called by a client application.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-starttls (imap-socket)
  (:documentation "Performs an IMAP STARTTLS \(rfc3501) command and performs
the negotiation with the server.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric cmd-authenticate-plain (imap-socket login password)
  (:documentation "Performs an IMAP AUTHENTICATE PLAIN \(rfc3501 and rfc4616) command as an
alternative to LOGIN command.

On success returns triplet of values:
1) reply list of strings
2) result-op alway OK
3) the result-op description \(what comes after OK in the IMAP reply.

On failure a condition of type OPERATIONAL-ERROR or SERVER-ERROR will be signaled."))

(defgeneric %pathname-to-mailbox (imap-socket pathname &optional ends-with-separator-p))

(defmethod %pathname-to-mailbox ((is imap-socket) pathname &optional (ends-with-separator-p nil))
  (if (not (pathnamep pathname))
      pathname
      (progn
        (unless (imap-socket-personal-namespaces is)
          (let ((namespaces (parse-namespace (if (imap-socket-has-capability is :namespace)
                                                 (cmd-namespace is)
                                                 (cmd-list is nil nil)))))
            (setf (imap-socket-personal-namespaces is) (first namespaces)
                  (imap-socket-others-namespaces is) (second namespaces)
                  (imap-socket-shared-namespaces is) (third namespaces))))
        (let* ((folders (rest (pathname-directory pathname)))
               (first-lvl-folder (first folders))
               (separator (or (gethash first-lvl-folder (imap-socket-personal-namespaces is))
                              (gethash first-lvl-folder (imap-socket-others-namespaces is))
                              (gethash first-lvl-folder (imap-socket-shared-namespaces is))
                              (gethash "" (imap-socket-personal-namespaces is))
                              "/"))
               (control-string (if ends-with-separator-p
                                   (format nil "~~{~~a~~^~a~~}~a" separator separator)
                                   (format nil "~~{~~a~~^~a~~}" separator))))
          (format nil control-string folders)))))

(defmethod cmd-logout ((is imap-socket))
  (imap-socket-send-command is :logout)
  (imap-socket-read-reply is))


(defmethod cmd-connect ((is imap-socket))
  (let ((iolib-stream (iolib:make-socket :address-family :internet
                                                       :type :stream
                                                       :connect :active
                                                       :external-format '(:latin-1)
                                                       :remote-host (imap-socket-remote-host is)
                                                       :remote-port (imap-socket-remote-port is))))
    (setf (imap-socket-socket is) (if (imap-socket-ssl-p is)
                                      (cl+ssl:make-ssl-client-stream (iolib:socket-os-fd iolib-stream)
                                                                     :external-format :latin-1)
                                      iolib-stream)))
  (setf (imap-socket-capabilities is) (parse-capability (cmd-capability is))))

  (defmethod cmd-login ((is imap-socket) login password)
    (imap-socket-send-command is :login login password)
    (imap-socket-read-reply is))

(defmethod cmd-select ((is imap-socket) mailbox-name)
  (imap-socket-send-command is :select (%pathname-to-mailbox is mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-noop ((is imap-socket))
  (imap-socket-send-command is :noop)
  (imap-socket-read-reply is))


(defmethod cmd-examine ((is imap-socket) mailbox-name)
  (imap-socket-send-command is :examine (%pathname-to-mailbox is mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-create ((is imap-socket) mailbox-name)
  (imap-socket-send-command is :create (%pathname-to-mailbox is mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-delete ((is imap-socket) mailbox-name)
  (imap-socket-send-command is :delete (%pathname-to-mailbox is mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-rename ((is imap-socket) mailbox-name new-mailbox-name)
  (imap-socket-send-command is :rename
                (%pathname-to-mailbox is mailbox-name)
                (%pathname-to-mailbox is new-mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-capability ((is imap-socket))
  (imap-socket-send-command is :capability)
  (imap-socket-read-reply is))


(defmethod cmd-namespace ((is imap-socket))
  (imap-socket-send-command is :namespace)
  (imap-socket-read-reply is))

(defmethod cmd-list ((is imap-socket) reference-name mailbox-name)
  (imap-socket-send-command is :list
                (or (and reference-name (%pathname-to-mailbox is reference-name t)) "\"\"")
                (or (and mailbox-name (%pathname-to-mailbox is mailbox-name)) "\"\""))
  (imap-socket-read-reply is))

(defmethod cmd-subscribe ((is imap-socket) mailbox-name)
  (imap-socket-send-command is :subscribe (if (pathnamep mailbox-name) (%pathname-to-mailbox is mailbox-name) mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-unsubscribe ((is imap-socket) mailbox-name)
  (imap-socket-send-command is :unsubscribe (if (pathnamep mailbox-name) (%pathname-to-mailbox is mailbox-name) mailbox-name))
  (imap-socket-read-reply is))

(defmethod cmd-lsub ((is imap-socket) reference-name mailbox-name)
  (imap-socket-send-command is :lsub
                (or (and reference-name (%pathname-to-mailbox is reference-name t)) "\"\"")
                (or (and mailbox-name (%pathname-to-mailbox is mailbox-name)) "\"\""))
  (imap-socket-read-reply is))

;;TODO move command continuation
(defmethod cmd-append ((is imap-socket) mailbox-name message &key (flags nil) (date nil))
  (imap-socket-send-command is :append
                (%pathname-to-mailbox is mailbox-name)
                (or (and flags (format nil "~a" flags)) "")
                (or date "")
                (format nil "{~d}" (length message))) ;; we don't send the message as is but we'll transmit it as a continuation
  (multiple-value-bind (reply result-op result-op-description) 
      (%read-line is)
    (if (starts-with "+" (first reply))
        (progn
          (let ((s (imap-socket-socket is)))
            (format s "~a~a~a" message #\Return #\Linefeed)
            (finish-output s))
          (imap-socket-read-reply is))
        (values
         reply
         result-op
         result-op-description))))

(defmethod cmd-status ((is imap-socket) mailbox-name &optional (status-data-item-names '(:messages :recent :unseen :uidnext :uidvalidity)))
  (imap-socket-send-command is :status
                (%pathname-to-mailbox is mailbox-name)
                (format nil "~a" status-data-item-names))
  (imap-socket-read-reply is))


(defmethod cmd-check ((is imap-socket))
  (imap-socket-send-command is :check)
  (imap-socket-read-reply is))

(defmethod cmd-close ((is imap-socket))
  (imap-socket-send-command is :close)
  (imap-socket-read-reply is))

(defmethod cmd-expunge ((is imap-socket))
  (imap-socket-send-command is :expunge)
  (imap-socket-read-reply is))

(defmethod cmd-search ((is imap-socket) &key (charset nil) criteria (uid-p nil))
  (if uid-p
      (imap-socket-send-command is :uid :search (if charset
                                               (format nil "CHARSET ~a" charset)
                                               "")
                                criteria)
      (imap-socket-send-command is :search (if charset
                                               (format nil "CHARSET ~a" charset)
                                               "")
                                criteria))
  (imap-socket-read-reply is))

(defmethod cmd-fetch ((is imap-socket) sequence-number &key criteria (uid-p nil))
  (if uid-p
      (imap-socket-send-command is :uid :fetch sequence-number criteria)
      (imap-socket-send-command is :fetch sequence-number criteria))
  (imap-socket-read-reply is))

(defmethod cmd-fetch-fields ((is imap-socket) 
                             sequence-number 
                             &key 
                             (fields '(date from to cc bcc subject message-id in-reply-to references))
                             (uid-p nil))
  (cmd-fetch is sequence-number :criteria (format nil "(uid flags bodystructure body[header.fields (~{~a~^ ~})])" fields) :uid-p uid-p))

(defmethod cmd-fetch-body ((is imap-socket) sequence-number &key section uid-p)
  (cmd-fetch is sequence-number 
             :criteria (if (listp section)
                           (format nil "body[~{~a~^.~}]" section)
                           (format nil "body[~a]" section))
             :uid-p uid-p))

(defmethod cmd-thread ((is imap-socket) &key (thread "references") (criteria "all") (uid-p t) (charset "utf-8"))
  (if uid-p
      (imap-socket-send-command is :uid :thread thread charset criteria)
      (imap-socket-send-command is :thread thread charset criteria))
  (imap-socket-read-reply is))

(defmethod cmd-sort ((is imap-socket) &key (sort "ID") (criteria "all") (uid-p t) (charset "utf-8"))
  (if uid-p
      (imap-socket-send-command is :uid :sort sort charset criteria)
      (imap-socket-send-command is :sort sort charset criteria))
  (imap-socket-read-reply is))

(defmethod cmd-starttls ((is imap-socket))
  (imap-socket-send-command is :starttls)
  (multiple-value-bind (reply result-op result-op-description)
      (imap-socket-read-reply is)
    (setf (imap-socket-socket is) (cl+ssl:make-ssl-client-stream (iolib:socket-os-fd (imap-socket-socket is))
                                                                 :external-format :latin-1))
    (values reply result-op result-op-description)))

(defmethod cmd-authenticate-plain ((is imap-socket) login password)
  (let ((encoded-auth (base64:string-to-base64-string (format nil "~c~a~c~a" #\Nul login #\Nul password))))
    (imap-socket-send-command is :authenticate "PLAIN" encoded-auth)
    (imap-socket-read-reply is)))
