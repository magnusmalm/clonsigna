;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/classes.lisp $

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

(defgeneric imap-socket-connected-p (imap-socket)
  (:documentation "This function is used to test if an IMAP-SOCKET object
is connected to the server"))

(defgeneric imap-socket-message-id (imap-socket)
  (:documentation "This function returns the current message sequence
number of an IMAP-SOCKET object \(see rfc3501)"))

(defgeneric imap-socket-next-message (imap-socket)
  (:documentation "This function increment the message sequence counter."))

(defgeneric imap-socket-status-changed-p (imap-socket status)
  (:documentation "This function returns a not null value if the IMAP-SOCKET
last stored status is different from the one passed as parameter.
Check is done on MESSAGES and UIDNEXT slot values.

See CMD-STATUS for more details."))

(defgeneric imap-socket-update-status (imap-socket status)
  (:documentation "This function updates MESSAGES and UIDNEXT slot values
of an IMAP-SOCKET object form the STATUS parameter.
This function is often called after a call to IMAP-SOCKET-STATUS-CHANGED-P
has returned a not null value.

See CMD-STATUS for more details."))

(defgeneric imap-socket-send-command (imap-socket command &rest args)
  (:documentation "This function is used to send generic commands to an
IMAP server. It's mainly used by all CMD-* command wrappers."))

(defgeneric imap-socket-has-capability (imap-socket capability)
  (:documentation "This function returns nil or a list of capabilities that
the IMAP-SOCKET has stored from the server.

CAPABILITY is a capability keyword.


See CMD-CAPABILITY for more details"))

(defgeneric %read-line-continuation (imap-socket line))

(defgeneric %read-line (imap-socket))

(defgeneric %eol-p (imap-socket ch))

(defgeneric imap-socket-read-reply (imap-socket)
  (:documentation "This function is a generic function that is used to read a reply from
an IMAP server after a command has been called ono the server.
Ususally you will not need it but use the CMD-* command wrappers."))

(defgeneric imap-socket-flush-buffer (imap-socket)
  (:documentation "This method simply flush server messages discarding the reply.
Ususally you will not need it but use the CMD-* command wrappers."))

(defclass imap-socket ()
  ((socket :accessor imap-socket-socket :initarg :socket
           :documentation "Holds the socket stream")
   (remote-host :accessor imap-socket-remote-host :initarg :remote-host
                :documentation "IMAP server address")
   (remote-port :accessor imap-socket-remote-port :initarg :remote-port
                :documentation "IMAP server port")
   (counter :reader imap-socket-counter :initform 0)
   (crlf :accessor imap-socket-crlf-p :initform t)
   (capabilities :accessor imap-socket-capabilities :initform (format nil "~a" #\Linefeed)
                 :documentation "Stores the capabilities of the imap server after connection.")
   (line-separator :accessor imap-socket-line-separator :initform :line-separator)
   (personal-namespaces :accessor imap-socket-personal-namespaces :initform (make-hash-table :test 'equal))
   (others-namespaces :accessor imap-socket-others-namespaces :initform (make-hash-table :test 'equal))
   (shared-namespaces :accessor imap-socket-shared-namespaces :initform (make-hash-table :test 'equal))
   (messages :reader imap-socket-messages :initform 0
             :documentation "Current messages number of the 'inbox' mailbox")
   (uidnext :reader imap-socket-uidnext :initform 0
            :documentation "Current uidnext of the 'inbox' mailbox")
   (ssl :reader imap-socket-ssl-p :initarg :ssl-p))
  (:default-initargs :socket nil :remote-host "127.0.0.1" :remote-port 143 :ssl-p nil)
  (:documentation "This class holds the connection to an IMAP server.
It's usually instantiated by the MAKE-IMAP function and activated by CMD-CONNECT."))

(defmethod imap-socket-connected-p ((is imap-socket))
  (and (imap-socket-socket is)
       (iolib:socket-connected-p (imap-socket-socket is))))

(defmethod imap-socket-message-id ((is imap-socket))
  (format nil "a~5,'0d" (imap-socket-counter is)))

(defmethod imap-socket-next-message ((is imap-socket))
  (setf (slot-value is 'counter) (+ 1 (imap-socket-counter is))))

(defmethod imap-socket-status-changed-p ((is imap-socket) status)
  (not (and (equal (imap-socket-messages is)
                   (getf status :messages))
            (equal (imap-socket-uidnext is)
                   (getf status :uidnext)))))

(defmethod imap-socket-update-status ((is imap-socket) status)
  (setf (slot-value is 'messages) (getf status :messages (slot-value is 'messages))
        (slot-value is 'uidnext) (getf status :messages (slot-value is 'uidnext))))

(defmethod imap-socket-send-command ((is imap-socket) command &rest args)
  (imap-socket-flush-buffer is)
  (let ((s (imap-socket-socket is))
        (line-term (format nil "~a~a" #\Return #\Linefeed))
        (format-string "~a ~a ~{~a~^ ~}~a"))
    (imap-socket-next-message is)
    (when +debug+
      (format t format-string (imap-socket-message-id is) (symbol-name command)  args line-term))
    (format s format-string (imap-socket-message-id is) (symbol-name command)  args line-term)
    (finish-output s)))

(defmethod imap-socket-has-capability ((is imap-socket) capability)
  (getf (imap-socket-capabilities is) capability))

(defmethod %read-line-continuation ((is imap-socket) line)
  (let* ((s (imap-socket-socket is))
         (continuation (first (cl-ppcre:all-matches-as-strings "({\\d+}$)" line)))
         (continuation-bytes (and continuation (parse-integer (first (cl-ppcre:all-matches-as-strings "\\d+" continuation))))))
    (cl-ppcre:regex-replace-all (format nil "~a~a" #\Return #\Linefeed) 
                                (or (and continuation-bytes
                                         (format nil
                                                 "~a~%~a"
                                                 (subseq line 0 (- (length line) (length continuation)))
                                                 (sb-ext:octets-to-string (coerce (loop for i from 1 to continuation-bytes
                                                                                     for byte = (read-byte s nil)
                                                                                     when byte
                                                                                     collect byte)
                                                                                  '(vector (unsigned-byte 8)))
                                                                          :external-format :latin-1)))
                                    line)
                                (format nil "~%"))))


(defmethod %eol-p ((is imap-socket) byte)
  (when (or (equal #|#\Linefeed|# 10 byte)
          (equal #|#\Return|# 13 byte))
      (let* ((s (imap-socket-socket is))
             (next-ch (read-char-no-hang s nil :eof))
             (result (or (null next-ch)
                         (equal :eof next-ch)
                         (equal #\Linefeed next-ch)
                         (equal #\Return next-ch))))
        (unless result
          (unread-char next-ch s))
        result)))

;;To avoid the following exception
;;Illegal :ASCII character starting at position 3834.
;;because stream is returning null inside a message for some misterious reason
(defmethod %read-line ((is imap-socket))
  (let* ((s (imap-socket-socket is))
         (ch-list (loop for byte = (read-byte s nil :eof)
                     while (not (%eol-p is byte))
                     when byte
                     collect byte)))
    (sb-ext:octets-to-string (coerce (if (equal 10 #|#\Linefeed|# (first (last ch-list))) 
                                         (butlast ch-list) 
                                         ch-list) 
                                     '(vector (unsigned-byte 8)))
                             :external-format :latin-1)))

(defmethod imap-socket-read-reply ((is imap-socket))
  (let* ((message-id (imap-socket-message-id is))
         (result nil)
         (result-op nil))
    (loop for continue-p = t then (not (starts-with message-id line))
       for line = (and continue-p (%read-line-continuation is (%read-line is)))
       while continue-p
       do (if (starts-with message-id line)
             (setf result-op (%parse-tagged-line line message-id))
             (push line result))
       finally (return (cond
                         ((string-equal "no" (first result-op)) (error 'operational-error :message (second result-op)))
                         ((string-equal "bad" (first result-op)) (error 'server-error :message (second result-op)))
                         (t (values (%parse-reply-result (imap-socket-line-separator is) (reverse result))
                                    (first result-op)
                                    (second result-op))))))))

(defmethod imap-socket-flush-buffer ((is imap-socket))
  (let ((result (loop for ch = (read-char-no-hang (clonsigna:imap-socket-socket is) nil :eof)
                   while (and ch (not (equal ch :eof)))
                   collect ch)))
    (coerce (if (equal #\Linefeed (first (last result))) 
                (butlast result) 
                result) 
            'string)))

(defun make-imap (&key (host "127.0.0.1") (port 143) (crlf-p t) (ssl-p nil))
  "Creates an IMAP-SOCKET object

Paratmers:
HOST Remote IMAP server address
PORT Remote IMAP server port
CRLF-P IMAP line termination \(it should always be true as spec. by rfc3501)
SSL-P When not null it instructs the client to connect to the server via SSL connection."
  (let ((result (make-instance 'imap-socket
                               :remote-host host
                               :remote-port port
                               :ssl-p ssl-p)))
    (setf (imap-socket-line-separator result) (or (and crlf-p (format nil "~a~a" #\Return #\Linefeed))
                                                  (format nil "~a" #\Linefeed))
          (imap-socket-crlf-p result) crlf-p)
    result))

(defclass bodystructure ()
  ((body :accessor bodystructure-body-list :initarg :body
         :documentation "Holds the body elements in a list of STRUCTURE-ELEMENT objects.")
   (cid :accessor bodystructure-cid-list :initarg :cid
        :documentation "Holds the reference elements of a body element for inline content \(as images) 
in a list of STRUCTURE-ELEMENT objects.")
   (attachment :accessor bodystructure-attachment-list :initarg :attachment
               :documentation "Holds the attachment elements in a list of STRUCTURE-ELEMENT objects.")
   (report :accessor bodystructure-report-list :initarg :report
           :documentation "Holds the repot elements in a list of STRUCTURE-ELEMENT objects."))
  (:documentation "After a bodystructure \(rfc3501, rfc2822) has been parsed by PARSE-BODYSTRUCTURE function,
this class maps the parsed plist.
It is usually instantiated by MAKE-BODYSTRUCTURE function.

See CMD-FETCH-FIELDS, PARSE-BODYSTRUCTURE, MAKE-BODYSTRUCTURE"))

(defclass structure-element ()
  ((mime-type :accessor structure-element-mime-type :initarg :mime-type
              :documentation "Holds the element mime-type")
   (body-parameters :accessor structure-element-body-parameters :initarg :body-parameters
                    :documentation "Holds body parameters in a plist structure \(you'll usually get the :charset from here)")
   (body-id :accessor structure-element-body-id :initarg :body-id
            :documentation "Holds the body id \(rfc3501#7.4.2)")
   (body-description :accessor structure-element-body-description :initarg :body-description
                     :documentation "Holds the body description \(rfc3501#7.4.2)")
   (body-encoding :accessor structure-element-body-encoding :initarg :body-encoding
                  :documentation "Holds the body encoding type \(rfc3501#7.4.2, rfc2822)")
   (section :accessor structure-element-section :initarg :section
            :documentation "Holds a list of numbers that represents the element section.
This list should then be passed to CMD-FETCH-BODY to get the current element."))
  (:documentation "This object may hold body, reference, attachment and message elements and
should not be directly instantiated. \(rfc3501#7.4.2)

See CMD-FETCH-FIELDS, PARSE-BODYSTRUCTURE, MAKE-BODYSTRUCTURE"))

(defun %make-structure-element (element-plist)
  (let ((data #|(getf element-plist :data)|# (first element-plist))
        (section #|(getf element-plist :data)|# (second element-plist)))
    (make-instance 'structure-element
                   :mime-type (getf data :mime-type)
                   :body-parameters (getf data :body-parameters)
                   :body-id (getf data :body-id)
                   :body-description (getf data :body-description)
                   :body-encoding (getf data :body-encoding)
                   :section #|(getf element-plist :section)|# section)))

(defun make-bodystructure (bodystructure-plist)
  "This function creates a BODYSTRUCTURE object on a PARSE-BODYSTRUCTURE result.
See CMD-FETCH-FIELDS for more details."
  (make-instance 'bodystructure
                 :body (loop for body in (getf bodystructure-plist :body)
                          collect (%make-structure-element body))
                 :cid (loop for cid in (getf bodystructure-plist :cid)
                         collect (%make-structure-element cid))
                 :attachment (loop for attachment in (getf bodystructure-plist :attachment)
                                collect (%make-structure-element attachment))
                 :report (loop for report in (getf bodystructure-plist :report)
                               collect (%make-structure-element report))))