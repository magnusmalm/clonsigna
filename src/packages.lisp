;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/packages.lisp $

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

(in-package :cl-user)

(defpackage :clonsigna
  (:use :cl)
  (:documentation "Simple IMAP4rev1 client library")
  (:export #:+debug+
           #:starts-with
           #:ends-with
           #:operational-error
           #:server-error
           #:most-recents-from-parsed-thread
           #:parse-capability
           #:parse-list
           #:parse-lsub
           #:parse-select
           #:parse-examine
           #:parse-namespace
           #:parse-status
           #:parse-search
           #:parse-fetch-fields
           #:parse-fetch-body
           #:parse-thread
           #:parse-sort
           #:parse-bodystructure
           #:imap-socket
           #:imap-socket-connected-p
           #:imap-socket-socket
           #:imap-socket-remote-host
           #:imap-socket-remote-port
           #:imap-socket-crlf-p
           #:imap-socket-capabilities
           #:imap-socket-messages
           #:imap-socket-uidnext
           #:imap-socket-status-changed-p
           #:imap-socket-update-status
           #:imap-socket-has-capability
           #:imap-socket-read-reply
           #:imap-socket-flush-buffer
           #:imap-socket-send-command
           #:make-imap
           #:bodystructure
           #:bodystructure-body-list
           #:bodystructure-cid-list
           #:bodystructure-attachment-list
           #:make-bodystructure
           #:structure-element
           #:structure-element-mime-type
           #:structure-element-body-parameters
           #:structure-element-body-id
           #:structure-element-body-description
           #:structure-element-body-encoding
           #:structure-element-section
           #:cmd-connect
           #:cmd-logout
           #:cmd-capability
           #:cmd-login
           #:cmd-select
           #:cmd-examine
           #:cmd-create
           #:cmd-delete
           #:cmd-rename
           #:cmd-namespace
           #:cmd-list
           #:cmd-subscribe
           #:cmd-unsubscribe
           #:cmd-lsub
           #:cmd-append
           #:cmd-status
           #:cmd-check
           #:cmd-close
           #:cmd-expunge
           #:cmd-search
           #:cmd-fetch
           #:cmd-fetch-fields
           #:cmd-fetch-body
           #:cmd-thread
           #:cmd-sort
           #:cmd-starttls
           #:cmd-authenticate-plain
           #:cmd-noop
	   #:cmd-store
	   #:cmd-idle
	   #:cmd-done))

(in-package :clonsigna)

(defvar +debug+ nil
  "When true it outputs to the standard output the commands sent to the server.")


#|
Api generation:

(asdf:oos 'asdf:load-op :cl-api)
(api-gen :clonsigna #P"/tmp/"
:exclude-func (lambda (s) (or (starts-with "imap" (format nil "~a" s))
(starts-with "bodystructure" (format nil "~a" s))
(starts-with "structure" (format nil "~a" s)))))

|#
