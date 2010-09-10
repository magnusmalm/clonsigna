;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: src/commons.lisp $

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


(defun octets-to-string (vector &key (external-format :utf-8) (start 0) end)
  (babel:octets-to-string vector :encoding external-format :start start 
                          :end end))

(defun string-to-octets (string &key (external-format :utf-8) (start 0) end)
  (babel:string-to-octets string :encoding external-format :start start 
                          :end end))

(defun starts-with (start str)
  "Functions that checks if the given STR starts with START regardless char case"
  (and (>= (length str) (length start)) 
       (string-equal start str :end2 (length start))))

(defun ends-with (end str)
  "Functions that checks if the given STR ends with START regardless char case"
  (and (>= (length str) (length end)) 
       (string-equal end str :start2 (- (length str) (length end)))))

(defun %base64-decode (string &key (external-format :utf-8) (stream nil))
  (if stream
      (base64:base64-string-to-stream string :stream stream)
      (if external-format
          (octets-to-string (base64:base64-string-to-usb8-array string) 
                                   :external-format (if (stringp external-format) 
                                                        (intern (string-upcase external-format) :keyword)
                                                        external-format))
          (base64:base64-string-to-usb8-array string))))

(defun %quoted-char-decode (char-code &key (external-format :utf-8))
  (format nil "~a" (octets-to-string (coerce (list char-code) '(vector (unsigned-byte 8))) 
                                            :external-format (if (stringp external-format) 
                                                                 (intern (string-upcase external-format) :keyword)
                                                                 external-format))))

(defun %quoted-decode (string &key (external-format :utf-8) (attribute-p t))
  (let ((sentence (or (and attribute-p (cl-ppcre:regex-replace-all "_" string " "))
                      (cl-ppcre:regex-replace-all "=\\n" string ""))))
    (cl-ppcre:regex-replace-all "=([0-9,A-F]{2})"
                                sentence
                                #'(lambda (match &rest registers)
                                    (declare (ignore match))
                                    (%quoted-char-decode 
                                     (parse-integer (subseq sentence (elt (nth 4 registers) 0) (elt (nth 5 registers) 0)) :radix 16)
                                     :external-format external-format)))))


(defun decode-string (string)
  (if (and (starts-with "=?" string) (ends-with "?=" string))
      (destructuring-bind (external-format encoding sentence) 
          (split-sequence:split-sequence #\? string :start 2 :end (- (length string) 2))
        (cond 
          ((or (string-equal "R" encoding)
               (string-equal "B" encoding)) (%base64-decode sentence :external-format external-format))
          ((string-equal "Q" encoding) (%quoted-decode sentence :external-format external-format))
          (t (error 'encoding-error :message (format nil "Wrong encoding ~a : ~a" encoding string)))))
      string))

(defun decode-header-value (string)
  (cl-ppcre:regex-replace-all "=\\?.*?\\?=" string #'(lambda (match &rest registers)
                                                      (declare (ignore match))
                                                      (decode-string (subseq string (third registers) (fourth registers))))))

(defun convert-to-imap-list (list)
  (format nil "~{~a~^,~}" (alexandria:flatten list)))
