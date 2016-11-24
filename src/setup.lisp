;;;
;;; Copyright (c) 2016, Gayane Kazhoyan <kazhoyan@cs.uni-bremen.de>
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions are met:
;;;
;;;     * Redistributions of source code must retain the above copyright
;;;       notice, this list of conditions and the following disclaimer.
;;;     * Redistributions in binary form must reproduce the above copyright
;;;       notice, this list of conditions and the following disclaimer in the
;;;       documentation and/or other materials provided with the distribution.
;;;     * Neither the name of the Institute for Artificial Intelligence/
;;;       Universitaet Bremen nor the names of its contributors may be used to
;;;       endorse or promote products derived from this software without
;;;       specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.

(in-package :sandbox)

(defun setup ()
  (setf prolog::*break-on-lisp-errors* t)
  (setf sem-map-utils::*cached-semantic-map* nil)
  (setf sem-map-utils::*cached-semantic-map-name* nil)
  (roslisp-utilities:startup-ros)
  (push '(:terrain "package://cram_sherpa_sandbox/resource/galtelli_for_ease.dae" nil)
        btr::*mesh-files*)
  (setf btr:*current-bullet-world* nil)
  (cut:force-ll (prolog:prolog `(and
                                 (bullet-world ?w)
                                 (assert ?w (object :mesh terrain ((0 0 50.012736) (0 0 0 1))
                                                    :mass 0.0 :color (0 0.5 0)
                                                    :mesh :terrain))
                                 (assert ?w (object :static-plane floor ((0 0 0) (0 0 0 1))
                                                    :normal (0 0 1) :constant 0))
                                 (assert ?w (object :semantic-map sem-map ((0 0 50.012736)
                                                                           (0 0 0 1))))
                                 (debug-window ?w)))))

