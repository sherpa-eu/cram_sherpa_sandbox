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
                                 (cram-sherpa-robots-common::terrain-name ?terrain)
                                 (assert ?w (object :mesh ?terrain
                                                    ((0 0 50.012736) (0 0 0 1))
                                                    :mass 0.0 :color (0 0.5 0)
                                                    :mesh :terrain))
                                 (assert ?w (object :static-plane floor ((0 0 0) (0 0 0 1))
                                                    :normal (0 0 1) :constant 0))
                                 (assert ?w (object :semantic-map sem-map
                                                    ((0 0 50.012736) (0 0 0 1))
                                                    :color (0 1 0)))
                                 (debug-window ?w))))
  (spawn-actors))

(defun spawn-actors ()
  ;; (push '(:busy-genius "package://cram_sherpa_sandbox/resource/Operator_Point_at.dae" nil)
  ;;       btr::*mesh-files*)
  ;; (push '(:wasp "package://cram_sherpa_sandbox/resource/quadrotor.dae" nil)
  ;;       btr::*mesh-files*)
  ;; (push '(:hawk "package://cram_sherpa_sandbox/resource/iai_rmax.dae" nil)
  ;;       btr::*mesh-files*)
  ;; (push '(:box "package://cram_sherpa_sandbox/resource/Box_COL.stl" nil)
  ;;       btr::*mesh-files*)
  ;; (let ((donkey-urdf (cl-urdf:parse-urdf
  ;;                     (roslisp:get-param "robot_description"))))
  ;;   (push '(:hawk "package://cram_sherpa_sandbox/resource/iai_rmax.dae" nil)
  ;;         btr::*mesh-files*)
  ;;   (cut:force-ll (prolog:prolog `(and
  ;;                                  (bullet-world ?w)
  ;;                                  ;; (assert ?w (object :urdf donkey6 ((-11 0 64.3) (0 0 0 1))
  ;;                                  ;;                    :urdf ,donkey-urdf :mass 10.0
  ;;                                  ;;                    :color (0 0 1.0)))
  ;;                                  (debug-window ?w)))))
  (push '(:hawk "package://cram_sherpa_sandbox/resource/hawk.dae" nil)
        btr::*mesh-files*)
  (push '(:donkey "package://cram_sherpa_sandbox/resource/Sherpa_Donkey.stl" nil)
        btr::*mesh-files*)
  (push '(:wasp "package://cram_sherpa_sandbox/resource/wasp.stl" nil)
          btr::*mesh-files*)
  (cut:force-ll (prolog:prolog `(and
                                 (bullet-world ?w)
                                 (assert ?w (object :mesh hawk:hawk
                                                    ((-9 -4 65.6) (0 0 0 1))
                                                    :mass 12.0 :color (0 0 1)
                                                    :mesh :hawk))
                                 (assert ?w (object :mesh donkey:donkey
                                                    ((-11 2 64.3) (0 0 0 1))
                                                    :mass 12.0 :color (1 0 0)
                                                    :mesh :donkey))
                                 (assert ?w (object :mesh red-wasp:red-wasp
                                                    ((-11 0 64.3) (0 0 0 1))
                                                    :mass 0.5 :color (1 0 0)
                                                    :mesh :wasp))
                                 (assert ?w (object :mesh blue-wasp:blue-wasp
                                                    ((-11 1 64.3) (0 0 0 1))
                                                    :mass 0.5 :color (0 0 1)
                                                    :mesh :wasp))
                                 (debug-window ?w)))))


(prolog:def-fact-group sandbox-facts (cram-robot-interfaces:robot)
  (<- (cram-robot-interfaces:robot red-wasp:red-wasp))
  ;; (<- (cram-robot-interfaces:robot hawk:hawk))
  )

(defun test ()
  (cram-projection:with-projection-environment
             helicopter:helicopter-bullet-projection-environment
    (cpl:top-level

      (cram-sherpa-robots-common:perform (desig:a motion (to set-altitude) (to 5)))
      (let ((?goal (cl-tf:pose->pose-stamped
                    "map"
                    0.0
                    (cl-tf:copy-pose
                     (object-pose 'red-wasp:red-wasp)
                     :origin (cl-tf:copy-3d-vector (cl-tf:origin
                                                    (object-pose 'red-wasp:red-wasp))
                                                   :y 2)))))
        (cram-sherpa-robots-common:perform (desig:a motion (to fly) (to ?goal)))))))
