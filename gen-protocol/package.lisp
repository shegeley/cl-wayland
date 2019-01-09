(defpackage #:generate-bindings
  (:use :cl #:xmls #:split-sequence)
  (:export #:generate-bindings))

(uiop:define-package #:wayland-scanner
    (:mix #:generate-bindings #:asdf #:cl #:split-sequence)
  (:export #:wl-scanner))
