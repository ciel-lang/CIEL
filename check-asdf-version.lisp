
(require 'asdf)

(uiop:format! t "ASDF version: ~a~&" (asdf:asdf-version))
(let ((version (asdf:asdf-version)))
  (cond
    ((uiop:version<= version "3.3.4")
     (uiop:quit 1))
    (t
     (uiop:quit 0))))
