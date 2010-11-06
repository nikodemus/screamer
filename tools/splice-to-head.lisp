#!/usr/local/bin/sbcl --script

;;; Hey, almost like Perl!
(loop for line = (read-line *standard-input* nil nil)
      while line
      do (when (search "</head>" line)
           (with-open-file (f (second *posix-argv*))
             (loop for splice = (read-line f nil nil)
                   while splice
                   do (write-line splice *standard-output*))))
         (write-line line *standard-output*))
