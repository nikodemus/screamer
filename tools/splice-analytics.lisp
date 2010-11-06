#!/usr/local/bin/sbcl --script

(defvar *analytics*
  "<script type=\"text/javascript\">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', 'UA-3652989-3']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>")

;;; Hey, almost like Perl!
(loop for line = (read-line *standard-input* nil nil)
      while line
      do (when (search "</head>" line)
           (write-line *analytics* *standard-output*))
         (write-line line *standard-output*))
