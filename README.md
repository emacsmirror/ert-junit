# ert-junit

ert-junit provides a way to store [ert][ERTMANUAL] test results in a machine
readable format.

[ert][ERTMANUAL] already provides a way to run tests in batch mode
from the command line.  The results are printed on stdout.  ert-junit
does the same, but instead of printing to stdout the test results are
stored in a [JUnit][JUNIT] compatible [XML-file][JUNITXSD].

ert-junit also provides various functions for use from general elisp code.

ert-junit was originally conceived to facilitate [CI][CI] testing on
http://shippable.com.

[ERTMANUAL]: https://www.gnu.org/software/emacs/manual/html_node/ert/index.html "ert online manual"
[JUNIT]: http://junit.org "JUnit Home"
[JUNITXSD]: http://windyroad.com.au/dl/Open%20Source/JUnit.xsd "JUnit xsd"
[CI]: http://en.wikipedia.org/wiki/Continuous_integration "Continous Integration on Wikipedia"


## git repo

ert-junit is hosted on [bitbucket.org][BITBUCKET]:
https://bitbucket.org/olanilsson/ert-junit

Pull requests welcome.

[BITBUCKET]: http://bitbucket.org "BitBucket"

## Issue tracker

https://bitbucket.org/olanilsson/ert-junit/issues

# Documentation

## ert-junit-run-tests-batch-and-exit (&optional selector)

Run all known [ert][ERTMANUAL] tests and exit emacs.  Corresponds to
[`ert-run-tests-batch-and-exit`][ert-b-and-e].  This function is only
meant to be used in Emacs batch mode.  The name of the output file is
read as the first available command line argument.  Exits Emacs with
exit status set to 0 if all test results were as expected, 1 on
unexpected results or 2 for non-test errors. The example below shows
how to use `ert-junit-run-tests-batch-and-exit` from the command line
to execute the tests defined in tests.el.
    
    $ emacs --batch -Q -L tests.el -L ert-junit.el \
        -f ert-junit-run-tests-batch-and-exit results.xml

[ert-b-and-e]: https://www.gnu.org/software/emacs/manual/html_node/ert/Running-Tests-in-Batch-Mode.html#Running-Tests-in-Batch-Mode "ERT: Running Tests in Batch Mode"


## ert-junit-run-tests-batch (result-file &optional selector)

Run ert tests selected by `selector` and write the XML report to
`result-file`.  If `result-file` already exists it is overwritten.
Corresponds to - and indeed calls - `ert-run-tests-batch`.

