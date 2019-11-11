# Author:
# Artur Araujo <artur.stat@gmail.com>
#
# Description:
#  Compute absolute error.
#
# Remarks:
#  None.

absoluteError <- function(
  true, # the estimand
  estimate # the estimate
) {
  return( abs(true-estimate) );
} # absoluteError
