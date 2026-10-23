## R CMD check results

0 errors | 0 warnings | 0 notes

* This is an update fixing data import bugs in `tidy_institutions()` and `get_institutions()`.

* Resubmission: the previous submission had a NOTE on Debian for the
  `get_balance_sheets()` example taking slightly over 5s elapsed time
  (CPU time was 0.26s; the delay is the download from the Bacen website).
  That example is now wrapped in `\donttest{}`.
