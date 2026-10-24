## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a small update to 0.5.1. `get_institutions()` silently saved nothing
  when the output directory was not writable; it now stops with an informative
  error and reports which files were downloaded.
