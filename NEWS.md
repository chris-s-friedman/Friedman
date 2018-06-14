# Friedman 0.4.0

* Added a `NEWS.md` file to track changes to the package.
* `str_detect_approx()`, `str_which_approx()`, and `str_subset_approx()` added. 
These functions work and return similiarly to their stringr cousins but instead 
of performing regex string matching, perform approximate Levenshtein string 
matching, built off of base R `agrep()` and `agrepl()`. 
