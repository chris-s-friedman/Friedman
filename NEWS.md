# Friedman 0.5.2

* Adds Drexel color palettes and `scale_color_drexel()` and `scale_fill_drexel()` funs. In addition, utility functions `display_drexel_all()` and `display_drexel_pal()` added to visualize the palattes. 

# Friedman 0.5.1

* Just bug fixes. Got the attr type to work
* add_class function gives the ability to concatenate a class to an objects 
classes

# Friedman 0.5.0

* adv_tbl funs and classes!

# Friedman 0.4.0

* Added a `NEWS.md` file to track changes to the package.
* `str_detect_approx()`, `str_which_approx()`, and `str_subset_approx()` added. 
These functions work and return similiarly to their stringr cousins but instead 
of performing regex string matching, perform approximate Levenshtein string 
matching, built off of base R `agrep()` and `agrepl()`. 
