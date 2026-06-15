## Test environments
* local macOS install, R 4.5.1
* win-builder (devel, release and oldrelease)
* r-devel (linux, macos and windows) on rhub

## R CMD check results
0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Subhrajyoty Roy <subhrajyotyroy@gmail.com>’
  New submission
  
  This is a resubmission. In this version I have:
  * Removed single quotes around "kodom" (and other words that are not software, package names or API) in the DESCRIPTION file, as requested by the CRAN review.
  * Added \value (via @return) to geom_kodom_periodic.Rd.
  * Replaced \dontrun{} with \donttest{} in examples.
  
  Note: You might see a NOTE about "possibly mis-spelled words in DESCRIPTION" for the word "kodom". "kodom" is not misspelled; it is the name of the plot style and flower. The single quotes were removed per the request of the CRAN reviewer in the previous submission.

## Downstream dependencies
There are currently no downstream dependencies. (Since this is a new release)