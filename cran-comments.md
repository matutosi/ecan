## Test environments

* local
    * Windows 11, R 4.2.2
    * Mac OS 11 Big Sur, R 4.2.2
* devtools::check_win_devel()
* devtools::check_rhub()
    * Windows Server 2022, R-devel, 64 bit
    * Ubuntu Linux 20.04.1 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran

## R CMD check results

There were 0 ERRORs, 0 WARNINGs, and 3 NOTEs.

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Toshikazu Matsumura <matutosi@gmail.com>'

  New submission

* checking for detritus in the temp directory ... NOTE   
  Found the following files/directories:   
    'lastMiKTeXException'   

  check_rhub() on Windows Server shows this note. 


* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

  check_rhub() on Fedora Linux shows this note.

## Downstream dependencies

There are currently no downstream dependencies for this package.