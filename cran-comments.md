## Test environments

* local
    * Windows 11, R 4.3.1
    * Mac OS 11 Big Sur, R 4.3.1

* devtools::check_win_devel()

* devtools::check_rhub()
    * Windows Server 2022, R-devel, 64 bit
    * Ubuntu Linux 20.04.1 LTS, R-release, GCC
    * Fedora Linux, R-devel, clang, gfortran


## R CMD check results

There were 0 ERRORs, 0 WARNINGs, and 3 NOTEs.

* checking CRAN incoming feasibility ... [5s/11s] NOTE
  Maintainer: ‘Toshikazu Matsumura <matutosi@gmail.com>’
  New submission

  Package was archived on CRAN
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2023-05-08 as requires archived package 'dave'.

* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found


* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
  'lastMiKTeXException'


  check_rhub() on Windows Server shows this note. 


## Downstream dependencies

There are currently no downstream dependencies for this package.
