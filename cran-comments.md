## Correct version

* I recieved an e-mail as shown below. (I added the heading numbers.)

  1.  The Description field is intended to be a (one paragraph) description of 
      what the package does and why it may be useful. Please add more details 
      about the package functionality and implemented methods in your 
      Description text.

  2.  If there are references describing the methods in your package, please 
      add these in the description field of your DESCRIPTION file in the form
      authors (year) <doi:...>
      authors (year) <arXiv:...>
      authors (year, ISBN:...)
      or if those are not available: <https:...>
      with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
      auto-linking. (If you want to add a title as well please put it in 
      quotes: "Title")

  3.  Please add \value to .Rd files regarding exported methods and explain 
      the functions results in the documentation. Please write about the 
      structure of the output (class) and also what the output means. (If a 
      function does not return a value, please document that too, e.g. 
      \value{No return value, called for side effects} or similar)
      Missing Rd-tags:
            dots2list.Rd: \value
            shoot.Rd: \value
            t_if_true.Rd: \value

  4.   Some code lines in examples are commented out. Please never do that. 
      Ideally find toy examples that can be regularly executed and checked. 
      Lengthy examples (> 5 sec), can be wrapped in \donttest{}.
      Examples in comments in:
             cluster.Rd

  5.  In tools/veg.r your are changing the users' par(). While you use 
      on.exit() to reset it, you only reset the par(mar) (in line 353) but 
      later change par(new) (in line 396) which is not resetted. In order to 
      reset this as well please change line 352 to:
           par.orig <- par(no.readonly = TRUE)

  6.  Please ensure that your functions do not write by default or in your 
      examples/vignettes/tests in the user's home filespace (including the 
      package directory and getwd()). This is not allowed by CRAN policies. 
      Please omit any default path in writing functions. In your 
      examples/vignettes/tests you can write to tempdir(). -> tools/veg.r, 
      basel2()

*   I appreciate your advices. In this version I have:
      1. 
      2. 
      3. Removed functions (moved into tools directory cf. 5. and 6.), because the funcions were under development.
      4. Wrapped examples (> 5 sec) in cluster.R 
      5. 6. Added tools/*.R in .rbuildignore to solve problems. (The directory of tools is for personal use.)

説明欄は、パッケージが何をするのか、なぜそれが有用なのかを（1段落）説明することを意図しています。パッケージの機能と実装されたメソッドについての詳細は、Descriptionテキストに追加してください。

もし、あなたのパッケージのメソッドを説明する文献があれば、DESCRIPTIONファイルのDescriptionフィールドに以下の形式で追加してください。
著者（年）＜doi:...＞の場合
著者名（年）＜arXiv:...＞の場合
著者名 (年, ISBN:...)
または、それらが利用できない場合。<https:...>
のように、'doi:'、'arXiv:'、'https:'の後にスペースを入れず、自動リンクのために角括弧を付けてください。(タイトルも付けたい場合は、引用符で囲んでください：「タイトル」）。

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

There were 0 ERRORs, 0 WARNINGs, and 4 NOTEs.

* checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Toshikazu Matsumura <matutosi@gmail.com>'

  New submission


* checking for detritus in the temp directory ... NOTE   
  Found the following files/directories:   
    'lastMiKTeXException'   

  check_rhub() on Windows Server shows this note. 


* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

* checking examples ... [9s/19s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
           user system elapsed
  cluster 2.245   0.12   5.142


  check_rhub() on Fedora Linux shows the last 2 notes.
  The example is very simple and short code, but elapsed time is over 5s.
  I have no idea to solve this.


## Downstream dependencies

There are currently no downstream dependencies for this package.
