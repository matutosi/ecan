# ecan (R パッケージ)

生態学の解析 (序列化・クラスタリング・多様度・指標種分析) を支援する R パッケージ．
`stats`・`vegan`・`labdsv` などのラッパを揃え，一貫した書き方で使えるようにしている．
CRAN 公開済み (最新リリース 0.2.1)．

- CRAN: <https://CRAN.R-project.org/package=ecan>
- GitHub: <https://github.com/matutosi/ecan>
- pkgdown サイト: <https://matutosi.github.io/ecan/>
- shiny 版 (ほぼ同じ機能): <https://matutosi.shinyapps.io/ecanvis/>

## ディレクトリ構成

| パス | 内容 |
| --- | --- |
| `R/` | 関数の本体 (`ordination.R`, `cluster.R`, `diversity.R`, `ind_val.R`, `one2multi.R`, `convert.R`, `layer_construction.R`, `gen_example_layer.R`, `read_biss.R`, `utils*.R`) |
| `man/` | roxygen2 が生成する `.Rd`．**手で編集しない** |
| `man/figures/` | README 用の PNG (`README.Rmd` の knit で生成) |
| `tests/testthat/` | テスト (現状 3 本のみ) |
| `docs/` | pkgdown の出力．`.gitignore` 済みで，GitHub Actions が `gh-pages` へデプロイ |
| `.github/workflows/pkgdown.yaml` | pkgdown のビルドとデプロイ |
| `inst/`, `tools/` | 付随データ・補助ファイル |

## 主な公開関数

- 序列化: `ordination()`, `ord_plot()`, `ord_extract_score()`, `ord_add_group()`
- クラスタリング: `cluster()`, `cls_color()`, `cls_add_group()`
- 距離: `distance()`, `dist2df()`
- 多様度: `shdi()` (種数・Shannon・Simpson など)
- 指標種分析: `ind_val()`
- データ変換: `df2table()`, `table2df()`, `one2multi` 系 (`is_one2multi()` など)
- その他: `gen_example()`, `read_biss()`, `draw_layer_construction()`, `pad2longest()`

## 開発の作法

- **ブランチ運用: 開発は `develop`，公開は `main`**．
  `main` へは `develop` からマージする (直接コミットしない)．
  リリース後の版数は `main` 側で `x.y.z.9000` の開発版に上げる運用になっている．
- `R/` の roxygen コメントを直したら **`devtools::document()`** で `man/` を再生成する．
- README は **`README.Rmd` が原稿**．`README.md` と `man/figures/README-*.png` は
  `devtools::build_readme()` (knit) で生成するので，`README.md` を手で直さない．
- リリース前は `devtools::check()` (できれば `--as-cran`) を通す．
- 版数を上げたら `NEWS.md` に日付と変更点を追記する．
- CRAN 提出の記録は `CRAN-SUBMISSION` (提出時に自動更新される)．

## 進捗状況

> **現状確認を頼まれたら，下の「CRAN 提出の手順」を必ず一緒に再掲する．**
> 別 PC へ移って作業を続けるための申し送り (2026-08-18)．
> 提出が済んだらこの指示は消してよい．

### CRAN 提出の手順 (0.2.2，未実施)

> **【重要】CRAN は 2026-08-05 〜 2026-08-19 の間，提出の受付を止めている**
> (CRAN team vacation and maintenance work)．**8-19 以降に再挑戦する**．
> 再開したかどうかは次で分かる (200 なら再開，404 なら停止中)．
>
> ```
> curl -sS -o /dev/null -w "%{http_code}\n" https://xmpalantir.wu.ac.at/cransubmit/index2.php
> ```

提出用のパッケージは `D:\Dropbox\todo\ecan_0.2.2.tar.gz`
(別 PC には無いので，その場合は `devtools::build(path = "..")` で作り直す)．

方法 1: **対話的な R セッション**から呼ぶ．

```r
# RStudio か R.exe のコンソールで (Rscript では動かない)
setwd("<ecan のディレクトリ>")
devtools::submit_cran()
```

方法 2: Web フォーム <https://cran.r-project.org/submit.html> から手動で提出．
name は `Toshikazu Matsumura`，email は `matutosi@gmail.com`，
コメント欄には `cran-comments.md` の内容を貼る．

どちらでも**確認メールのリンクを踏むまで提出は完了しない**．

### 現在の状態

- 更新: 2026-08-18 09:32 (JST)
- **CRAN 提出を試みたが，CRAN 側が受付を停止していてできなかった**．
  - フォームのページ <https://xmpalantir.wu.ac.at/cransubmit/> に
    `CRAN submissions will be offline from Aug 5, 2026 to Aug 19, 2026` と告知が出ており，
    入力欄ごと消えている．devtools が POST する `index2.php` は **404** を返す．
  - 対話的な R コンソールから `devtools::submit_cran()` を実行したところ，
    ビルドは成功 (`ecan_0.2.2.tar.gz`，72.4 Kb) したが，
    アップロードで `Resolving timed out [xmpalantir.wu.ac.at]` になった．
    **これは DNS の一時的な失敗で本質ではない** (直後に確認したら 0.008 秒で解決した)．
    名前解決が通っても，受付が再開するまでは 404 で `Package failed to upload.` になる．
- **この PC (`MATUTOSI_DP`) には devtools が入っていなかったので導入した**
  (R 4.6.1 のユーザーライブラリ `win-library/4.6`，devtools 2.5.2 と依存一式)．
  LaTeX は `C:\texlive\2024` にあり `pkgbuild::has_latex()` は TRUE なので，
  `build(manual = TRUE)` は通る．
- 【参考】非対話セッションから `submit_cran()` を回したいときは，
  `devtools:::yesno()` を差し替える．**`yesno()` は「はい以外」で TRUE を返す**ので，
  `utils::assignInNamespace("yesno", function(msg, .envir = parent.frame()) FALSE, ns = "devtools")`
  とすれば「はい」を選んだのと同じになる．確認は 2 か所だけで，他に対話は入らない．
- `.claude/CLAUDE.md` を新規作成し，パッケージの構成・ブランチ運用・開発の作法を記録した
  (あわせて `.Rbuildignore` に `^\.claude$` を追加)．
- 開発用パッケージを導入した (R 4.6.1 のユーザーライブラリ `win-library/4.6`)．
  devtools 2.5.2 / roxygen2 8.1.0 / testthat 3.3.2 / pkgdown 2.2.1 / ggdendro 0.2.0 /
  knitr 1.51 / rmarkdown 2.31．
- `main` を `develop` へマージし，版数の逆転を解消した．
  `develop` の `DESCRIPTION` は 0.2.1.9000，`CRAN-SUBMISSION` は 0.2.1 になった．
  `README.Rmd` の `remotes` 修正も取り込んだ．
- **段階 2 (基準線) を完了した**．
  - `devtools::check()` は変更の前後とも **0 errors / 0 warnings / 0 notes** (R 4.6.1)．
  - `DESCRIPTION` に `BugReports` を追加した．
  - `devtools::document()` を実行した．roxygen2 が 8 系になり
    `RoxygenNote` → `Config/roxygen2/version`，`NAMESPACE` の `importFrom` が複数行形式に
    変わったが，**`man/*.Rd` の内容に差分は無かった**．
  - `README.Rmd` に `set.seed(1)` を入れた．`ind_val()` が並べ替え検定のため，
    従来は `build_readme()` のたびに p 値と全 PNG が変わっていた．
    再実行して**差分ゼロ**になることを確認した．
  - `README.md` と `man/figures/README-*.png` を再生成した．
- **段階 3 (品質) を完了した**．`check()` は 0/0/0，テストは 108 パス・警告 0．
  - **`pcoa` のバグを直した (`R/ordination.R`)**．
    `res$st_scores <- ord$` と `$` の後ろが空のまま行が終わっており，
    R がコメントを飛ばして次行を続きとして解釈するため，
    `res$eig_val <- ord$eig` が丸ごと飲み込まれていた．
    その結果 `st_scores` に固有値のベクトルが入り，`eig_val` は設定されなかった．
    構文としては正しいので `R CMD check` は素通りしていた．
  - `ca` と `dca` も距離法を使わないので `distance_method` を `NULL` にした
    (`pca` に合わせた．従来は未使用の `"bray"` を記録していた)．
  - `df2table()` の `pivot_wider()` を `dplyr::all_of()` で包み，
    tidyselect の非推奨警告 60 件を解消した．
  - テストを追加した (`cluster`, `convert`, `one2multi`, `ind_val` は新規，
    `ordination` は拡充)．3 本 → 35 本．
    既存の `expect_equal(res_ord$sdev, res_pca$eig_val)` は
    **両辺とも NULL で素通りしていた**ので比較先を直した．
- **段階 4-5 (リリース準備) を完了した．CRAN 提出だけが残っている**．
  - 版数を 0.2.2 に上げ，`NEWS.md` に 0.2.2 の項を書いた．
    `DESCRIPTION` の `URL:` をカンマ区切りにし，`Suggests:` の余分なコンマを消した．
  - **4 環境すべてで 0 errors / 0 warnings / 0 notes**．
    ローカル Windows R 4.6.1，win-builder R-devel (r90413)，
    rhub の linux / macOS / Windows (いずれも R-devel)．
  - rhub は 2.x から GitHub Actions 方式なので `.github/workflows/rhub.yaml` を追加した
    (`rhub::rhub_setup()` が生成．**デフォルトブランチに無いと動かない**)．
    実行は `$env:GITHUB_PAT = (gh auth token)` を設定してから
    `rhub::rhub_check(platforms = c('linux','macos','windows'))`．
  - `develop` を `main` へ `--no-ff` でマージし，**タグ `v0.2.2`** を付けて push した
    (このリポジトリで最初のタグ)．pkgdown のデプロイも成功した．

### 積み残し

CRAN 提出 (次はここから．**8-19 以降に再挑戦する**)

1. **`devtools::submit_cran()` は実行したが，CRAN の受付停止 (〜8-19) で提出できなかった**．
   `cran-comments.md` は 4 環境の結果を書いた状態になっている．
   提出用のパッケージは `D:\Dropbox\todo\ecan_0.2.2.tar.gz` に作ってある．
   - **【落とし穴】`Rscript -e "devtools::submit_cran()"` は動かない**．
     `yesno()` が `Called from non-interactive context.` で落ちる．
     回避フラグは無いので，**R コンソール (RStudio か `R.exe`) から呼ぶ**．
   - Web フォーム <https://cran.r-project.org/submit.html> からの手動提出でもよい．
     ただし**手動提出では `CRAN-SUBMISSION` が更新されない**ので，あとで手で直す．
   - どちらでも，**確認メールのリンクを踏むまで提出は完了しない**．
2. 受理されたら `main` の `DESCRIPTION` を 0.2.2.9000 に上げる (この運用の作法)．

いつか

3. テストがまだ無いもの: `gen_example()`, `read_biss()`, `draw_layer_construction()`,
   `pad2longest()`．
4. 段階 3 の修正の後に `build_readme()` を回し，`README.md` に差分が出ないことは確認済み
   (README は `pcoa` を載せていないため)．

### コミット履歴 (直近)

- `aa29ffa` Merge branch 'develop' for 0.2.2 (main，タグ `v0.2.2`)
- `84585b5` add the R-hub v2 workflow
- `0b7b92b` prepare release 0.2.2
- 段階 3: pcoa 修正 → ca/dca の distance_method → tidyselect 非推奨 → テスト追加 の 4 つに分けた
- `fbe6b4a` rebuild README.md and figures (生成物)
- `2f26e16` set the seed in README.Rmd for reproducible output (生成元)
- `dd943e0` add BugReports and re-document with roxygen2 8.1.0
- `3db5453` Merge branch 'main' into develop
- `79ba137` add .claude/CLAUDE.md with project notes
- `b00f309` Merge branch 'develop' (main の先端)
