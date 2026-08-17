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

### 現在の状態

- 更新: 2026-08-18 07:50 (JST)
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

CRAN 提出 (次はここから)

1. **`devtools::submit_cran()` はまだ実行していない**．取り消しが効かないので，
   ユーザ自身で実行する．`cran-comments.md` は 4 環境の結果を書いた状態になっている．
2. 受理されたら `main` の `DESCRIPTION` を 0.2.2.9000 に上げる (この運用の作法)．
   `CRAN-SUBMISSION` は提出時に自動更新される．

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
