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

- 更新: 2026-08-18 06:16 (JST)
- `.claude/CLAUDE.md` を新規作成し，パッケージの構成・ブランチ運用・開発の作法を記録した．
- 作業ツリーはクリーン，`develop` は `origin/develop` と同期している．

### 積み残し

1. **`main` と `develop` で版数が逆転している**
   - `main`: `DESCRIPTION` = 0.2.1.9000 (開発版), `CRAN-SUBMISSION` = 0.2.1
   - `develop`: `DESCRIPTION` = 0.2.1, `CRAN-SUBMISSION` = 0.2.0
   - 本来は `develop` が `.9000` 側．`main` の版数上げコミットを `develop` へ取り込んで解消する．
2. **`README.Rmd` も `main` の修正が `develop` に来ていない**
   - `main` は `# install.packages("remotes")`，`develop` は `devtools` のまま
     (実際に呼ぶのは `remotes::install_github()` なので `remotes` が正しい)．
3. **`develop` の ordination 修正が `main` へ未マージ**
   - `pca` のとき `d_method <- NULL` を明示 (PCA に距離法は不要)
   - `if` の整形，コメント 1 行削除
4. `NEWS.md` が 0.2.1 (2023-07-07) 止まり．`develop` の変更分が未記載．
5. **テストが薄い**．`R/` は 11 ファイルあるがテストは 3 本 (diversity, layer_construction, ordination)．
   `cluster` / `ind_val` / `one2multi` / `convert` などが未カバー．
6. `RoxygenNote: 7.2.3` と古い．`DESCRIPTION` に `BugReports` が無い．

### コミット履歴 (直近)

- `082740e` fix formatting in ordination function for consistency (develop の先端)
- `d613800` refactor ordination function to set default distance method and handle PCA case
- `b00f309` Merge branch 'develop' (main の先端)
- `2a079b0` version 0.2.1.9000 (main のみ)
- `08223c1` ver 0.2.1
