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

- 更新: 2026-08-18 06:31 (JST)
- `.claude/CLAUDE.md` を新規作成し，パッケージの構成・ブランチ運用・開発の作法を記録した
  (あわせて `.Rbuildignore` に `^\.claude$` を追加)．
- 開発用パッケージを導入した (R 4.6.1 のユーザーライブラリ `win-library/4.6`)．
  devtools 2.5.2 / roxygen2 8.1.0 / testthat 3.3.2 / pkgdown 2.2.1 / ggdendro 0.2.0 /
  knitr 1.51 / rmarkdown 2.31．
- `main` を `develop` へマージし，版数の逆転を解消した．
  `develop` の `DESCRIPTION` は 0.2.1.9000，`CRAN-SUBMISSION` は 0.2.1 になった．
  `README.Rmd` の `remotes` 修正も取り込んだ．
- ここまでを `origin/develop` へ push した．

### 積み残し

段階 2: 基準線を作る

1. `DESCRIPTION` に `BugReports` を追加する．
2. `devtools::document()` で `man/` を再生成する
   (roxygen2 が 8.1.0 なので `RoxygenNote: 7.2.3` が更新される)．
3. `devtools::build_readme()` で `README.md` と `man/figures/README-*.png` を再生成する．
4. **`devtools::check()` を通し，R 4.6.1・vegan 2.7.5 の組み合わせでの警告・注意を洗い出す．**
   テストを増やす前にここを済ませ，失敗が環境由来か自分のコード由来かを切り分けられるようにする．

段階 3: 品質

5. `ordination()` の PCA 変更 (`d_method <- NULL`) の回帰テストを追加する．
   `develop` の変更のうち唯一の挙動の変更なので，リリースするなら根拠が要る．
6. **テストが薄い**．`R/` は 11 ファイルあるがテストは 3 本 (diversity, layer_construction, ordination)．
   `cluster` 系 → `ind_val` → `convert` (`df2table`/`table2df`) → `one2multi` 系 の順に追加する．

段階 4-5: リリース

7. `NEWS.md` が 0.2.1 (2023-07-07) 止まり．0.2.2 の項を追記する．
8. `DESCRIPTION` を 0.2.2 に上げ，`cran-comments.md` を更新して `check(cran = TRUE)`．
9. `develop` → `main` マージ，タグ付け，CRAN 提出，pkgdown のデプロイ確認．
   公開後に `main` の `DESCRIPTION` を 0.2.2.9000 に戻す．

### コミット履歴 (直近)

- `082740e` fix formatting in ordination function for consistency (develop の先端)
- `d613800` refactor ordination function to set default distance method and handle PCA case
- `b00f309` Merge branch 'develop' (main の先端)
- `2a079b0` version 0.2.1.9000 (main のみ)
- `08223c1` ver 0.2.1
