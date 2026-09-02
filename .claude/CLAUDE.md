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
| `tests/testthat/` | テスト |
| `vignettes/` | vignette (`twinspan.Rmd`．2026-08-27 に新設) |
| `docs/` | pkgdown の出力．`.gitignore` 済みで，GitHub Actions が `gh-pages` へデプロイ |
| `.github/workflows/pkgdown.yaml` | pkgdown のビルドとデプロイ |
| `inst/`, `tools/` | 付随データ・補助ファイル |

## 主な公開関数

- 序列化: `ordination()`, `ord_plot()`, `ord_extract_score()`, `ord_add_group()`
- クラスタリング: `cluster()`, `cls_color()`, `cls_add_group()`
- 距離: `distance()`, `dist2df()`
- 多様度: `shdi()` (種数・Shannon・Simpson など)
- 指標種分析: `ind_val()`
- TWINSPAN: `twinspan()`, `pseudospecies()`, `tw_ra()`, `tw_downweight()`,
  `tw_inertia()`, `tw_preference()`, `tw_hill_const()`, `tw_two_way()`
  (Modified TWINSPAN も `twinspan(modified = TRUE)` で)．
  **既定の `polish = "hill"` は原典 TWINSPAN の分割手順そのもので，
  5 データで分類が原典と完全一致する**．`polish = "ecan"` で従来の挙動．
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

## check の生成物の後始末

- **`R CMD check` などで作られる `*.tar.gz` は，役割が終わったら削除する**．
  結果を確認し終えたら (CRAN へ出す場合は提出が済んだら) 消してよい．
  DESCRIPTION とソースから何度でも作り直せるため，残しておく理由がない．
- 同じ理由で，`*.Rcheck/` (check の作業ディレクトリ) も確認が済んだら消す．
- 補足: `*.tar.gz` を作るのは `R CMD build` / `devtools::build()` で，
  `devtools::check()` は既定で一時ディレクトリに作るためプロジェクト直下には残らない．
  プロジェクト直下に残るのは `R CMD build` を直接実行したときが多い．
  どちらの経路でできたものでも，見つけたら消す．

## 進捗状況

**CRAN 提出の手順・段階ごとの作業ログ・過去のコミット履歴は
[.claude/notes/history.md](notes/history.md)**．

**TWINSPAN の方針・ライセンス調査・実装の構成・原典との差異は
[.claude/notes/twinspan.md](notes/twinspan.md)**．
**原典 FORTRAN を読んで分かった仕様と，一致に至るまでの詰めは
[.claude/notes/twinspan_fortran.md](notes/twinspan_fortran.md)**．
**実装の際にユーザが入力したプロンプトは
[.claude/notes/twinspan_prompts.md](notes/twinspan_prompts.md)** (2026-08-27 の1セッション分)．

### 現在の状態

- 更新: 2026-09-02 09:05 (このセッション，MATUTOSI_DP)
  **黙って結果が壊れるバグ 6 件を直し，テストを 216 → 257 に増やした** (0 失敗)．
  shdi の NaN・ind_val の並べ替え無効・cls_add_group の全 NA・dist2df の 0 距離消失・
  ordination の不明メソッド・ord_add_group の未使用引数．README も再生成．

- 更新: 2026-08-27 (JST)
  **原典 TWINSPAN と完全一致に到達した**．dune・sipoo・varespec・mite・BCI・pyrifos の
  6 データで，**標本の分類も種の分類も，群・番号・固有値まで原典と一致**する．
  決め手は「種の分類は指標種を使わない (`MIND = 0`)」ことだった．

- 更新: 2026-08-27 (JST)
  **原典 FORTRAN を読んで同じ分割手順を実装し，`polish = "hill"` を既定にした**．
  **dune・varespec・mite・sipoo・BCI の 5 データで，階層のすべてのレベルの分類が
  原典と完全一致** (ARI = 1.000)．固有値も一致．全 216 テスト成功．

- 更新: 2026-08-27 11:13 (JST)
  **TWINSPAN の一式を `develop` へ merge した**．README に節を足して `build_readme()` で再生成し，
  **ecan で初めての vignette** (`vignettes/twinspan.Rmd`) を新設．merge 前の `R CMD check` で
  `as.hclust` の総称関数を import していない不具合が見つかり修正 (**0 errors / 0 warnings**)．
  `develop` を push 済み．

### 積み残し

0.2.2 の CRAN 対応はすべて完了 (受理・main マージ・版数上げ・push まで済み)．

TWINSPAN (`develop` へ merge 済み．詳細は [notes/twinspan.md](notes/twinspan.md))

- **【決定 2026-08-27】pure R の独立実装のままとし，現時点では原典との完全一致は追わない**．
  `?twinspan` に既知の差異と，原典どおりの結果が要る場合の案内 (jarioksa/twinspan) を明記済み．
- **【訂正 2026-09-02】`max_depth` の既定は 6** (原典の `levmax`)．
  2026-08-27 にいったん 7 にしたが，実測に合わせて 6 に戻してある．
- **【完了 2026-08-27】README への記載，vignette の新設，`develop` への merge**．
1. **【完了 2026-09-02】`devtools::check(--as-cran)` を通した**．
   **Status: OK (0 errors / 0 warnings / 0 notes)**．vignette を足した後も問題なし．
2. **pkgdown サイトへの vignette の掲載は，`main` へ merge するまで起きない**．
   `.github/workflows/pkgdown.yaml` の trigger は `main`/`master` への push だけで，
   `develop` への push では走らない (手で回すなら `workflow_dispatch`)．

### 2026-09-02 の点検で直したもの

**バグ** (いずれも実測で再現を確認してから直した．テストを先に足して落ちることを見た)

- `shdi()`: abundance に 0 があると `h` が NaN になっていた (`0 * log(0)`)．
- `ind_val()`: 並べ替えが効いていなかった．`res$ind.val` が代入前の labdsv の結果を
  指し，成分名 (`indval`/`pval`) に部分一致しないので `NULL` になっていた．
- `cls_add_group()`: 標本が1つでも `df` に無いと**全ラベルが NA** になっていた
  (`pad2longest()` の `max()` が NA を返すため)．
- `dist2df()`: 対角を外す `filter(dist != 0)` が，別プロット間の距離 0 も落としていた．
- `ordination()`: 未知の `o_method` で `switch` の末尾のカンマ (`fspa` の名残) により
  無関係なエラーが出ていた．
- `ord_add_group()`: `group` 引数が完全に未使用だった (今は必ず残す)．

**その他**: `tw_hill_const()` の `@return` に `mz_out` を追加，`polish = "hill"` が
定数 (`frq_lim`・`cwt_min`) を実際に使うようにした (値は同じで結果は不変)，
`ordination()` の無操作な `res$d_method <- NULL` 3 か所を削除，
`inculde_self` を `include_self` に改名 (旧名も受ける)．

**テスト**: 216 → 257 検査．重複していた検査 (無操作の代入の検査，
`tw_two_way()` の重複，seed の 2 ブロック，`draw_layer_construction()` の 2 ブロック) を整理．

いつか

1. `ind_val()` の群の並びは「df に現れた順」で，因子の順ではない
   (`group_no <- seq_along(unique(...))`)．直すかどうかは未判断．
2. 段階 3 の修正の後に `build_readme()` を回し，`README.md` に差分が出ないことは確認済み
   (README は `pcoa` を載せていないため)．

### コミット履歴 (直近)

- `10b7b0e` bump version to 0.2.2.9000 for development (main)
- `95e6382` Merge branch 'develop' (main，CRAN-SUBMISSION の食い違いを解消)
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
