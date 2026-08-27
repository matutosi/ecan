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

### 現在の状態

- 更新: 2026-08-27 09:12 (JST，worktree `worktree-twinspan`)
  **TWINSPAN と Modified TWINSPAN を R で実装した** (公表された記述からの実装．原典 FORTRAN は未参照)．
  `tw_ra()` が `vegan::cca()` の第1軸と完全一致することを確認し，新規 49 検査を含む全 185 テストが成功．
  **原典 FORTRAN のライセンスは MIT と判明** (jarioksa/twinspan)．clean-room は不要になった．

- 更新: 2026-08-23 (JST)
- **CRAN から 0.2.2 の受理連絡 (auto-check OK) が届いた．積み残しの手順をすべて終えた**．
  `develop` を `main` へマージし (`CRAN-SUBMISSION` の食い違いを解消)，
  `main` の `DESCRIPTION` を `0.2.2.9000` に上げてこの運用の作法どおり開発版へ戻した．
  `develop` も `main` に追随させて版数をそろえた．**push は未実施 (次の一手)**．

- 更新: 2026-08-22 19:05 (JST)
- **CLAUDE.md の書き方ルール (todo 直下で確定) をこのプロジェクトにも適用した**．
  段階ごとの作業ログ (08-18 〜 08-21) と直近のコミット履歴を `.claude/notes/history.md` へ移し，
  本体には現在の状態・積み残しだけを残した．CRAN 提出まわりの現在の状況・積み残しは変更なし．

- 更新: 2026-08-22 18:09 (JST)
- **0.2.2 を Web フォームから手動で提出し，確認メールのリンクも踏んで完了した**．
  疎通確認 (同日) で受付が開いていることを確かめたうえでの提出．
  `CRAN-SUBMISSION` は 0.2.2 (2026-08-22 09:06:50 UTC) に自動更新されていた
  (手動提出でも更新されると分かった．前回の記述は誤り)．

- 更新: 2026-08-22 (JST)
- **CRAN 提出サーバへの疎通を再確認した．提出可能な状態**．
  `https://xmpalantir.wu.ac.at/cransubmit/` は HTTP 200，フォームの入力欄も通常どおり表示．
  ページ内の「受付停止」の文言は `<!-- -->` でコメントアウトされた 2017/2018 年の古い告知で，
  現在アクティブな告知は無い．
  準備 (版数 0.2.2，`cran-comments.md` 4環境 0/0/0，タグ `v0.2.2` push 済み) は整っている．

### 積み残し

0.2.2 の CRAN 対応はすべて完了 (受理・main マージ・版数上げ・push まで済み)．

TWINSPAN (`worktree-twinspan` で作業中．詳細は [notes/twinspan.md](notes/twinspan.md))

- **【決定 2026-08-27】pure R の独立実装のままとし，原典との完全一致は追わない**．
  この件はこれで終わり (以後は蒸し返さない)．`?twinspan` に既知の差異4点と，
  原典どおりの結果が要る場合の案内 (jarioksa/twinspan) を明記済み．
1. **【判断待ち】`max_depth` の既定を 6 のままにするか，原典と同じ 7 にするか**．
2. **【判断待ち】`develop` へ merge する時期**．
3. README / vignette への記載．

いつか

1. テストがまだ無いもの: `gen_example()`, `read_biss()`, `draw_layer_construction()`,
   `pad2longest()`．
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
