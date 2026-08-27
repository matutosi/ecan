# TWINSPAN の実装 (設計の記録)

2026-08-27 開始．作業は worktree `worktree-twinspan` (ブランチも同名)．

## 方針 (2026-08-27 ユーザ確定)

1. **アルゴリズムの筋は忠実に，原典 `twinspan.exe` との完全一致は当面の目標にしない**．
   差異はドキュメントに明記する．
2. **標本の分類・種の分類・二元表まで実装する** (段階的に)．
3. **独立関数 `twinspan()` を新設し，`as.hclust()` で既存の可視化に接続する**
   (`cluster()` へは統合しない．`cluster()` は距離行列を前提にしていて相性が悪い)．
4. **Modified TWINSPAN も実装する** (Roleček et al. 2009)．
5. 将来，余力があれば完全一致も目指す．

## ライセンスの調査 (2026-08-27)

**結論: 原典 FORTRAN は MIT で入手でき，ecan (MIT) へ移植できる．clean-room は不要**．

| 入手経路 | ライセンス | 中身 | 移植 |
|---|---|---|---|
| [jarioksa/twinspan](https://github.com/jarioksa/twinspan) | **MIT + file LICENCE** | Hill の FORTRAN そのもの (`src/twinsub*.f`, `revspec.f`) | **可** |
| [zdealveindy/twinspanR](https://github.com/zdealveindy/twinspanR) | GPL-2 | `twinspan.exe` を包むだけ．ソース無し | 不可．参照しない |

- jarioksa/twinspan の `DESCRIPTION` は `License: MIT + file LICENCE`，
  `Authors@R` に **Jari Oksanen (cre, aut) と Mark O. Hill (aut)** の両名．
- ただし `LICENCE` の記載は `YEAR: 2019 / COPYRIGHT HOLDER: Jari Oksanen` で，
  **Hill 本人の許諾文は公開されていない**．Oksanen の表示に依拠する点は承知のうえで進める．
- 移植する場合の条件: **著作権表示と許諾文を残す** (`inst/COPYRIGHTS` か `LICENSE` に追記)，
  `Authors@R` に Oksanen と Hill を `ctb`/`cph` で加える，由来をヘルプと `NEWS.md` に書く．
- twinspan.exe の**出力を検証用の fixture として取り込むのは問題ない** (数値結果には著作権が及ばない)．

### 検討して採らなかった案

- **clean-room (A が FORTRAN を読んで仕様書 → B が仕様書だけで実装)**．
  ライセンスが MIT と分かったので不要になった．
  なお当初案の「A と B が似ていたら C が実装し直す」は保護にならない
  (C は同じ仕様書を読むので，仕様書が汚染されていれば同じものを再生産する)．
- **完全一致版を別パッケージ (GPL-2) に分ける**．MIT で移植できるので不要．

## 実装の構成 (`R/twinspan.R`)

段階は S1 から S6 と呼んでいる．

| 段階 | 関数 | 中身 |
|---|---|---|
| S1 | `pseudospecies()` | 擬似種化．既定の cut level は `0, 2, 5, 10, 20`．列名は `種名_水準番号`．どの標本にも出ない擬似種は落とす |
| S2 | `tw_ra()` | reciprocal averaging による対応分析の第1軸．重み付き重心で中心化し，反復の縮尺 `s` がそのまま固有値になる |
| S3 | `tw_refine()` (内部) | 精密化序列．`tw_preference()` の選好性から差異種を選び，重み付けした得点で再分割．安定するまで反復 |
| S4 | `tw_indicator()` (内部) | 指標種序列．選好性の上位を最大 `max_indicators` 個選び，±1 の合計得点の閾値で S3 の分割を最もよく再現する切り方を選ぶ．誤分類数を記録 |
| S5 | `tw_two_way()` | 種の分類 (擬似種を単位に転置行列へ同じ手続きを適用) と，並べ替えた二元表 |
| S6 | `twinspan(modified = TRUE)` | Modified TWINSPAN．非均質性 (`tw_inertia()` = 総イナーシャ) が最大の群から分割し，`n_clusters` で止める |

- 分割の制御は `tw_tree()` に切り出してあり，標本にも種にも同じものを使う．
- `as.hclust.twinspan()` で `hclust` に変換し，`cls_color()`・`cls_add_group()`・
  `ggdendro::ggdendrogram()` にそのまま載せられる．
  枝の高さは**分割の順序**から与える (親は必ず子より先に分割されるので単調になる)．

### 既定値

```
cut_levels = c(0, 2, 5, 10, 20), min_size = 5, max_depth = 6, max_indicators = 7,
diff_threshold = 1/3, refine_iter = 5, modified = FALSE, n_clusters = NULL,
use_indicator = FALSE, species = TRUE
```

- `diff_threshold = 1/3` は**頻度比 2:1** に相当する (選好性 `(f2-f1)/(f2+f1)`)．
- `use_indicator = TRUE` にすると，原典と同じく**指標種序列の結果で最終分割**する．
  既定の `FALSE` では精密化序列の結果を使い，指標種は要約としてのみ報告する．

### 原典と違うと分かっている点

1. **`max_depth` の既定が 6**．原典は **7**．
2. **borderline 標本の入れ替え規則と，群の大きさの平衡化を入れていない**．
   小さすぎる群を作らない配慮は `min_size` (それ未満の群は分割しない) だけ．
3. **種の分類で標本群による重み付けをしていない**．転置した擬似種行列へ同じ手続きを当てるだけ．
4. RA は**収束するまで回す**．原典は反復回数が決め打ちの可能性がある．

## 検証の状況

- **`tw_ra()` は `vegan::cca()` の第1軸と完全一致** (固有値・得点の相関とも)．
- dune の 5 群は `dune.env$Moisture` の傾度と対応する．
- 退化データ (全ゼロ・全行同一) では分割せずに止まる．
- テストは `tests/testthat/test-twinspan.R` に 10 件 49 検査．パッケージ全体で 185 件が成功．
- 副産物: jarioksa/twinspan の README から，**Roleček の非均質性は「群の全固有値の和 =
  scaled Chi-square」**と確認できた．`tw_inertia()` の総イナーシャと同じ定義．

## 原典 FORTRAN を実際に見て分かったこと (2026-08-27)

`jarioksa/twinspan` の `src/` を確認した (ライセンスは MIT なので参照に支障は無い)．

- **倍精度で書かれている** (`twinsub12.f` の `CLASS` は `IMPLICIT DOUBLE PRECISION (A-H,O-Z)`
  と `REAL(8)`)．**単精度の再現という最大の懸念は消えた**．R の double でそのまま合う．
- **文献では決まらなかった定数が `COMMON` にそのまま並んでいる**．
  `COMMON/LIMS/RARE,FEEBLE,FRQLIM,TOL,RATLIM,REPLIM,PRECIS`，
  `COMMON/ARBS/CWTMIN,CRLONG,CRCUT`，`COMMON/IARBS/ICWEXP,IEND,MMIN,IPREXP,LEVMAX`．
  差異種の閾値・平衡化の規則・borderline の扱いは読めば分かる．
- **それでも手数は残る**．`CLASS` だけで約 425 行，`GOTO` が 30 箇所近い F77．
  `ISORT` は **heap sort (不安定ソート)** なので，同点の並び順が分割に効く箇所があると
  R の `order()` (安定) と食い違う．
- 一致の検証には原典を走らせる必要があり，Windows では Rtools (C と Fortran のコンパイラ) が要る．

### 完全一致の道は3つ

| | 方法 | 期間 | 得るもの / 代償 |
|---|---|---|---|
| a | **FORTRAN を ecan に同梱してラッパを書く** | 1-2 日 | 完全一致が構造的に保証．CRAN から原典どおりの TWINSPAN が使える / **ecan に初のコンパイル依存** |
| b | FORTRAN を読んで R に翻訳する | 3-5 日 | pure R を保てる / 同点処理などで完全一致に届かない可能性 |
| c | **今の独立実装のままにする** | 0 | pure R で軽い / 完全一致はしない．必要な人には jarioksa/twinspan を案内 |

**b は費用対効果が最も低い** (完全一致なら a が確実で安く，pure R なら c で足りる)．
b が意味を持つのは「pure R のまま完全一致も欲しい」ときだけ．
判断は「**pure R を守るか，完全一致を取るか**」の1点に集約される．
推奨は **c を既定に保ち，必要になった時点で a を別関数として足す**．

### 【決定 2026-08-27】c を選ぶ (ユーザ確定)

**pure R の独立実装のままとし，原典との完全一致は追わない**．この件はこれで終わり
(以後は蒸し返さない)．必要になったときに a (同梱) を別関数として足す道は残っている．

反映したこと:

- `?twinspan` に **原典の移植ではないこと，既知の差異4点，
  原典どおりの結果が要るなら jarioksa/twinspan を使うこと**を明記した．
- `NEWS.md` にも同じ案内を書いた．
- 完全一致のための fixture の用意は**不要になった**ので取り下げる．

## 次に決めること

1. **【判断待ち】`max_depth` の既定を 6 のままにするか，原典と同じ 7 にするか**．
   完全一致は追わないので必須ではないが，そろえておくと原典との比較がしやすい．
2. **【判断待ち】`develop` へ merge する時期**．
3. README / vignette への記載．
