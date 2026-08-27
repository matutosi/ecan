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
cut_levels = c(0, 2, 5, 10, 20), min_size = 5, max_depth = 7, max_indicators = 7,
diff_threshold = 1/3, refine_iter = 5, modified = FALSE, n_clusters = NULL,
use_indicator = FALSE, species = TRUE
```

- `diff_threshold = 1/3` は**頻度比 2:1** に相当する (選好性 `(f2-f1)/(f2+f1)`)．
- `use_indicator = TRUE` にすると，原典と同じく**指標種序列の結果で最終分割**する．
  既定の `FALSE` では精密化序列の結果を使い，指標種は要約としてのみ報告する．

## 原典との比較 (2026-08-27 実測)

`jarioksa/twinspan` (原典 FORTRAN を呼ぶ．MIT) を導入して同じデータで走らせた．
条件はそろえた (`cut_levels` 既定，`min_size = 5`，`max_depth = 6` = 原典の `levmax`)．
**twinspanR ではなくこちらを使った**理由: twinspanR は `twinspan.exe` (コンパイル済みバイナリ) の
ラッパで `riojaExtra`・`betapart` を要し，中身は同じ Hill の FORTRAN．
`jarioksa/twinspan` なら Rtools のコンパイラだけで入り，原典そのものを走らせられる．

| データ | 大きさ | 擬似種数 (原典/ecan) | 第1分割の一致 | 分割回数 | 終端群数 | ARI |
|---|---|---|---|---|---|---|
| dune | 20×30 | **75 / 75** | 19/20 (95%) | 6 / 4 | 7 / 5 | **0.871** |
| varespec | 24×44 | **107 / 107** | **24/24 (100%)** | 8 / 8 | 9 / 9 | **0.516** |
| mite | 70×35 | **128 / 128** | 58/70 (83%) | 21 / 25 | 22 / 26 | **0.287** |

- **擬似種化 (S1) はすべてのデータで完全一致**．ここは原典どおり．
- **粗い構造は一致し，分割が深くなるほど離れる**．dune で違うのは stand 9 の1つだけ．
- **データが大きいほど差が広がる** (ARI 0.87 → 0.52 → 0.29)．

### 固有値の差の原因は「稀種の重み下げ」と判明

固有値は**常に ecan のほうが大きい**．原典は `decorana()` と同じ Hill の重み下げを
擬似種に掛けており，`vegan::downweight()` を掛けた CA の第1固有値と**0.001 以内で一致**する．

| データ | 原典 | ecan (= 素の CA) | downweight 後の CA |
|---|---|---|---|
| dune | 0.5106 | 0.5404 | **0.5151** |
| varespec | 0.1789 | 0.2271 | **0.1785** |
| mite | 0.3629 | 0.3903 | **0.3638** |

### 重み下げを実装して再比較 (2026-08-27)

`tw_downweight()` を新設して**既定で有効**にした (`vegan::downweight()` と 1e-11 以内で一致)．
重みは**序列 (`tw_ra`) と非均質性 (`tw_inertia`) にだけ効かせ**，選好性は素の出現で計算する
(原典と同じ扱い)．`twinspan(downweight = FALSE)` で従来の挙動に戻せる．

| データ | 第1分割の一致 (FALSE → TRUE) | ARI (FALSE → TRUE) | 第1固有値 (原典 / TRUE) |
|---|---|---|---|
| dune | 95% → 95% | 0.871 → 0.871 | 0.5106 / **0.5151** |
| varespec | 100% → 96% | 0.516 → **0.520** | 0.1789 / **0.1785** |
| mite | 83% → 83% | 0.287 → **0.396** | 0.3629 / **0.3638** |

- **固有値はどのデータでも原典との差が 0.005 以内**になった．序列の段階は原典に揃ったと言える．
- **分類の改善は限定的**．mite は大きく改善したが，varespec はほぼ変わらず
  (第1分割は 100% → 96% とむしろ下がった)，dune は変化なし．
- **残る差は精密化序列の細部** (borderline の入れ替え・平衡化) にある．
  dune の分割ごとの固有値も node 1 で 0.5106/0.5151，node 2 で 0.3843/0.4041 と，
  深くなるほど開く (群の中身が食い違うため)．

### 指標種の選び方も違う

原典は `indmax = 7` でも**少数しか選ばない** (dune で 4 個，varespec で 2 個)．
ecan は選好性が閾値を超えたものから機械的に上位 7 個を採る．
ただし**最上位の指標種は一致する** (varespec はどちらも `Cladrang5(-)`)．

### `max_depth` の既定は 6 が正しかった

原典の既定は `levmax = 6` (`args(twinspan::twinspan)` と結果の `levelmax` で確認)．
README の「deeper than 7 levels」という表現に引かれて一度 7 にしたが，**6 に戻した**．

### 残る差の切り分け (2026-08-27．ここで打ち切り)

重み下げを入れたあと，**どこに差が残るか**を調べた．結論から書くと，
**残る差は「分割点のすぐ近くにある標本 (境界標本) の扱い」に集中している**．

**1. 精密化のつまみは dune と varespec をまったく動かさない**．
`refine_iter` (0/1/5)・`diff_threshold` (0.2/1/3/0.5/0.8)・`max_indicators`・`use_indicator`
のどれを変えても，第1分割の一致は dune 95%・varespec 96% のまま動かなかった
(mite だけは動き，`refine_iter = 0` で 94% まで上がるが ARI は 0.396 → 0.342 と下がる)．
→ **食い違いは精密化序列ではなく，一次序列とその分割点にある**．

**2. 食い違う標本は，いずれも分割点に最も近い標本だった**．
dune で違う stand 9 は得点 0.1576 で 20 標本中 3 番目に 0 に近く，
varespec で違う stand 14 は 0.1534 で 24 標本中 2 番目に近い．
→ **境界標本の扱い**が違う．なお dune の分割は ecan 12/8 に対し原典 13/7 で，
**原典のほうが不均等**．単純な「群の大きさを揃える規則」では説明できない．

**3. 精密化の式を変えても一貫した改善は無い**．
差異種に絞った CA の種得点の平均を使い，2群の重心の中点で切ると **dune は 100% になる**が，
varespec は 96% のまま，mite は 94%．逆に mite は差異種で絞らない形が最良 (96%)．
**データごとに最良の式が違う** = 3 データへの当てはめにすぎず，原典の規則ではない．

**【2026-08-27 追記】原典の該当箇所を読んだ**．分かった仕様と段階の計画は
[twinspan_fortran.md](twinspan_fortran.md) に分けて書いた．
差の正体は，**分割点が重心ではなく軸の範囲の中点であること**と，
**頻度の計算から中央帯の標本を除いていること**の 2 点だった (どちらも境界標本に効く)．
なお `ZONEUP` は二元表の帯分けで，境界標本の扱いではなかった (下の記述は誤り)．

→ (以下は読む前の見立て) **原典 FORTRAN の精密化の規則を実際に読む**しかない
(`twinsub21.f` の `POLISH` と `twinsub22.f` の `ZONEUP`)．MIT なので読むこと自体に支障は無い．
**読むべき範囲は合わせて 101 行**なので (下の「原典 FORTRAN の規模」を見る)，
**見積りは 0.5-1 日**．**現時点では追わない方針なので，ここで打ち切る**．

### 原典と違うと分かっている点

1. **borderline 標本の入れ替え規則と，群の大きさの平衡化を入れていない**．
   小さすぎる群を作らない配慮は `min_size` (それ未満の群は分割しない) だけ．
2. **指標種を上位 `max_indicators` 個まで機械的に採る**．原典はもっと少数しか選ばない．
3. **種の分類で標本群による重み付けをしていない**．転置した擬似種行列へ同じ手続きを当てるだけ．
4. RA は**収束するまで回す**．原典は反復回数が決め打ちの可能性がある．

稀種の重み下げは **2026-08-27 に実装して既定で有効にした**ので，差異ではなくなった．
`max_depth` の既定は **6** (原典の `levmax` と同じ)．
一度 7 にしたが，原典の既定が 6 と実測で分かったので戻した．

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
- **それでも手数は残る**．driver の `CLASS` は 272 行で `GOTO` が 26 箇所の F77．
  `ISORT` は **heap sort (不安定ソート)** なので，同点の並び順が分割に効く箇所があると
  R の `order()` (安定) と食い違う．
- 一致の検証には原典を走らせる必要があり，Windows では Rtools (C と Fortran のコンパイラ) が要る．

### 原典 FORTRAN の規模 (2026-08-27 実測)

`jarioksa/twinspan` を clone して数えた．

| | 値 |
|---|---|
| ファイル数 (`.f`) | **21** |
| 総行数 | **1414** (コメント 186，空行 1 → **実行行 1227**) |
| `GOTO` の出現 | **146** |
| C のグルー | `init.c` 35 行，`data2hill.c` 34 行 (計 69 行) |

**サブルーチンの対応** (どこを読めばよいかの地図):

| ファイル | 名前 | 行数 | 役割 |
|---|---|---|---|
| twinsub12.f | `CLASS` | 272 | 分割の driver |
| twinsub19.f | `RA` | 198 | reciprocal averaging |
| twinsub11.f | `PSEUDO` | 130 | 擬似種化 |
| twinsub27.f | `REPORT` | 94 | 出力 |
| twinsub13.f | `CLOSER` | 80 | |
| twinsub26.f | `FIND` | 73 | |
| **twinsub21.f** | **`POLISH`** | **64** | **精密化序列 (残る差の本命)** |
| twinsub16.f | `RECODE` | 54 | |
| twinsub23.f | `TOPIND` | 51 | 指標種の選抜 |
| twinsub07.f | `ISORT` | 46 | heap sort |
| revspec.f | `revspec` | 45 | |
| twinsub15.f | `UPDATE` | 43 | |
| twinsub30.f | `INDSCO` | 42 | 指標得点 |
| **twinsub22.f** | **`ZONEUP`** | **37** | **境界帯の更新 (残る差の本命)** |
| twinsub18.f | `WEIGHT` | 37 | 重み (重み下げ) |
| twinsub25.f | `TABLE` | 34 | 二元表 |
| twinsub24.f | `CODESC` | 30 | |
| twinsub29.f | `YXMULT` | 26 | |
| twinsub28.f | `XYMULT` | 26 | |
| twinsub14.f | `DECODE` | 17 | |
| twinsub20.f | `XMAXMI` | 14 | |

**要点**: 一致に向けて読む必要があるのは全体ではなく，**`POLISH` (64 行) と
`ZONEUP` (37 行) の 101 行**．`ZONEUP` という名前は，まさに切り分けで突き止めた
「境界帯 (zone) の標本の扱い」に対応する．

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

**pure R の独立実装のままとし，現時点では原典との完全一致は追わない**．
必要になったときに a (同梱) を別関数として足す道は残っている
(2026-08-27 ユーザ指示により「現時点では」と改めた．将来の再検討を閉じない)．

反映したこと:

- `?twinspan` に **原典の移植ではないこと，既知の差異4点，
  原典どおりの結果が要るなら jarioksa/twinspan を使うこと**を明記した．
- `NEWS.md` にも同じ案内を書いた．
- 完全一致のための fixture の用意は**不要になった**ので取り下げる．

## 次に決めること

- **【完了 2026-08-27】README.Rmd に TWINSPAN の節を足し，`build_readme()` で再生成した**
  (生成元と生成物は別コミット)．差分は 97 行の追加のみで既存部分に変化なし．
  あわせて `vignettes/twinspan.Rmd` を新設し，`DESCRIPTION` に `VignetteBuilder: knitr` を足した
  (ecan で初めての vignette)．
- **【完了 2026-08-27】`develop` へ merge した**．

次にやること

- **【完了 2026-08-27】稀種の重み下げを実装し，既定で有効にした** (`downweight = FALSE` で戻せる)．
- **【決定 2026-08-27】既定は `downweight = TRUE` のまま** (ユーザ確定)．
- **【完了 2026-08-27】原典 FORTRAN を読んで同じ手順を実装し，`polish = "hill"` を
  既定にした**．**5 データ (dune・varespec・mite・sipoo・BCI) で全レベルの分類が
  原典と完全一致**．詳細は [twinspan_fortran.md](twinspan_fortran.md)．
  ※上の「【決定】完全一致は追わない」「検討は打ち切る」は，この時点までの記録．
    その後ユーザ指示で再開し，一致に到達した．
1. 次のリリースの前に `devtools::check(--as-cran)` を通す．
2. **`CLOSER` も実装済み**．群の番号まで原典と一致する．
   **残る差は種の分類だけ** (原典は標本群への忠実度の比を 0.8/2/6 で切った
   擬似標本を作る．ecan は転置した擬似種行列を使う)．標本の分類には影響しない．
   詳細は [twinspan_fortran.md](twinspan_fortran.md)．
3. **`pyrifos` は深い分割で分かれる** (同点の破り方の違いと見られる)．
3. pkgdown サイトへ vignette が載ることを確認する
   (**`main` へ merge するまで走らない**．workflow の trigger は `main`/`master` への push のみ)．
