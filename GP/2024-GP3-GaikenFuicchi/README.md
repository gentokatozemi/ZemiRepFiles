
### 加藤言人・稲垣和由・飯尾天翔・田口俊介 (近刊予定) 「外見と政策の不一致は候補者選択を阻害するか？候補者の外見年齢が争点投票に与える影響に関するサーベイ実験」 ,　『選挙研究』.

#### [事前登録/Preregistration (2025年1月18日)](https://doi.org/10.17605/OSF.IO/5KMY6)

#### [オンライン付録/Online Appenix (2026年3月3日)](G3_オンライン付録.pdf)

#### 分析再現ファイル（本サブレポジトリ）

分析を再現するには、各データファイルおよび分析コードファイルをダウンロード（ファイルを開いたページの右上にある「download raw file」アイコンをクリック）して、分析コードファイルを実行してください。分析コードはR言語で記述されています。相対パスでデータを読み込むためには、RStudioを使用してコードを実行してください。

<!-- * <code>G3_オンライン付録.pdf</code>: オンライン付録ファイル。 -->
* <code>[dataset_g3_main.rds](dataset_g3_main.rds)</code>: 実験データ。
* <code>[codebook_g3_main.txt](codebook_g3_main.txt)</code>: 実験データのコードブック。
* <code>[analysis_g3_main_v3.R](codebook_g3_main.txt)</code>: 実験の主要分析結果を再現するファイル。図2, 3, 4, 5, B.3, C.1, C.2, C.3, C.4, D.1, E.1, E.2, 表D.1, D.2を出力する。
* <code>[analysis_g3_main_v3_kanshinalt.R](analysis_g3_main_v3_kanshinalt.R)</code>: 値０と１を統合した政治関心変数を用いた分析（オンライン付録E.2）を再現するファイル。表E.1, 図E.3, E.4を出力する。
* <code>[analysis_g3_main_v3_kiokualt.R](analysis_g3_main_v3_kiokualt.R)</code>: 事前登録された公約記憶変数を用いた分析（オンライン付録F）を再現するファイル。表F.1, 図F.1, F.2, F.3を出力する。
* <code>[analysis_g3_main_v3_chishikialt.R](analysis_g3_main_v3_chishikialt.R)</code>: 正答０－１問を統合した政治知識変数を用いた分析（オンライン付録G）を再現するファイル。表G.1, 図G.1, G.2を出力する。 
* <code>[analysis_g3_main_v3_controlled.R](analysis_g3_main_v3_controlled.R)</code>: 統制変数を含めた分析を再現するファイル。表H.1, H.2, 図H.1, H.2, H.3, H.4, H.5, H.6, H.7, H.8を出力する。