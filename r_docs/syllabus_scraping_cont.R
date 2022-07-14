
## load libraries
pacman::p_load(tidyverse, stringi, rvest)

## CSS selectors
author_css <- "tr:nth-child(23) tr:nth-child(4) .cell_border_y~ .cell_border_y+ .cell_border_y , .syllkmok tr:nth-child(12) .cell_border_y~ .cell_border_y+ .cell_border_y , .syllkmok tr:nth-child(4) .cell_border_y~ .cell_border_y+ .cell_border_y , tr:nth-child(23) tr:nth-child(12) .cell_border_y~ .cell_border_y+ .cell_border_y"
## year
year_css <- "tr:nth-child(23) tr:nth-child(16) .cell_border_y:nth-child(3) , tr:nth-child(23) tr:nth-child(8) .cell_border_y:nth-child(3) , .syllkmok tr:nth-child(16) .cell_border_y:nth-child(3) , .syllkmok tr:nth-child(8) .cell_border_y:nth-child(3)"
## title
title_css <- ".syllkmok tr:nth-child(4) .cell_border_y:nth-child(1) , .syllkmok tr:nth-child(12) .cell_border_y:nth-child(1) , tr:nth-child(23) tr:nth-child(12) .cell_border_y:nth-child(1) , tr:nth-child(23) tr:nth-child(4) .cell_border_y:nth-child(1)"
## publisher
publisher_css <- "tr:nth-child(23) tr:nth-child(16) .cell_border_y:nth-child(1) , .syllkmok tr:nth-child(8) .cell_border_y:nth-child(1) , .syllkmok tr:nth-child(16) .cell_border_y:nth-child(1) , tr:nth-child(23) tr:nth-child(8) .cell_border_y:nth-child(1)"

# teacher clumps------------------------------------------------------------------

inui <- "社会科学のためのデータ分析	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510601101&value(crclumcd)=2442017000
経済成長論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510701101&value(crclumcd)=2442017000
The Economic Development of Japan	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510706601&value(crclumcd)=2442017000
Globalization, Economic Growth and Income Distribution	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510801101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800103&value(crclumcd)=0900000001
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801103&value(crclumcd)=0900000001
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900102&value(crclumcd)=0900000001"


makita <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112109&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113109&value(crclumcd)=2442017000
開発と環境の地理学	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511200101&value(crclumcd)=2442017000
世界の貧困問題	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511300101&value(crclumcd)=2442017000
Sustainable Development	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511302101&value(crclumcd)=2442017000
Case Study Methods	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511400101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800109&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801110&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900106&value(crclumcd)=
"
ito <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112101&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113101&value(crclumcd)=2442017000
International Economics	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510705101&value(crclumcd)=2442017000
Current Economic Issues in the Global Economy	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510800101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800101&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801101&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900101&value(crclumcd)=
"

kashiwagi <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112103&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113103&value(crclumcd)=2442017000
マクロ経済学（国際社会科学科）	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510602101&value(crclumcd)=2442017000
国際金融論（国際社会科学科）	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510702101&value(crclumcd)=2442017000
Theory of International Finance	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510707501&value(crclumcd)=2442017000
International Macroeconomic Policy	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510802501&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800104&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801104&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900103&value(crclumcd)=

"

zhao <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112108&value(crclumcd)=2442017000
Health Economics	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510805101&value(crclumcd)=2442017000
中国社会の経済分析	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511600101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800108&value(crclumcd)=
"

yamasaki <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112110&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113110&value(crclumcd)=2442017000
国際開発論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511502101&value(crclumcd)=2442017000
アフリカ経済論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511602101&value(crclumcd)=2442017000
Economic Development	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511605101&value(crclumcd)=2442017000
Education and Economic Development in Africa	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511702101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800110&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801111&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900107&value(crclumcd)=
"

kubo <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112106&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113106&value(crclumcd)=2442017000
アジア経済論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511501101&value(crclumcd)=2442017000
地域研究の手法	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511601101&value(crclumcd)=2442017000
Emerging Asian Economy and Society	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511604101&value(crclumcd)=2442017000
Politics and Economy in Southeast Asia	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511701101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800106&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801106&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900110&value(crclumcd)=
"

ishikawa <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112102&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113102&value(crclumcd)=2442017000
グローバル経済論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510603101&value(crclumcd)=2442017000
統計学（国際社会科学科）	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510604101&value(crclumcd)=2442017000
ゲーム理論（国際社会科学科）	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510704101&value(crclumcd)=2442017000
Industrial Organization	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510709101&value(crclumcd)=2442017000
Japan in the World Economy	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510806101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800102&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801102&value(crclumcd)=
"

tamaki <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112107&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113107&value(crclumcd)=2442017000
社会学（国際社会科学科）	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511201101&value(crclumcd)=2442017000
計量社会学	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511301101&value(crclumcd)=2442017000
Sociology of Population	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511303101&value(crclumcd)=2442017000
International Migration	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511401101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800107&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801107&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900105&value(crclumcd)=
"

hoshi <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112112&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113112&value(crclumcd)=2442017000
ビジネス法	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510301101&value(crclumcd)=2442017000
比較会社法	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510400601&value(crclumcd)=2442017000
Law and Economics	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510401101&value(crclumcd)=2442017000
Corporate Finance and Law	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510500101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800113&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801112&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900109&value(crclumcd)=
"

kim <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112111&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113111&value(crclumcd)=2442017000
マネジメント論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510900101&value(crclumcd)=2442017000
組織行動論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511000101&value(crclumcd)=2442017000
Cross-Cultural Organizational Behavior	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511003101&value(crclumcd)=2442017000
International Human Resource Management	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511100101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800112&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801113&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900111&value(crclumcd)=0900000001
"

usui <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112105&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113105&value(crclumcd)=2442017000
マーケティング（国際社会科学科）	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510902101&value(crclumcd)=2442017000
マーケティングと消費者行動	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511002101&value(crclumcd)=2442017000
Marketing Strategy	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511005101&value(crclumcd)=2442017000
Global Marketing	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511103101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800105&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801105&value(crclumcd)=
"

garcia <- "入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113113&value(crclumcd)=2442017000
簿記	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510903101&value(crclumcd)=2442017000
Financial Accounting	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511004101&value(crclumcd)=2442017000
Financial Statement Analysis	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511101601&value(crclumcd)=2442017000
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801114&value(crclumcd)=
"

shibata <- "入門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510112104&value(crclumcd)=2442017000
入門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510113104&value(crclumcd)=2442017000
国際経営論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U510905101&value(crclumcd)=2442017000
技術経営論	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511002601&value(crclumcd)=2442017000
Studies of Multinational Enterprises	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511006101&value(crclumcd)=2442017000
Innovation Management and Globalization	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511104101&value(crclumcd)=2442017000
専門演習Ⅰ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511800111&value(crclumcd)=
専門演習Ⅱ	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511801109&value(crclumcd)=
卒業論文・卒業演習	https://g-port.univ.gakushuin.ac.jp/campusweb_gk/slbssbdr.do?value(risyunen)=2022&value(semekikn)=1&value(kougicd)=U511900108&value(crclumcd)=0900000001
"


## create a tibble with professors as chr vector
## then convert each chr vector to symbol
## (matching value stored in global environment)
## and evalute the symbol to give the lump string from global environment

iss_profs <-
tibble(professor = c("inui","ito","kashiwagi","zhao","shibata","kubo",
               "yamasaki", "hoshi", "garcia", "ishikawa","usui",
               "kim","tamaki","makita"),
      lump = map_chr(professor, ~eval(sym(.x)))) %>%
  arrange(professor)

## extract the course and link from each lump

iss_profs <-
iss_profs %>%
  mutate(course = map(lump, ~
                        str_extract_all(.x,"(.*)(?=https)") %>%
                        unlist() %>%
                        # remove empty strings from list
                        .[. !=""] %>%
                        # tidy up text
                        str_squish()),
         link = map(lump, ~
           str_split(.x, "(.*)(?=https)") %>%
           unlist() %>%
           # remove empty strings from list
           .[. !=""] %>%
           # tidy up text
           str_squish())) %>%
  select(-lump) %>%
  unnest(cols = c(course,link))


## Use stri_enc_isascii() to identify course language 


 iss_eng <-
   iss_profs %>%
  ## find language of course
  mutate(eng = stri_enc_isascii(course)) %>%
  # filter only English
  filter(eng)

## scrape reading details

eng_reading<- iss_eng  %>%
  mutate(
    author = map(link, ~read_html(.x)) %>%
      map(html_nodes, author_css) %>%
      map(html_text) %>%
      map(str_squish),
    year = map(link, ~read_html(.x)) %>%
      map(html_nodes, year_css) %>%
      map(html_text) %>%
      map(parse_number),
    title = map(link, ~read_html(.x)) %>%
      map(html_nodes, title_css) %>%
      map(html_text) %>%
      map(str_squish),
    publisher = map(link, ~read_html(.x)) %>%
      map(html_nodes, publisher_css) %>%
      map(html_text) %>%
      map(str_squish))

## unnest columns to see the results
EMI_reading <- eng_reading %>%
  select(-c(link,eng)) %>%
  unnest(cols = c(author, year, title, ,publisher)) %>%
  mutate(professor = str_to_title(professor)) %>%
  mutate(title = str_remove_all(title, "[^a-zA-Z ]")) %>%
  mutate(type = "EMI")


## Create a nice looking table
EMI_reading %>% 
  janitor::clean_names(case = "title") %>%
  knitr::kable() %>%
  kableExtra::kable_styling()
