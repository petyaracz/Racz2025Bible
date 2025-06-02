# Text density across mediaeval Hungarian translations of the Gospels

Rácz, Péter

Tetszék énnékem is, ki eleitől fogva mindeneknek szorgalmasan végére jártam, hogy azokról rendszerint írjak néked, jó Theofilus / Hogy megtudhasd azoknak a dolgoknak bizonyosságát, a melyekre taníttatál. (Lk 1:3-4)

## Points

1. Language entropy vs. text entropy
2. Text entropy, semantics, and morphology
3. Other measures of morphology: parsing
4. Parsing is absent if the text is old and variable
5. Holding the semantics constant in old texts: Bible translations
6. Mediaeval Hungarian translations of the Gospel

## Calculator

[practical entropy](https://petyaracz.shinyapps.io/perplexity_complexity_calculator/)

- More complex morphology results in higher text entropy: "én megyek", "mi megyünk" vs "I go", "we go"
- More variable morphology results in higher text entropy: "én cselekedek", "én cselekszek", "én cselekedem", "én cselekszem" vs "I act"

## Translations used

1. Müncheni kódex, 1466    
2. Jordánszky kódex, 1516                        
3. Pesti Gábor: Újtestamentum, 1536                
4. Sylvester János: Újtestamentum, 1541            
5. Heltai Gáspár: Újtestamentum, 1565              
6. Károli Gáspár Vizsolyi Bibliája, 1590           
7. Káldi György fordítása, 1626                    
8. Károli Gáspár revideált fordítása, 1908
9. Káldi Neovulgáta, 1997 
10. Szent István Társulati Biblia, 2003
11. Magyar Bibliatársulat újfordítású Bibliája, 2014

Misztótfalusi Kis Miklós not included, because only the normalised version is available (see below).

## Repo Structure

- dat
	- bible_matcher.tsv: descriptions from https://parallelbible.nytud.hu/
	- gospel_entropy.tsv: entropies and other information stats across chapters
	- gospels.gz: combined gospel texts
- script
	- model.R: fit stan models
	- setup.R: add info measures to gospels.gz
	- source.R: draw data from clone of https://github.com/nytud/parallelbible
	- viz.R: draw viz. model preds are drawn in model.R
- viz
	- gospel_stats_correlations.png: correlations of stats calced on normalised and original chapters
	- gospel_stats_original.png: stats calced on original chapters
	- gospel_stats_normalised.png: ~ normalised chapters
	- predictions.png: model predictions

## Data dictionary

gospels.gz

- chr (7): translation, description, type, file_name, book, verse, text
- dbl (2): year, chapter

gospel_entropy.tsv

 - translation     : chr ... translation short name
 - year            : num ... translation year (see original info)
 - description     : chr ... description from original
 - type            : chr ... type: original facsimile text (betuhu), normalised, or modern
 - book            : chr ... Mt, Mk, Lk, Jn
 - chapter           : num ... chapter num
 - entropy         : num ... chapter entropy, see script/setup.R
 - perplexity      : num ... chapter perplexity, ~
 - wc              : num ... chapter word count, ~
 - avg_word_length : num ... chapter avg word length, ~
 - type_count      : num ... chapter type count, ~
 - type_token_ratio: num ... chapter type / token ratio, ~

## Text densities

Text is either original / facsimile or normalised. Here's Mark 6:11.

| Original | Normalised | English |
|----------|------------|---------|
| ⁊ valakic nē ſogadandnac tu̇to̗ket  onnan ki mènuen leraʒiatoc a· poꝛt tu̇ labatocꝛol o̗ nèkic tanoſagoca / | és valakik nem fogadnak titeket , onnan kimenvén lerázzátok a port ti lábatokról őnekik tanúságukra . | And if any place will not welcome you or listen to you, leave that place and shake the dust off your feet as a testimony against them. |

For the modern translations, the two are the same.

We calculate the perplexity of individual chapters in the four Books of the Gospel across translations for original / normalised.

Perplexity is an exponential function of entropy.

$\text{Perplexity} = 2^{\text{entropy}}$

Entropy is a measure of the unpredictability of a text. Sort of.

$H(X) = - \sum_{i=1}^{n} P(x_i) \log_2 P(x_i)$

Entropy is shaped by the size of the underlying vocabulary and text size as well as text structure.

Complexity is an approximation of Kolmogorov complexity, it is the length of the gzip-compressed form of each chapter.

Wc is word count. Type / token ratio is the number of unique words divided by the number of words in each chapter.

## Vis

![Correlation of measures](viz/gospel_varcorr_corplot.png)

![Measures: original](viz/gospel_stats_facsimile.png)

![Measures: normalised](viz/gospel_stats_normalised.png)

![Predictions](viz/gospel_preds.png)

![Example](viz/gospel_matthew.png)

## References

Simon, Eszter and Ágnes Kalivoda. A Párhuzamos Bibliakorpusz és Bibliaolvasó fejlesztése. In Általános Nyelvészeti Tanulmányok XXXII., edited by István Kenesei, Éva Dékány, Tamás Halm, and Balázs Surányi. Budapest: Akadémiai Kiadó, 2020.

Simon, Eszter and Bálint Sass. Nyelvtechnológia és kulturális örökség, avagy korpuszépítés ómagyar kódexekből. (2018). Általános Nyelvészeti Tanulmányok XXIV. Bp: Akadémiai kiadó.

Vadász, Noémi, and Eszter Simon. Normo: egy automatikus normalizáló eszköz középmagyar szövegekhez. In Proceedings of the 14th Magyar Számítógépes Nyelvészeti Konferencia (Szeged, 2018), 135–145. ISBN 978-963-306-578-5. http://acta.bibl.u-szeged.hu/id/eprint/59040.

## Bibliography

Pintér, Tibor M. Online segédletek a magyar nyelvű bibliafordítások olvasásához. Modern Nyelvoktatás 27, no. 3-4 (2021): 43-57. DOI: 10.51139/monye.2021.3-4.43.57

Pintér, Tibor M. Magyar nyelvű bibliafordítások statisztikai elemzése. (2024). Alkalmazott Nyelvtudomány, Különszám, 2024/1. szám, 22–36. DOI: 10.18460/ANY.K.2024.1.002
