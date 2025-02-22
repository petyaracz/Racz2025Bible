## structure

- dat
	- bible_matcher.tsv: descriptions from https://parallelbible.nytud.hu/
	- gospel_entropy.tsv: entropies and other information stats across verses
	- gospels.gz: combined gospel texts
	- gospel_bigram_informativity.gz: bigram informativity across gospel verses
	- gospel_trigram_informativity.gz: trigram ~
- script
	- model.R: fit stan models
	- setup.R: add info measures to gospels.gz
	- source.R: draw data from clone of https://github.com/nytud/parallelbible
	- viz.R: draw viz. model preds are drawn in model.R
- viz
	- gospel_stats_correlations.png: correlations of stats calced on normalised and original verses
	- gospel_stats_original.png: stats calced on original verses
	- gospel_stats_normalised.png: ~ normalised verses
	- predictions.png: model predictions

## data dict

gospels.gz

- chr (7): translation, description, type, file_name, book, line, text
- dbl (2): year, verse

gospel_entropy.tsv

 - translation     : chr ... translation short name
 - year            : num ... translation year (see original info)
 - description     : chr ... description from original
 - type            : chr ... type: original facsimile text (betuhu), normalised, or modern
 - book            : chr ... Mt, Mk, Lk, Jn
 - verse           : num ... verse num
 - entropy         : num ... verse entropy, see script/setup.R
 - perplexity      : num ... verse perplexity, ~
 - wc              : num ... verse word count, ~
 - avg_word_length : num ... verse avg word length, ~
 - type_count      : num ... verse type count, ~
 - type_token_ratio: num ... verse type / token ratio, ~