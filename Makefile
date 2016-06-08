README.md: README.rmd
	Rscript -e 'knitr::knit("$<", "$@")'
