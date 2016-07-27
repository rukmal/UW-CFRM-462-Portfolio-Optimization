all:
	Rscript -e 'rmarkdown::render("main.Rmd", output_file = "Final Project Writeup.pdf")'
