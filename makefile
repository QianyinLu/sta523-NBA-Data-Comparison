project_report.html: project_report.Rmd data/james.rds data/durant.rds data/image.rds data/direct.rds
	Rscript -e "library(rmarkdown); render('project_report.Rmd')"

data/james_raw.rds: R/data_scraping.R
	mkdir -p data/
	Rscript $<

data/james.rds: R/data_cleaning.R data/james_raw.rds
	Rscript $<

data/durant_raw.rds: R/data_scraping.R
	Rscript $<
	
data/durant.rds: R/data_cleaning.R data/durant_raw.rds
	Rscript $<

data/image.rds: R/image_saving.R
	Rscript $<

data/direct.rds: R/data_cleaning.R data/durant_raw.rds data/james_raw.rds
	Rscript $<

.PHONY: clean_html, clean_data

clean_html:
	rm project_report.html

clean_data:
	rm -rf data/