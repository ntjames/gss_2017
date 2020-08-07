# gss_2017

2017 JSM Data Challenge

This project was developed for the 2017 Joint Statistical Meetings GSS Data Challenge. It provides tools to allow quick, simple, interactive access to basic dataset and variable information and comparisons of annual trends for intitial exploratory data analysis. The app can be accessed at: https://ntjames.shinyapps.io/jsm17/

The main files are:
  
  /src/cln_data.R  
  /src/scrp_data.R  
  /src/app.R  

This project is organized using John Myles White's excellent [ProjectTemplate](http://projecttemplate.net/) R package. To load this project, you'll first need to install the package using `install.packages('ProjectTemplate')` and attach it with `ibrary('ProjectTemplate')`.  Then use `setwd()` to change the working directory to the location where this README file is located. Then you need to run the following two lines of R code:

  library('ProjectTemplate')  
	load.project()  

After you enter the second line of code, you'll see a series of automated
messages as ProjectTemplate goes about doing its work. This work involves:
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in he configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing your data using the files in the `munge` directory.

Once that's done, you can execute any code you'd like. For every analysis
you create, we'd recommend putting a separate file in the `src` directory.
If the files start with the two lines mentioned above:

	library('ProjectTemplate')
	load.project()

You'll have access to all of your data, already fully preprocessed, and
all of the libraries you want to use.

