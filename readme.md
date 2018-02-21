
# R-Project for reproducible research on archaeological ceramics compositional data                
 License Script: GPL 
 http://www.gnu.org/licenses/gpl-3.0.html
 
## Description

* A series of routines for exploratory analysis of ceramic compositions 
* Designed for fully reproducible research
* It is applied on two datasets: well known Roman-British Pottery published by Tubb et al.(1980) and secondly to new data from northern Iberian Peninsula pottery production centers
* Based on M.J. Baxter and Jaume Buxeda i Garrigos variation matrix
* Includes data visualization tools and robust statistical treatments
* To use with a new dataset change the name of the csv file and run the code on each Rmarkdown file, included in the following folders and following the order:

         * 01_Exploration

         * 02_Group_creation

         * 03_Output

## Running the Arch Flow project

### Debian Linux

Install R from a terminal with root privileges:

```
apt-get update
apt-get install dirmngr
apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
```

If Debian 8.0

```
echo "" >> /etc/apt/sources.list
echo "# CRAN R source" >> /etc/apt/sources.list
echo "deb http://cran.cnr.berkeley.edu//bin/linux/debian jessie-cran34/" >> /etc/apt/sources.list
```

If Debian 9.0
```
echo "" >> /etc/apt/sources.list
echo "# CRAN R source" >> /etc/apt/sources.list
echo "deb http://cran.cnr.berkeley.edu//bin/linux/debian stretch-cran34/" >> /etc/apt/sources.list
```

apt-get update
apt-get install r-base r-base-dev
apt-get install libcurl4-openssl-dev
apt-get install libssl-dev
```

Then download the appropiate package for your architecture and version from https://www.rstudio.com/products/rstudio/download/ and open a terminal with root privileges in the directory of the downloaded file and execute the dkpg command (rstudio-1.1.423-amd64.deb used as example, use your downloaded file version):

```
dpkg --install rstudio-1.1.423-amd64.deb
```

Clone the Arch Flow project to your computer. Open the terminal where you want to clone it and type:

```
git clone https://github.com/esteful/arch_flow.git
```

Launch RStudio and open the project, using the desktop interface (open RStudio and go to "File"-> "Open Project..." and selecting the "arch_flow.Rproj" file) or opening the terminal in the cloned project directory and typing:

```
rstudio arch_flow.Rproj
```

Open the "install.r" file and hit the "Run" button or press Ctrl+Enter keys.


## Credits

Following packages have been used for the current project. 

__ArchData__:
David L. Carlson and Georg Roth (2016). archdata: Example Datasets from Archaeological
Research. R package version 1.1. https://CRAN.R-project.org/package=archdata

__Compositions__:
K. Gerald van den Boogaart, Raimon Tolosana and Matevz Bren (2014). compositions:
  Compositional Data Analysis. R package version 1.40-1.
  https://CRAN.R-project.org/package=compositions
  
__Dendextend__:
Tal Galili (2015). dendextend: an R package for visualizing, adjusting, and comparing trees of hierarchical clustering. Bioinformatics. DOI: 10.1093/bioinformatics/btv428

__Dplyr__:
  Hadley Wickham, Romain Francois, Lionel Henry and Kirill Müller (2017). dplyr: A Grammar
  of Data Manipulation. R package version 0.7.3. https://CRAN.R-project.org/package=dplyr

__Plotrix__:
  Lemon, J. (2006) Plotrix: a package in the red light district of R. R-News, 6(4): 8-12.

__Ggbiplot__:
  Vincent Q. Vu (2011). ggbiplot: A ggplot2 based biplot. R package version 0.55.
  http://github.com/vqv/ggbiplot
