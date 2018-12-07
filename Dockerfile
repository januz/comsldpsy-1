FROM rocker/binder:3.5.0

# required
MAINTAINER Janosch Linkersdörfer <linkersdoerfer@mailbox.org>

# install newer package versions
RUN Rscript -e "devtools::install_version('drake', version = '6.1.0', repos = 'https://cran.rstudio.com/')"
RUN Rscript -e "devtools::install_version('ggplot2', version = '3.1.0', repos = 'https://cran.rstudio.com/')"
RUN Rscript -e "devtools::install_version('redcapAPI', version = '2.2', repos = 'https://cran.rstudio.com/')"
RUN Rscript -e "devtools::install_version('haven', version = '2.0.0', repos = 'https://cran.rstudio.com/')"
RUN Rscript -e "devtools::install_version('sjmisc', version = '2.7.6', repos = 'https://cran.rstudio.com/')"

COPY . /comsldpsy

# go into the repo directory
RUN . /etc/environment \
    && R -e "devtools::install('/comsldpsy', dep=TRUE)"

