# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse

# required
MAINTAINER Ben Marwick <benmawick@gmail.com>

WORKDIR /Tulai
COPY . /Tulai

RUN  sudo apt-get update -y \
  && R -e "install.packages(c('BiocManager', 'remotes'), repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github(c('rstudio/renv', 'quarto-dev/quarto-r'))" \
  # install pkgs we need
  && R -e "renv::restore()" \
  # run all the code
  && R -e "quarto::quarto_render('/Tulai/paper/paper.qmd')"
