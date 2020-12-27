FROM rocker/rstudio

EXPOSE 8787
ENV PASSWORD=glasp

RUN apt-get update && apt-get install -y libxml2-dev libgdal-dev

RUN R -e "install.packages(c('readxl', 'ggplot2', 'tidyr', 'rmarkdown', 'devtools', 'rgdal'))"
RUN R -e "devtools::install_github('jlaria/glasp', dependencies=TRUE)"
RUN R -e "devtools::install_version('tune', '0.1.0'); devtools::install_version('parsnip', '0.1.3')"
RUN R -e "install.packages('sparklyr'); sparklyr::spark_install()"
RUN R -e "install.packages('ggpubr')"

RUN R -e "sparklyr::spark_install()"

EXPOSE 8787

COPY entrypoint.sh /usr/bin/
RUN chmod +x /usr/bin/entrypoint.sh
ENTRYPOINT ["entrypoint.sh"]
