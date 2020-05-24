FROM debian

WORKDIR /root/

# Install R
#RUN apt-get update && apt-get install gnupg2 wget libcurl4-openssl-dev libcurl4-gnutls-dev libxml2-dev libssl-dev libgit2-dev -y
#RUN apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
# ADD jranke.asc .
# RUN apt-key add jranke.asc
# RUN echo "deb http://cloud.r-project.org/bin/linux/debian stretch-cran35/" | tee -a /etc/apt/sources.list
RUN apt-get update && apt-get install r-cran-dplyr r-cran-rcpparmadillo r-cran-mass r-cran-devtools r-cran-devtools -y

# Install R dependencies and glasp
RUN wget https://raw.githubusercontent.com/jlaria/glasp-code/master/install.R
RUN Rscript install.R

# Bring the code
ADD https://api.github.com/repos/jlaria/glasp-code/commits  /dev/null
RUN git clone https://github.com/jlaria/glasp-code.git && cd glasp-code

CMD ["/bin/bash"]

