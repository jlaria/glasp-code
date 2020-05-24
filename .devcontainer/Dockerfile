FROM debian:stretch

WORKDIR /root/

# Install R from cran backports
RUN apt-get update && apt-get install gnupg2 wget -y
RUN apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
RUN echo -e "\ndeb http://cloud.r-project.org/bin/linux/debian stretch-cran35/\n" >> /etc/apt/sources.list
RUN apt-get update && apt-get install r-base -y

# Install R dependencies and glasp
RUN wget https://raw.githubusercontent.com/jlaria/glasp-code/master/install.R
RUN Rscript install.R

# Bring the code
ADD https://api.github.com/repos/jlaria/glasp-code/commits  /dev/null
RUN git clone https://github.com/jlaria/glasp-code.git && cd glasp-code

CMD ["/bin/bash"]

