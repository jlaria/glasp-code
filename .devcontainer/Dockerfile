FROM debian:stretch

# Install R from cran backports
RUN apt-get update && apt-get install gnupg2 wget -y
RUN apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
RUN echo -e "\ndeb http://cloud.r-project.org/bin/linux/debian stretch-cran35/\n" >> /etc/apt/sources.list
RUN apt-get update && apt-get install r-base -y

