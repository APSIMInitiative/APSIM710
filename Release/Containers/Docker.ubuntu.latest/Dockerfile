######
# Build latest apsim 
######

FROM ubuntu:latest

ADD CottonPassword.txt etc/CottonPassword.txt

# Uncomment if you have a caching apt proxy available
RUN echo "Acquire::http::proxy \"http://qaafi-hss8hy2.instrument.net.uq.edu.au:3142/\"\;" > /etc/apt/apt.conf.d/01proxy 

 # set noninteractive installation
 RUN export DEBIAN_FRONTEND=noninteractive; apt-get update && apt-get install -y tzdata; \
    ln -fs /usr/share/zoneinfo/Australia/Brisbane /etc/localtime; \
    dpkg-reconfigure --frontend noninteractive tzdata

# Download and configure the build environment
RUN apt-get -y install gpg-agent ca-certificates software-properties-common dirmngr \
	git g++ gfortran libxml2-dev tcl8.6 tcl8.6-dev tcllib subversion p7zip p7zip-full

RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9; \
	apt-key adv --keyserver keyserver.ubuntu.com --recv-keys A6A19B38D3D831EF; 

RUN echo "deb https://download.mono-project.com/repo/ubuntu stable-focal main" > /etc/apt/sources.list.d/mono.list
RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/" > /etc/apt/sources.list.d/cran.list
RUN apt-get update && apt-get -y install \
	mono-devel \
	mono-vbnc \
	mono-runtime \
	r-base \
	r-base-dev \
	r-recommended
RUN Rscript -e "install.packages(c(\"Rcpp\",\"RInside\"), repos =\"https://cloud.r-project.org/\")"

ADD BuildApsim.sh usr/local/bin
RUN chmod +x /usr/local/bin/BuildApsim.sh

#RUN /usr/local/bin/BuildApsim.sh
#ENTRYPOINT ["BuildApsim.sh"]
