######
# Build apsim revisions
######

FROM ubuntu:latest

ADD . usr/local/bin

RUN echo "Acquire::http::proxy \"http://172.17.0.1:3142/\"\;" > /etc/apt/apt.conf.d/01proxy 

# Download and configure the build environment
RUN apt-get update && apt-get -y install g++ gfortran libxml2-dev tcl8.5 tcl8.5-dev tcllib subversion p7zip p7zip-full

RUN gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-key E084DAB9; \
	gpg -a --export E084DAB9 | apt-key add -; \
	gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-key A6A19B38D3D831EF; \
	gpg -a --export A6A19B38D3D831EF | apt-key add -

RUN echo "deb http://download.mono-project.com/repo/debian  wheezy/snapshots/4.8.1 main" > /etc/apt/sources.list.d/mono.list
RUN echo "deb http://mirror.aarnet.edu.au/pub/CRAN/bin/linux/ubuntu xenial/" > /etc/apt/sources.list.d/cran.list
RUN apt-get update && apt-get -y install \
	mono-devel \
	mono-vbnc \
	mono-runtime \
	r-base \
	r-base-dev \
	r-recommended
RUN Rscript -e "install.packages(c(\"Rcpp\",\"RInside\", \"XML\"), repos =\"http://mirror.aarnet.edu.au/pub/CRAN/\")"

EXPOSE 2500
ENTRYPOINT usr/local/bin/server.tcl
