# APSIM

The Agricultural Production Systems sIMulator (APSIM) is internationally recognised as a highly advanced simulator of agricultural systems. It contains a suite of modules which enable the simulation of systems that cover a range of plant, animal, soil, climate and management interactions. APSIM is undergoing continual development, with new capability added to regular releases of official versions. Its development and maintenance is underpinned by rigorous science and software engineering standards. The APSIM Initiative has been established to promote the development and use of the science modules and infrastructure software of APSIM.

Continuous Integration builds of this repository can be found [Here](https://apsimdev.apsim.info/APSIM.Builds.Portal/Bob.aspx). 

# Building APSIM

Microsoft Visual Studio (2017 or 2019) is used to build Apsim Classic. The (free on registration) Community Edition will suffice.

Version control: you will need at least a command line git client installed on your system, and you may feel comfortable using a GUI such as [TortoiseGit](https://tortoisegit.org/) to examine changes to the source tree. There are several described [here](https://stackoverflow.com/questions/8047483/does-tortoisegit-work-with-portablegit-x-x-x-x-previewyyyyyy-what-are-compatibl/32427897#32427897).

### Steps

1. Clone the repository:

```git clone https://github.com/APSIMInitiative/APSIMClassic```

2. Install [7Zip](http://www.7-zip.org/), unpack the build library archive _alongside_ the repository.

```
cd ..
7z x APSIMClassic/Model/Build/BuildLibraries.7z
```

3. Install a 32 bit GNU fortran 7.x compiler (from any non-cygwin binary distribution). This package also includes GNU make, which is used by the builder. A zip archive is available [here](https://www.apsim.info/wp-content/uploads/2023/02/gcc-5.3.0.7z).

The unit tests require [NUnit 2.x](https://nunit.org/).

The cotton module will not build unless the password is obtained from the module owner.

To build the GUI, you will need two commercial products: [TeeChart for .NET](http://www.steema.com/teechart/net) component, and  the [Quantum Whale editor](http://www.qwhale.net/products/editor.htm) component.

To build the R component, you will need R installed, and the extensions RCpp, RInside.

4. Open the Visual Studio 2017/9 Developer command prompt, and change to …/APSIMClassic/Model/Build. Run the command “BuildAll.bat” in that directory.

![Output of build script](https://www.apsim.info/wp-content/uploads/2019/11/Capture.png)

# Linux

The linux build of ApsimClassic uses the mono runtime for .NET components. Unfortunately, recent (5,6.x) versions of the runtime have [proved unstable](https://github.com/APSIMInitiative/ApsimX/issues/2604), so an older version (4.8.1) is recommended. On debian systems, this involves adding keys and repositories (as root):

```
gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-key E084DAB9
gpg -a --export E084DAB9 | apt-key add -
gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-key A6A19B38D3D831EF
gpg -a --export A6A19B38D3D831EF | apt-key add -

echo "deb http://download.mono-project.com/repo/debian wheezy/snapshots/4.8.1 main" > /etc/apt/sources.list.d/mono.list
echo "deb http://mirror.aarnet.edu.au/pub/CRAN/bin/linux/ubuntu bionic-cran35/" > /etc/apt/sources.list.d/cran.list
apt-get update && apt-get -y install \
   g++ gfortran libgfortran3 \
   libxml2 libxml2-dev tcl8.5 tcllib curl \
   mono-devel mono-vbnc mono-runtime \
   r-base r-base-dev r-recommended
Rscript -e "install.packages(c(\"Rcpp\",\"RInside\"))"
```

Although the GUI components can be built and run under Linux, they are not fully operational, and are not supported by the APSIM development team. If you wish to experiment with the GUI, you may need to make a few changes to enable Mono on Linux to emulate the Windows registry. This can be done with the following commands:

```
sudo mkdir /etc/mono/registry
sudo mkdir /etc/mono/registry/ClassesRoot
sudo chmod 777 /etc/mono/registry -R
```