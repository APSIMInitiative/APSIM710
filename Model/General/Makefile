include $(APSIM)/Model/Build/Platform.make

PROJECT = General

SRC =   pch.cpp 		\
    date_class.cpp		\
	math_functions.cpp	\
	stream_functions.cpp	\
	string_functions.cpp	\
	StringTokenizer.cpp	\
	stristr.cpp		\
	macro.cpp		\
	io_functions.cpp	\
	xml.cpp			\
	path.cpp		\
	IniFile.cpp 		\
	stream_processor.cpp    \
	dll.cpp			\
	http.cpp   \
	strptime.cpp

PROJECTTYPE=libdll
ifeq ($(PLATFORM),WIN32VS)
LIBS = ShLwApi
endif

PRECOMPILE = ../General/pch.h

include $(APSIM)/Model/Build/$(PLATFORM)CPP.make
