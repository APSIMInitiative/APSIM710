###########################################
# Utility programs and commands (MSDOS)
###########################################
ECHO=echo
RM=del /f
MV=move /y

ifdef windir
   PLATFORM=WIN32
   DLL=dll
else
   PLATFORM=Linux
   DLL=so
endif