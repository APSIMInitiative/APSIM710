###########################################
# Utility programs and commands (MSDOS)
###########################################

ifdef windir
   PLATFORM=WIN32
   DLL=dll
   ECHO=echo
   RM=del /f
   MV=move /y
else
   PLATFORM=Linux
   DLL=so
   ECHO=echo
   RM=rm -f 
   MV=mv
endif