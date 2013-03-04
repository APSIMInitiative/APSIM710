###########################################
# Utility programs and commands (MSDOS)
###########################################

ifdef windir
   PLATFORM=WIN32VS
   DLL=dll
   ECHO=echo
   RM=del /f
   MV=move /y
   CP=xcopy /i /y
else
   PLATFORM=Linux
   DLL=so
   ECHO=echo
   RM=rm -f 
   MV=mv
   CP=cp
   USE_MONO=1
endif
