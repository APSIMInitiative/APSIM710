###########################################
# Utility programs and commands (MSDOS)
###########################################

ifdef windir
 ifdef VS100COMNTOOLS
   PLATFORM=WIN32VS
   DLL=dll
   ECHO=echo
   RM=del /f
   MV=move /y
   CP=xcopy /i /y
 else
   PLATFORM=WIN32
   DLL=dll
   ECHO=echo
   RM=del /f
   MV=move /y
   CP=xcopy /i /y
 endif
else
   PLATFORM=Linux
   DLL=so
   ECHO=echo
   RM=rm -f 
   MV=mv
   CP=cp
   USE_MONO=1
endif
