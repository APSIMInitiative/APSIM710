###########################################
# Utility programs and commands (MSDOS)
###########################################

ifdef windir
 ifdef VSINSTALLDIR
   PLATFORM=WIN32VS
   DLL=dll
   ECHO=echo
   RM=del /f
   MV=move /y
 else
   PLATFORM=WIN32
   DLL=dll
   ECHO=echo
   RM=del /f
   MV=move /y
 endif
else
   PLATFORM=Linux
   DLL=so
   ECHO=echo
   RM=rm -f 
   MV=mv
   USE_MONO=1
   MONO=/opt/mono-2.10
endif

