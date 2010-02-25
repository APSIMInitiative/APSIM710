#include <stdlib.h>
#include <General/exec.h>
#include <General/string_functions.h>
#include <vector>
#include <process.h>
#include <fstream>

#include <windows.h>
#include <strsafe.h>
// ------------------------------------------------------------------
//  Short description:
//    executes a program and optionally waits until it has finished.
//    Returns true if program was successfully executed.  NOTE:  This
//    routine is NOT THREAD SAFE.  Wrap inside a Synchronize method call
//    if being called from a thread.
//    If the terminateFlagToCheck is specified, then the routine will
//    check the doTerminate flag periodically and if true, the process
//    will terminate the child process and exit the routine.

//  Notes:

//  Changes:
//    DPH 17/4/1997
//    dph 20/5/98  added new algorithm based on createprocess.
//    dph 21/4/99  added better error handling code.

// ------------------------------------------------------------------
bool Exec(const char* Command_line,
          unsigned int Show_flag,
          bool Wait_for_finish)
   {
   STARTUPINFO StartupInfo;
   PROCESS_INFORMATION ProcessInfo;

   memset(&StartupInfo, '\0', sizeof(STARTUPINFO));
   StartupInfo.cb = sizeof(StartupInfo);
   StartupInfo.dwFlags = STARTF_USESHOWWINDOW;
   StartupInfo.wShowWindow = (WORD) Show_flag;

   if (!CreateProcess( NULL,
                       (LPSTR) Command_line,         // pointer to command line string
                       NULL,                   // pointer to process security attributes
                       NULL,                   // pointer to thread security attributes
                       false,                  // handle inheritance flag
                       NORMAL_PRIORITY_CLASS,  // creation flags
                       NULL,                   // pointer to new environment block
                       NULL,                   // pointer to current directory name
                       &StartupInfo,           // pointer to STARTUPINFO
                       &ProcessInfo) )         // pointer to PROCESS_INF
      {
      LPVOID lpMsgBuf;
      LPVOID lpDisplayBuf;

      FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
          NULL,
          GetLastError(),
          MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
          (LPTSTR) &lpMsgBuf,
          0,
          NULL);
          
      lpDisplayBuf = (LPVOID)LocalAlloc(LMEM_ZEROINIT, 
          (lstrlen((LPCTSTR)lpMsgBuf) + 60) * sizeof(TCHAR)); 
      StringCchPrintf((LPTSTR)lpDisplayBuf, 
        LocalSize(lpDisplayBuf) / sizeof(TCHAR),
        TEXT("CreateProcess failed with error %d: %s"), 
        GetLastError(), lpMsgBuf); 

      // Display the string.
      MessageBox(NULL, (LPCTSTR)lpDisplayBuf, TEXT("Error"), MB_OK|MB_ICONINFORMATION );

      // Free the buffer.
      LocalFree( lpMsgBuf );
      LocalFree(lpDisplayBuf);

      }
   else
      {
      DWORD exitCode;
      bool processIsRunning = true;
      while (processIsRunning)
         {
         DWORD WaitState = MsgWaitForMultipleObjects(1,
                               &ProcessInfo.hProcess,
                               false,
                               INFINITE,
                               QS_ALLINPUT);
         MSG msg;
         switch (WaitState)
            {
            case WAIT_OBJECT_0: processIsRunning = false;
                                break;
            case WAIT_OBJECT_0 + 1: while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
                                       {
                                       TranslateMessage(&msg);
                                       DispatchMessage(&msg);
                                       }
                                    break;
            }
         }
      return true;
      }
   return false;
   }

