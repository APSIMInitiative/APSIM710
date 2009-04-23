using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace CSGeneral
   {
    /// <summary>
    /// The problem with using command line parameters for any applications is that a new
    /// instance of the application is started. For some apps it would be preferable if
    /// there was only one instance of the app and the command line could be forwarded
    /// back to the previous instance of the app.
    ///
    /// This can be done by looking for a previous instance of the app, sending a
    /// WM_COPYDATA message to that previous instance and exiting (if it is found). If it
    /// is not found then the application can start normally. This is the technique used
    /// by MS Word and other applications.
    /// </summary>
   public class SingleApplicationInstance
      {
        //Pick any number at random. This number will be used to uniquely identify the message
        private static int _messageID = -1163005939;
        //this message will used to send the commandline to the previous instance of the app
        public const int WM_COPYDATA = 0x4A;
        //API call to send WM_COPYDATA to the previous instance of the app
        [DllImport("user32", EntryPoint = "SendMessageA")]
        private static extern int SendMessage(IntPtr hWnd, int wMsg, int wParam, COPYDATASTRUCT lParam);


      /// <summary>
      /// Checks for a previous instance of this app and forwards the
      /// command line to this instance if found.
      /// </summary>
      /// <returns>
      /// True if a previous instance was NOT found.
      /// </returns>
      public static bool NoPreviousInstance(string ProcessName, string[] Args)
         {
         IntPtr hWnd = GetHWndOfPrevInstance(ProcessName);
         if (hWnd != IntPtr.Zero)
            {
            SendCommandLine(hWnd, Args);
            return false;
            }
         return true;
         }

      /// <summary>
      /// Searches for a previous instance of this app.
      /// </summary>
      /// <returns>
      /// hWnd of the main window of the previous instance
      /// or IntPtr.Zero if not found.
      /// </returns>
      private static IntPtr GetHWndOfPrevInstance(string ProcessName)
         {
         //get the current process
         Process CurrentProcess = Process.GetCurrentProcess();
         //get a collection of the currently active processes with the same name
         Process[] Ps = Process.GetProcessesByName(ProcessName);
         //if only one exists then there is no previous instance
         if (Ps.Length >= 1)
            {
            foreach (Process P in Ps)
               {
               if (P.Id != CurrentProcess.Id)//ignore this process
                  {
                  //weed out apps that have the same exe name but are started from a different filename.
                  if (P.ProcessName.ToLower() == ProcessName.ToLower())
                     {
                     IntPtr hWnd = IntPtr.Zero;
                     try
                        {
                        //if process does not have a MainWindowHandle then an exception will be thrown
                        //so catch and ignore the error.
                        hWnd = P.MainWindowHandle;
                        }
                     catch { }
                     //return if hWnd found.
                     if (hWnd.ToInt32() != 0) return hWnd;
                     }
                  }
               }
            }
         return IntPtr.Zero;
         }

      /// <summary>
      /// Structure required to be sent with the WM_COPYDATA message
      /// This structure is used to contain the CommandLine
      /// </summary>
      [StructLayout(LayoutKind.Sequential)]
      public class COPYDATASTRUCT
         {
         public int dwData = 0;//32 bit int to passed. Not used.
         public int cbData = 0;//length of string. Will be one greater because of null termination.
         public string lpData;//string to be passed.

         public COPYDATASTRUCT()
            {
            }

         public COPYDATASTRUCT(string Data)
            {
            lpData = Data + "\0";   //add null termination
            cbData = lpData.Length; //length includes null chr so will be one greater
            }
         }


      /// <summary>
      /// Sends command line to a previous instance of this app
      /// </summary>
      /// <param name="hWnd">Main Window handle of the previous instance of this app. Found using the function GetHWndOfPrevInstance()</param>
      /// <param name="CommandLine">CommandLine or message to send</param>
      private static void SendCommandLine(IntPtr hWnd, string[] Args)
         {
         string CommandLine = "";
         foreach (string Arg in Args)
            {
            if (CommandLine != "")
               CommandLine += " ";
            CommandLine += Arg;
            }
         SendMessage(hWnd, WM_COPYDATA, _messageID, new COPYDATASTRUCT(CommandLine));
         }


      public static string ProcessWM_COPYDATA(System.Windows.Forms.Message m)
         {
         if (m.WParam.ToInt32() == _messageID)
            {
            COPYDATASTRUCT st = (COPYDATASTRUCT)Marshal.PtrToStructure(m.LParam, typeof(COPYDATASTRUCT));
            return st.lpData;
            }
         return null;
         }

      }
   }
