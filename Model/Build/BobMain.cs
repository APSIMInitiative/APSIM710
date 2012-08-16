//css_ref System.Data.dll;

using System;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Threading;
using System.Net;
using System.Net.Sockets;
using System.Collections.Generic;
using System.Data.SqlClient;
using System.Reflection;

class BobMain
{

   /// <summary>
   /// This is the script that Bob runs from Bob.cs. It is executed once the patch has been extracted.
   /// The current working directory will be the root of the APSIM directory.
   /// </summary>
   static int Main(string[] args)
   {
      int ReturnCode = 0;

      try
      {
         Process P = Process.Start("Model\\Build\\BobMain.bat");
         P.WaitForExit();
      }
      catch (Exception err)
      {
         Console.WriteLine(err.Message);
         ReturnCode = 1;
      }
      return ReturnCode;
   }
   
}
