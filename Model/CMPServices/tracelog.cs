using System;
using System.IO;

namespace CMPServices
{
   //==============================================================================
	/// <summary>
	/// TTraceLog is a small wrapper for logging text strings to a file.
	/// </summary>
	//==============================================================================
	public class TTraceLog : IDisposable
	{
      private bool FTraceOn;
      private TextWriter FTraceFile;
        /// <summary>
        /// Name of the trace file.
        /// </summary>
      protected string FTraceFileName;
      
      //==============================================================================
      /// <summary>
      /// Construct a logging object.
      /// </summary>
      //==============================================================================
		public TTraceLog()
		{
		  FTraceOn = false;
		  FTraceFileName = "";
		  FTraceFile = null;
		}
        //==============================================================================
        /// <summary>
        /// Close any open file.
        /// </summary>
        //==============================================================================
        public void Dispose()
        {
            if (FTraceFile != null)
                FTraceFile.Close();
            GC.SuppressFinalize(this);
        }
	  //==============================================================================
	  /// <summary>
	  /// Set the tracing to on or off.
	  /// </summary>
	  //==============================================================================
	  public bool DoTrace
	  {
		 get { return FTraceOn;  }
		 set { FTraceOn = value; }
	  }
      //==============================================================================
      /// <summary>
      /// Close the log file.
      /// </summary>
      //==============================================================================
      public void closeLog()
      {
         if (FTraceFile != null) 
         {
            FTraceFile.WriteLine("===========================================");
            FTraceFile.WriteLine("Trace file completed: " + DateTime.Now);
            FTraceFile.Close();
         }
      }
      //==============================================================================
      /// <summary>
      /// Configure the tracing object.
      /// </summary>
      /// <param name="logName">Name of the log file.</param>
      /// <param name="traceOn">True if write the log to file.</param>
      //==============================================================================
      public void config(string logName, bool traceOn)
      {
         FTraceFileName = logName;
         FTraceOn = traceOn;
         if ( (FTraceOn) && (FTraceFile == null) )
         {
            FTraceFile = new StreamWriter(logName);
         }
      }
      //==============================================================================
      /// <summary>
      /// 
      /// </summary>
      /// <param name="text"></param>
      //==============================================================================
      public void logText(string text)
      {
         if ( (FTraceOn) && (FTraceFile != null) )
         {
            FTraceFile.WriteLine(text);
         }
      }
	}
}
