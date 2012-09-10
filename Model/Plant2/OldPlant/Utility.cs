using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;

    class Util
    {
        public static StreamWriter Dbg = null;
        
        [Conditional("DEBUG")]
        public static void Debug(string format, object value)
        {
            if (Dbg == null)
                Dbg = new StreamWriter("plant2.debug");
            format = format.Replace("%f0", "{0:0}");
            format = format.Replace("%f2", "{0:0.00}");
            format = format.Replace("%f", "{0:0.000}");
            format = format.Replace("%i", "{0:0}");
            format = format.Replace("%s", "{0}");
            Dbg.WriteLine(string.Format(format, value));
        }

        [Conditional("DEBUG")]
        public static void DebugArray(string format, double[] value, int NumElements)
        {
            for (int i = 0; i < NumElements; i++)
                Debug(format, value[i]);
        }

    }
