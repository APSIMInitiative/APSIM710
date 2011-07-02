using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

namespace DiffDiff
{
    static class Program
    {

        //[System.Runtime.InteropServices.DllImport("kernel32.dll")]
        //private static extern bool AllocConsole(); 

        /// <summary>
        /// The main entry point for the application.
        /// </summary>
        [STAThread]
        static int Main()
        {
            Args args = new Args(Environment.GetCommandLineArgs());

            if (args.LoadGUI)
            {
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new frmMain(args));
                return 0;
            }
            else
                if (args.PrintHelp)
                {
                    Console.WriteLine(help());
                    return 0;
                }
                else
                    return new CmdLineProcessor(args).Run();
        }

        public static string help()
        {
            return
@"DiffDiff Command-Line Usage:

diffdiff.exe <input1> <input2> <additional args>

    where 'input1' and 'input2' may point to individual files or folders

    all arguments are optional, but the following are required if you DO NOT
    want a gui to appear:
        <input1>
        <input2>
        -t:*tol*
    
    if any of these are missing (or if '-gui' is specified) then the GUI will
    open and you may then run diffs from there.

    the default method of reporting from the command line is to create a txt
    file for each failed output file containing information on what failed
    (tab delimited).  This has the same name as the output with '.txt' appended

    valid arguments are:
        -console    redirect output to console instead of log files
        -f:*filter* if directories are used as inputs then a filter may be
                    specified here for getting certain files
                    defaults to '*.out' if not specified
        -gui        force the GUI to open
        -help       display this message
        -o:*out*    where *out* can be replaced with either a single file or a
                    directory.  

                        If file then all output is dumped into given file

                        If directory (MUST EXIST) then logs are created for 
                        each FAILED diff inside given dir
        -t:*tol*    set an acceptable tolerance for differences in output files
                    where *tol* is replaced with:
                    
                        *any decimal* to set exclusive maximum difference
                            i.e. -t:0.01

                        *any int*dp to use a given number of dec. places as
                            the tolerance. -ve ints specify number of digits at
                            end of value to ignore (if -1dp then 0.026 -> 0.02)
                            i.e. -t:2dp     -t:-1dp

                        *any decimal*pct to use a %age tolerance (new value must
                            be within 'x'% of original value)
                            i.e. -t:10pct
        -vsdebug    used when debugging from Visual Studio because it adds an
                    extra cmd line arg by default.  This tells diffdiff to
                    ignore the first argument

DiffDiff returns a count of the number of failed tests ('0' if successful)

Example Useage:
diffdiff C:\prjA\tests\orig C:\prjA\tests\new -t:0.01 -o:C:\prjA\diffs.txt
";
        }

    }

    /// <summary>
    /// Container to hold data from arguments passed in
    /// </summary>
    public class Args
    {
        public string A { get; private set; }
        public string B { get; private set; }
        public string Output { get; private set; }
        public string Filter { get; private set; }

        public bool LoadGUI { get; private set; }
        public bool ReportToConsole { get; private set; }
        public bool PrintHelp { get; private set; }

        public DiffMethod Method { get; private set; }

        public decimal Tolerance { get; private set; }

        public Args(string[] rawargs)
        {
            List<string> args = new List<string>(rawargs);

            if (args.Contains("/?") || args.Contains("-help") || args.Contains("--help") || args.Contains("?"))
            {
                LoadGUI = false;
                PrintHelp = true;
                return;
            }

            if (args.Contains("-vsdebug"))
                args.RemoveAt(0);

            A = null;
            B = null;
            Output = null;
            Filter = "*.out";

            LoadGUI = false;
            ReportToConsole = false;

            Method = DiffMethod.nul;
            Tolerance = 0;

            foreach (string arg in args)
                if (arg.StartsWith("-"))
                    switch (arg[1])
                    {
                        case 'a':
                            A = arg.Substring(arg.IndexOf(':') + 1);
                            break;
                        case 'b':
                            B = arg.Substring(arg.IndexOf(':') + 1);
                            break;
                        case 'c':
                            ReportToConsole = arg == "-console";
                            break;
                        case 't':
                            decimal t = 0;
                            string targ = "";
                            if (arg.EndsWith("dp"))
                            {
                                Method = DiffMethod.dp;
                                targ = arg.Substring(3).Replace("dp", "");
                            }
                            else if (arg.Replace("%", "pct").EndsWith("pct"))
                            {
                                Method = DiffMethod.pct;
                                targ = arg.Substring(3).Replace("pct", "");
                            }
                            else
                            {
                                Method = DiffMethod.abs;
                                targ = arg.Substring(3);
                            }

                            if (!decimal.TryParse(targ, out t))
                                throw new Exception(string.Format("Error attempting to parse Tolerance\n\t{0} -> {1} (error)", arg, targ));

                            Tolerance = t;
                            break;
                        case 'o':
                            Output = arg.Substring(arg.IndexOf(':') + 1);
                            break;
                        case 'f':
                            Filter = arg.Substring(arg.IndexOf(':') + 1);
                            break;
                        case 'g':
                            LoadGUI = arg == "-gui";
                            break;
                    }
                else
                    if (A == null)
                        A = arg;
                    else
                        B = arg;

            LoadGUI |= A == null || B == null || Method == DiffMethod.nul;
        }
    }

}
