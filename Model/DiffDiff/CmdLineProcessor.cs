using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace DiffDiff
{
    public class CmdLineProcessor
    {
       Args args;

        TextWriter report;

        public CmdLineProcessor(string[] args)
            : this(new Args(args))
        { }

        /// <summary>
        /// Wrapper for 'Differ' that provides command-line functionality.  Capable of diffing entire folders or single files
        /// </summary>
        /// <param name="args">'Args' object containing information on command line arguments passed in</param>
        public CmdLineProcessor(Args args)
        {
            this.args = args;
            if (args.ReportToConsole)
                report = Console.Out;
            else if (args.Output != null && !Directory.Exists(args.Output))
                report = new StreamWriter(args.Output);
        }

        /// <summary>
        /// Actually run the Differ using the arguments provided to the constructor
        /// </summary>
        /// <returns>The number of files that failed diffs</returns>
        public int Run()
        {
            //load our inputs into Dictionairies.  'Key' is Path.FileNameWithoutExtension('Value')
            //'Pairify' matches the a and b dictionary keys up, returning a dictionary with 'b' keys as the 'Key's and 'a' keys as 
            //the 'Value's (this is how we figure out which files to diff against)
            Dictionary<string, string>
                a = Dictionaryify(getFiles(args.A, args.Filter)),
                b = Dictionaryify(getFiles(args.B, args.Filter)),
                pairs = Pairify(a, b);

            int bad_result = 0;
            bool report_individually = report == null;

            //process the files
            foreach (KeyValuePair<string, string> pair in pairs)
            {
                bool reported = false;

                //stringbuilder used for reporting
                StringBuilder info = new StringBuilder();
                try
                {
                    //append file names in the order 'new' then 'orig'
                    info.AppendFormat("{0}\t{1}\t", b[pair.Key], a[pair.Value]);

                    //load our files into a form where the differ can recognise them
                    InputFile
                        f_orig = new InputFile(a[pair.Value]),
                        f_new = new InputFile(b[pair.Key]);

                    //diff the files, if there are any errors it will return 'false' (StringBuilder 'info' is passed in by ref here for reporting purposes)
                    //note that all common columns will be diffed
                    if (!Differ.Diff(f_orig, f_new, Differ.GetCommonColNames(f_orig, f_new), args.Tolerance, args.Method, ref info))
                    {
                        //if we failed then increment our total fails
                        bad_result++;

                        //let 'finally' know we want to report a fail
                        reported = true;
                    }
                }
                catch (Exception e)
                {
                    //if there was an exception for this file then report it as a failed diff
                    bad_result++;
                    info.AppendFormat("ERROR\t{0}\n{1}\n", e.Message, e.StackTrace);
                    reported = true;
                }
                finally
                {
                    //if reporting individually, only report if there was an error (easier to tell which files failed then becasue they will the the only ones with logs)
                    if (report_individually && reported)
                        if (args.Output != null)
                            using (report = new StreamWriter(Path.Combine(args.Output, pair.Key + ".txt")))
                                report.Write(info.ToString());
                        else
                            using (report = new StreamWriter(b[pair.Key] + ".txt"))
                                report.Write(info.ToString());

                    //if not reporting individually the write 'info' to the log file regardless of whetehr it passed or failed
                    else if (!report_individually)
                        report.WriteLine(info.ToString());
                }
            }

            return bad_result;
        }

        /// <summary>
        /// Build a Dictionary using the keys from 'b'.  For each key in 'b' (and therefore 'result') the best matching key from 'a' will be found and used as the value for said key.
        /// Each pair will be matched on a 'best-fit' basis based on the number of characters that match between them
        /// </summary>
        /// <param name="a">first Dictionary (values of result)</param>
        /// <param name="b">second Dictionary (keys of result)</param>
        /// <returns>For each key in 'b' (and therefore 'result') the best matching key from 'a' will be found and used as the value for said key</returns>
        protected virtual Dictionary<string, string> Pairify(Dictionary<string, string> a, Dictionary<string, string> b)
        {
            Dictionary<string, string> result = new Dictionary<string, string>();

            foreach (string _b in b.Keys)
                result.Add(_b, match(_b, a.Keys));

            return result;
        }

        /// <summary>
        /// Look through 'a' to find the best match for '_b' using a very basic character matching technique (the first item in 'a' with the most matches wins)
        /// </summary>
        /// <param name="_b">the string we are trying to find a match for</param>
        /// <param name="a">the possible matches</param>
        /// <returns>the first best possible match from 'a' for '_b'</returns>
        private string match(string _b, ICollection<string> a)
        {
            int max = -1;
            string maxkey = "";

            if (a.Contains(_b))
                return _b;
            else
                foreach (string key in a)
                {
                    int score = 0;
                    for (int i = 0; i < key.Length && i < _b.Length; i++)
                        if (_b[i] == key[i])
                            score++;
                    if (score > max)
                    {
                        max = score;
                        maxkey = key;
                    }
                }

            return maxkey;
        }

        /// <summary>
        /// Given a list of files, return a Dictionary where the Keys are the file names without extensions and the values are the full file names
        /// </summary>
        /// <param name="files">An array of file names</param>
        /// <returns>A Dictionary where the Keys are the file names without extensions and the values are the full file names</returns>
        private Dictionary<string, string> Dictionaryify(string[] files)
        {
            Dictionary<string, string> result = new Dictionary<string, string>();
            foreach (string bit in files)
                result.Add(Path.GetFileNameWithoutExtension(bit), bit);

            return result;
        }

        /// <summary>
        /// Given an 'input' string and a filter return a list of files.  If input string IS a file then just return that in an array of Length 1, 
        /// otherwise assume a is a directory and use the filter to search it for files.  If neither method works a generic 'Exception' is thrown
        /// </summary>
        /// <param name="input">Either a file or folder path</param>
        /// <param name="filter">A valid file search filter</param>
        /// <returns>An array of file names</returns>
        string[] getFiles(string input, string filter)
        {
            if (File.Exists(input))
                return new string[] { input };
            else if (Directory.Exists(input))
                return Directory.GetFiles(input, filter);
            else throw new Exception("Error, input string was invalid (did not correspond to existing file or directory)");

        }
    }


  }
