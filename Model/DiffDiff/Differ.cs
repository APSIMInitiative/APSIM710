using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace DiffDiff
{

    public enum DiffMethod { pct, abs, dp, nul }

    public class Differ
    {
        internal static List<string> GetCommonColNames(InputFile a, InputFile b)
        {
            List<string>
                final = new List<string>(),
                templist = new List<string>(a.ColNames);

            foreach (string bit in b.ColNames)
                if (templist.Contains(bit))
                    final.Add(bit);

            return final;
        }

        /// <summary>
        /// Diff the given columns of two given files using a given tolerance and method reporting any information back to the StringBuilder passed by ref
        /// </summary>
        /// <param name="origfile">the original file</param>
        /// <param name="newfile">the new file to diff against the original file</param>
        /// <param name="colnames">the colnames to focus on</param>
        /// <param name="tolerance">the toleracne to use (absolute value/dp/%age depedning on 'method')</param>
        /// <param name="method">the method to use when diffing (absolute value/dp/%age)</param>
        /// <param name="output">a StringBuilder for reporting</param>
        /// <returns>true if files are approximately the same (within the given tolerance)</returns>
        internal static bool Diff(InputFile origfile, InputFile newfile, List<string> colnames, decimal tolerance, DiffMethod method, ref StringBuilder output)
        {
            Dictionary<string, int>
                orig_lookup = new Dictionary<string, int>(),
                new_lookup = new Dictionary<string, int>();

            List<string>
                orig_cols = new List<string>(origfile.ColNames),
                new_cols = new List<string>(newfile.ColNames);

            //basic little factory for setting up our comparer object
            CompareMethod comparer =
                method == DiffMethod.pct ? (CompareMethod)new PctCompare(tolerance) :
                 method == DiffMethod.abs ? (CompareMethod)new AbsCompare(tolerance) :
                  (CompareMethod)new DPCompare(tolerance);

            //set up a lookup dictionary to translate between column name and index
            foreach (string col in colnames)
            {
                orig_lookup.Add(col, orig_cols.IndexOf(col));
                new_lookup.Add(col, new_cols.IndexOf(col));
            }

            //grab the raw data to diff
            object[][]
                origdata = origfile.GetData(true),
                newdata = newfile.GetData(true);

            string result = "";

            if (origdata.Length != newdata.Length)
                result += "The 2 files had different lengths, diff will end at bottom of shortest file" + Environment.NewLine;

            object diff;

            int
                badhits = 0,
                ibadline = -1,
                numlines = (int)Math.Min(origdata.Length, newdata.Length);

            //for each line in our files
            for (int i = 0; i < numlines; i++)
                //for each column of interest
                foreach (string col in colnames)
                    //if we don't have a 'match' (comparer.match takes care of the tolerances for us)
                    if (!comparer.match(origdata[i][orig_lookup[col]], newdata[i][new_lookup[col]], out diff))
                    {
                        badhits++;
                        //if we haven't picked up any fails on this line yet
                        if (ibadline != i)
                        {
                            //if this isn't the first line of our report then make sure we add a newline
                            if (ibadline >= 0)
                                output.AppendLine();
                            //if it is the first line then make sure we make it clear the file failed
                            else
                                output.AppendLine("FAIL");

                            //print the line number (remember this is the first fail for this line)
                            output.AppendFormat("\t{0,-4}:", i);
                            ibadline = i;
                        }

                        //print out the column name of the value that failed, along with the difference
                        output.AppendFormat("\t{0}:{1}", col, diff);
                    }

            if (badhits == 0)
                output.AppendLine("PASS");

            return badhits == 0;
        }

        public abstract class CompareMethod
        {
            public abstract bool match(object origval, object newval, out object output);

            protected bool CheckForStrings(object a, object b, out bool match)
            {
                string 
                    sa = a as string, 
                    sb = b as string;

                match = false;

                if (sa != null && sb != null)
                {
                    match = sa.ToLower() == sb.ToLower();
                    return true;
                }
                else
                    return sa != null || sb != null;

            }
        }

        public class PctCompare : CompareMethod
        {
            decimal tolerance;

            public PctCompare(decimal tolerance)
            {
                this.tolerance = decimal.Multiply(tolerance, 100);
            }

            public override bool match(object origval, object newval, out object output)
            {
                bool stringmatch = false;
                output = string.Format("{0} = {1}", origval, newval);

                if (!CheckForStrings(origval, newval, out stringmatch))
                {
                    decimal 
                        a = (decimal)origval,
                        b = (decimal)newval;

                    output = decimal.Subtract(b, a).ToString();

                    return a.Equals(b) ? true : Math.Abs(decimal.Subtract(a, b)) < decimal.Multiply(a, tolerance);
                }
                else 
                    return stringmatch;
            }
        }

        public class AbsCompare : CompareMethod
        {
            decimal tolerance;
            public AbsCompare(decimal tolerance)
            {
                this.tolerance = tolerance;
            }

            public override bool match(object origval, object newval, out object output)
            {
                bool stringmatch = false;
                output = string.Format("{0} = {1}", origval, newval);

                if (!CheckForStrings(origval, newval, out stringmatch))
                {
                    decimal 
                        a = (decimal)origval,
                        b = (decimal)newval;

                    output = decimal.Subtract(b, a).ToString();

                    return a.Equals(b) ? true : Math.Abs(decimal.Subtract(a, b)) < tolerance;
                }
                else 
                    return stringmatch;
            }
        }

        public class DPCompare : CompareMethod
        {
            int tolerance;
            public DPCompare(decimal tolerance)
            {
                this.tolerance = (int)Math.Round(tolerance);
            }

            public override bool match(object origval, object newval, out object output)
            {
                bool stringmatch;

                output = string.Format("{0} = {1}", origval, newval);

                if (!CheckForStrings(origval, newval, out stringmatch))
                {
                    decimal 
                        adj_orig = tolerance < 0 ? reverseTruncate((decimal)origval, tolerance) : truncate((decimal)origval, tolerance),
                        adj_new = tolerance < 0 ? reverseTruncate((decimal)newval, tolerance) : truncate((decimal)newval, tolerance);

                    output = decimal.Subtract(adj_new,adj_orig).ToString();

                    return adj_orig.Equals(adj_new);
                }
                else
                    return stringmatch;
                
            }

            /// <summary>
            /// Remove the last 'tolerance' digits from a decimal number (won't got past '.')
            /// </summary>
            /// <param name="dec_val"></param>
            /// <param name="tolerance"></param>
            /// <returns></returns>
            private decimal reverseTruncate(decimal dec_val, int tolerance)
            {
                //figure out how many dp we have
                string str_val = dec_val.ToString();
                int dp = str_val.Length - (str_val.IndexOf(".") + 1);

                return truncate(dec_val, (int)Math.Max(0, dp + tolerance));
            }

            /// <summary>
            /// Besaue 'decimal' is retarded and won't let you truncate a number to set number of DP we have to write our own method to do this
            /// <para>
            /// Basically just multiply given decimal number by 10^num_dp, remove anything after the '.', then divide it back out again
            /// </para>
            /// </summary>
            /// <param name="dec_val"></param>
            /// <param name="tolerance"></param>
            /// <returns></returns>
            private decimal truncate(decimal dec_val, int tolerance)
            {
                tolerance = (int)Math.Round(Math.Pow(10,tolerance));
                return decimal.Divide(decimal.Truncate(decimal.Multiply(dec_val, tolerance)), tolerance);
            }
        }
    }
}