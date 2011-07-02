using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace DiffDiff
{
    class InputFile
    {
        string filename;

        public string[] ColNames { get; private set; }

        public int DataStartLine { get; private set; }

        public InputFile(string filename)
        {
            // TODO: Complete member initialization
            this.filename = filename;

            LoadCols();
        }

        private void LoadCols()
        {
            using (StreamReader sr = new StreamReader(filename))
            {
                sr.ReadLine();
                sr.ReadLine();
                string[] rawcols = sr.ReadLine().Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
                List<string> finalcols = new List<string>();
                Dictionary<string, int> dups = new Dictionary<string,int>();

                foreach (string colname in rawcols)
                    if (finalcols.Contains(colname))
                    {
                        if (dups.ContainsKey(colname))
                            dups[colname]++;
                        else
                            dups.Add(colname, 1);

                        finalcols.Add(colname + "_" + dups[colname].ToString());
                    }
                    else
                        finalcols.Add(colname);

                ColNames = finalcols.ToArray();
            }
        }

        public object[][] GetData(bool convert)
        {
            object[][] data = null;

            using (StreamReader sr = new StreamReader(filename))
            {
                sr.ReadLine();
                sr.ReadLine();
                sr.ReadLine();

                string[] lines = sr.ReadToEnd().Split(new string[] { Environment.NewLine }, StringSplitOptions.RemoveEmptyEntries);

                int istart = lines[0].Contains("(") ? 1 : 0;

                DataStartLine = istart + 4;

                data = new object[lines.Length - istart][];

                for (int i = istart; i < lines.Length; i++)
                    ToObjectArray(lines[i], ref data[i - istart], convert);
            }

            return data;
        }

        IConverter[] converters;

        private void ToObjectArray(string p, ref object[] result, bool convert)
        {
            string[] elements = p.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries);
            result = new object[elements.Length];

            if (converters == null)
            {
                decimal temp = 0;
                converters = new IConverter[elements.Length];
                for (int i = 0; i < elements.Length; i++)
                    if (convert && decimal.TryParse(elements[i], out temp))
                    {
                        converters[i] = new DecimalConverter();
                        result[i] = temp;
                    }
                    else
                    {
                        converters[i] = new StringConverter();
                        result[i] = elements[i];
                    }
            }
            else
                for (int i = 0; i < elements.Length; i++)
                    result[i] = converters[i].convert(elements[i]);

        }

        interface IConverter
        {
            object convert(string element);
        }

        class StringConverter : IConverter
        {
            public object convert(string element)
            {
                return element;
            }
        }

        class DecimalConverter : IConverter
        {
            public object convert(string element)
            {
                return decimal.Parse(element);
            }
        }

    }
}
