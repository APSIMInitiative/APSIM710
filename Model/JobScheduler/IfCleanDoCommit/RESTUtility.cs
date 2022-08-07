// -----------------------------------------------------------------------
// <copyright file="Utility.cs" company="CSIRO">
// TODO: Update copyright text.
// </copyright>
// -----------------------------------------------------------------------

namespace Utils
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Text;
    using System.Net;
    using System.IO;
    using System.Xml.Serialization;
    using System.Xml;

    /// <summary>
    /// TODO: Update summary.
    /// </summary>
    public class REST
    {
        /// <summary>Call REST web service.</summary>
        /// <typeparam name="T">The return type</typeparam>
        /// <param name="url">The URL of the REST service.</param>
        /// <returns>The return data</returns>
        public static T CallService<T>(string url, string headerName = null, string headerValue = null)
        {
            WebRequest wrGETURL;
            wrGETURL = WebRequest.Create(url);
            wrGETURL.Method = "GET";
            wrGETURL.ContentType = @"application/xml; charset=utf-8";
            wrGETURL.ContentLength = 0;
            if (!string.IsNullOrEmpty(headerName) && !string.IsNullOrEmpty(headerValue))
            {
                Console.WriteLine("Header {headerName} is being added to web service call");
                wrGETURL.Headers.Add(headerName, headerValue);
            }
            using (HttpWebResponse webresponse = wrGETURL.GetResponse() as HttpWebResponse)
            {
                Encoding enc = System.Text.Encoding.GetEncoding("utf-8");
                // read response stream from response object
                using (StreamReader loResponseStream = new StreamReader(webresponse.GetResponseStream(), enc))
                {
                    string st = loResponseStream.ReadToEnd();
                    if (typeof(T).Name == "Object" || st == null || st == string.Empty)
                        return default(T);

                    XmlSerializer serializer = new XmlSerializer(typeof(T));

                    //ResponseData responseData;
                    return (T)serializer.Deserialize(new NamespaceIgnorantXmlTextReader(new StringReader(st)));
                }
            }
        }

        /// <summary>Helper class to ignore namespaces when de-serializing</summary>
        public class NamespaceIgnorantXmlTextReader : XmlTextReader
        {
            /// <summary>Constructor</summary>
            /// <param name="reader">The text reader.</param>
            public NamespaceIgnorantXmlTextReader(System.IO.TextReader reader) : base(reader) { }

            /// <summary>Override the namespace.</summary>
            public override string NamespaceURI
            {
                get { return ""; }
            }
        }

        /// <summary>Return the valid password for this web service.</summary>
        public static string GetValidPassword()
        {
            string pwfile = @"C:\dbConnect.txt";
            if (!File.Exists(pwfile)) { pwfile = "/etc/dbConnect.txt"; }
            string connectionString = File.ReadAllText(pwfile).TrimEnd(new char[] { '\r', '\n' });
            int posPassword = connectionString.IndexOf("Password=");
            return connectionString.Substring(posPassword + "Password=".Length);
        }
    }
}
