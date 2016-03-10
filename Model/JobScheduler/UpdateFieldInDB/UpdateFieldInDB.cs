using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Net;
using System.Xml;
using System.Xml.Serialization;
using CSGeneral;

class Program
{
    /// <summary>
    /// This program updates a field in the database for the current build job.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 2)
                throw new Exception("Usage: UpdateFieldInDB FieldName Value");

            DBUpdateField(args[0], args[1].Replace("\"", ""));
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void DBUpdateField(string name, string value)
    {
        int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));
        string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateField" +
            "?JobID=" + JobID +
            "&FieldName=" + name +
            "&FieldValue=" + value +
            "&DbConnectPassword=" + GetValidPassword();
        CallRESTService<object>(url);
    }

    /// <summary>Call REST web service.</summary>
    /// <typeparam name="T">The return type</typeparam>
    /// <param name="url">The URL of the REST service.</param>
    /// <returns>The return data</returns>
    public static T CallRESTService<T>(string url)
    {
        WebRequest wrGETURL;
        wrGETURL = WebRequest.Create(url);
        wrGETURL.Method = "GET";
        wrGETURL.ContentType = @"application/xml; charset=utf-8";
        wrGETURL.ContentLength = 0;
        using (HttpWebResponse webresponse = wrGETURL.GetResponse() as HttpWebResponse)
        {
            Encoding enc = System.Text.Encoding.GetEncoding("utf-8");
            // read response stream from response object
            using (StreamReader loResponseStream = new StreamReader(webresponse.GetResponseStream(), enc))
            {
                string st = loResponseStream.ReadToEnd();
                if (typeof(T).Name == "Object")
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
        string pwfile = @"C:\ChangeDBPassword.txt";
        if (!File.Exists(pwfile)) { pwfile = "/etc/dbConnect.txt"; }
        string connectionString = File.ReadAllText(pwfile).TrimEnd(new char[] { '\r', '\n' });
        int posPassword = connectionString.IndexOf("Password=");
        return connectionString.Substring(posPassword + "Password=".Length);
    }

}
  
