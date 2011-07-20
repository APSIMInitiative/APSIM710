using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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

            Go(args[0], args[1].Replace("\"", ""));
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string FieldName, string Value)
    {
        int JobID = Convert.ToInt32(JobScheduler.TalkToJobScheduler("GetVariable~JobID"));

        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();

        DB.UpdateField(JobID, FieldName, Value);

        DB.Close();
    }
}
