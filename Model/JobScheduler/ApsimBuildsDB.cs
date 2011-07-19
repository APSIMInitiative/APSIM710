using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Net;
using System.IO;
using System.Text;
using System.Security.Authentication;
using System.Data.SqlClient;
using CSGeneral;

public class ApsimBuildsDB
{
    private SqlConnection Connection;
    private string JobID = null;

    /// <summary>
    /// Open the SoilsDB ready for use.
    /// </summary>
    public void Open()
    {
        //string ConnectionString = "Data Source=www.apsim.info\\SQLEXPRESS;Initial Catalog=\"APSIM Builds\";Integrated Security=True";
        string ConnectionString = "Data Source=www.apsim.info\\SQLEXPRESS;Database=\"APSIM Builds\";Trusted_Connection=True;User ID=apsrunet;password=CsiroDMZ!";
        Connection = new SqlConnection(ConnectionString);
        Connection.Open();
    }

    /// <summary>   
    /// Close the SoilsDB connection
    /// </summary>
    public void Close()
    {
        if (Connection != null)
        {
            Connection.Close();
            Connection = null;
        }
    }

    /// <summary>
    /// Add a new entry to the builds database.
    /// </summary>
    public void Add(string UserName, string Password, string PatchFileName, string Description, int BugID, bool DoCommit)
    {
        Dummy();
        string SQL = "INSERT INTO BuildJobs (UserName, Password, PatchFileName, Description, BugID, DoCommit) " +
                     "VALUES (@UserName, @Password, @PatchFileName, @Description, @BugID, @DoCommit)";

        SqlCommand Cmd = new SqlCommand(SQL, Connection);

        Cmd.Parameters.Add(new SqlParameter("@UserName", UserName));
        Cmd.Parameters.Add(new SqlParameter("@Password", Password));
        Cmd.Parameters.Add(new SqlParameter("@PatchFileName", PatchFileName));
        Cmd.Parameters.Add(new SqlParameter("@Description", Description));
        Cmd.Parameters.Add(new SqlParameter("@BugID", BugID));
        if (DoCommit)
            Cmd.Parameters.Add(new SqlParameter("@DoCommit", "1"));
        else
            Cmd.Parameters.Add(new SqlParameter("@DoCommit", "0"));
        Cmd.ExecuteNonQuery();
    }

    /// <summary>
    /// Get the patch file name for the current build job.
    /// </summary>
    public string GetPatchFileName()
    {
        GetJobID();
        if (JobID == null)
            return "";
        string FileName = "";
        string SQL = "SELECT PatchFileName FROM BuildJobs WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        SqlDataReader Reader = Command.ExecuteReader();
        if (Reader.Read())
            FileName = Reader["PatchFileName"].ToString();
        Reader.Close();
        return FileName;
    }

    public Dictionary<string, object> GetDetails()
    {
        GetJobID();

        string SQL = "SELECT * FROM BuildJobs WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        SqlDataReader Reader = Command.ExecuteReader();
        if (Reader.Read())
        {
            Dictionary<string, object> Items = new Dictionary<string, object>();
            for (int i = 0; i < Reader.FieldCount; i++)
            {
                Items.Add(Reader.GetName(i), Reader[i]);
            }
            Reader.Close();
            return Items;
        }
        else
        {
            Reader.Close();
            return null;
        }
    }

    /// <summary>
    /// Update the status of the current build job.
    /// </summary>
    public void UpdateStatus(string NewStatus)
    {
        GetJobID();
        string SQL = "UPDATE BuildJobs SET Status = '" + NewStatus + "' WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }

    /// <summary>
    /// Update the status of the current build job.
    /// </summary>
    public void UpdateStartDateToNow()
    {
        GetJobID();
        string NowString = DateTime.Now.ToString("yyyy-MM-dd hh:mm");
        string SQL = "UPDATE BuildJobs SET StartTime = '" + NowString + "' WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }

    /// <summary>
    /// Update the status of the current build job.
    /// </summary>
    public void UpdateEndDateToNow()
    {
        GetJobID();
        string NowString = DateTime.Now.ToString("yyyy-MM-dd hh:mm");
        string SQL = "UPDATE BuildJobs SET FinishTime = '" + NowString + "' WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }

    /// <summary>
    /// Update the revision number for the current build job.
    /// </summary>
    public void UpdateRevisionNumber(int RevisionNumber)
    {
        GetJobID();
        Dummy();
        string SQL = "UPDATE BuildJobs SET RevisionNumber = " + RevisionNumber.ToString() + " WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }

    /// <summary>
    /// Update the paths for all the revision number for the current build job.
    /// </summary>
    public void UpdateDiffFileName(string DiffsFileName)
    {
        GetJobID();
        string SQL = "UPDATE BuildJobs SET DiffsFileName = " + StringManip.DQuote(DiffsFileName) +
                                         " WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }

    /// <summary>
    /// Set the number of diffs.
    /// </summary>
    public void SetNumDiffs(int NumDiffs)
    {
        GetJobID();
        string SQL = "UPDATE BuildJobs SET NumDiffs = " + NumDiffs.ToString() + " WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }

    /// <summary>
    /// Set the number of diffs.
    /// </summary>
    public int GetNumDiffs()
    {
        int NumDiffs = -1;
        GetJobID();
        string SQL = "SELECT NumDiffs FROM BuildJobs WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        SqlDataReader Reader = Command.ExecuteReader();
        if (Reader.Read())
            NumDiffs = Convert.ToInt32(Reader["NumDiffs"]);
        Reader.Close();
        if (NumDiffs == -1)
            throw new Exception("Cannot find number of diffs");
        return NumDiffs;
    }

    /// <summary>
    /// Updates a field in the database for the current job with the specified value.
    /// </summary>
    public void UpdateField(string FieldName, string FieldValue)
    {
        GetJobID();
        string SQL = "UPDATE BuildJobs SET " + FieldName + " = " + StringManip.DQuote(FieldValue) +
                                         " WHERE ID = " + JobID;

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }


    /// <summary>
    /// Get the ID of the current build job.
    /// </summary>
    private void GetJobID()
    {
        if (JobID == null)
        {
            string SQL = "SELECT ID FROM BuildJobs WHERE Status = 'Running' or Status IS NULL ORDER BY ID";
            
            SqlCommand Command = new SqlCommand(SQL, Connection);
            SqlDataReader Reader = Command.ExecuteReader();
            if (Reader.Read())
                JobID = Reader["ID"].ToString();
            Reader.Close();
        }
    }



    /// <summary>
    /// A dummy method for altering the database structure.
    /// </summary>
    public void Dummy()
    {
        //string SQL = "ALTER TABLE BuildJobs ALTER COLUMN Status char(30)";
        //string SQL = "ALTER TABLE BuildJobs ADD NumDiffs int";
        //string SQL = "ALTER TABLE BuildJobs ADD BuildTreeFileName varchar";
        //string SQL = "ALTER TABLE BuildJobs DROP COLUMN RevisionNumber";
        //string SQL = "DELETE FROM BuildJobs";
        string SQL = "ALTER TABLE BuildJobs ADD DoCommit int";

        SqlCommand Command = new SqlCommand(SQL, Connection);
        Command.ExecuteNonQuery();
    }



}