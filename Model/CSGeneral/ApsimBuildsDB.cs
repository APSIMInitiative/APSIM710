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
using System.Threading;

namespace CSGeneral
{
    public class ApsimBuildsDB
    {
        private SqlConnection Connection;

        /// <summary>
        /// Open the SoilsDB ready for use.
        /// </summary>
        public void Open()
        {
            //string ConnectionString = "Data Source=www.apsim.info\\SQLEXPRESS;Initial Catalog=\"APSIM Builds\";Integrated Security=True";
            string ConnectionString = "Data Source=www.apsim.info\\SQLEXPRESS;Database=\"APSIM Builds\";Persist Security Info=True;User ID=sv-login-external;Password=P@ssword123";

            // There are often network intermittent issues so try 5 times to make a connection.
            for (int i = 0; i < 5; i++)
            {
                try
                {
                    Connection = new SqlConnection(ConnectionString);
                    Connection.Open();
                    return;
                }
                catch (Exception)
                {
                    Thread.Sleep(3 * 60 * 1000); // 3 minutes
                }
            }
            throw new Exception("Cannot open connection to SQL server: www.apsim.info");
        }

        /// <summary>   
        /// Close the SoilsDB connection
        /// </summary>
        public void Close()
        {
            if (Connection != null)
            {
                // There are often network intermittent issues so try 5 times to close a connection.
                for (int i = 0; i < 5; i++)
                {
                    try
                    {
                        Connection.Close();
                        Connection = null;
                        return;
                    }
                    catch (Exception)
                    {
                        Thread.Sleep(3 * 60 * 1000); // 3 minutes
                    }
                }
                throw new Exception("Cannot close connection to SQL server: www.apsim.info");
            }
        }

        private void ExecuteNonQuery(SqlCommand Cmd)
        {
            // There are often network intermittent issues so try 5 times to execute a non query
            for (int i = 0; i < 5; i++)
            {
                try
                {
                    Cmd.ExecuteNonQuery();
                    return;
                }
                catch (Exception)
                {
                    Thread.Sleep(3 * 60 * 1000); // 3 minutes
                }
            }
            throw new Exception("Cannot execute query to SQL server: www.apsim.info");
        }
        private SqlDataReader ExecuteReader(SqlCommand Cmd)
        {
            // There are often network intermittent issues so try 5 times to execute a reader query
            for (int i = 0; i < 5; i++)
            {
                try
                {
                    return Cmd.ExecuteReader();
                }
                catch (Exception)
                {
                    Thread.Sleep(1 * 60 * 1000); // 1 minutes
                }
            }
            throw new Exception("Cannot execute reader query to SQL server: www.apsim.info");
        }

        /// <summary>
        /// Add a new entry to the builds database.
        /// </summary>
        public void Add(string UserName, string Password, string PatchFileName, string Description, int BugID, bool DoCommit)
        {
            string SQL = "INSERT INTO BuildJobs (UserName, Password, PatchFileName, Description, BugID, DoCommit, Status) " +
                         "VALUES (@UserName, @Password, @PatchFileName, @Description, @BugID, @DoCommit, @Status, @linuxStatus)";

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
            Cmd.Parameters.Add(new SqlParameter("@Status", "Queued"));
            Cmd.Parameters.Add(new SqlParameter("@linuxStatus", "Queued"));
            ExecuteNonQuery(Cmd);
        }

        /// <summary>
        /// Return details about a specific job.
        /// </summary>
        public Dictionary<string, object> GetDetails(int JobID)
        {
            string SQL = "SELECT * FROM BuildJobs WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            SqlDataReader Reader = null;
            try
            {
                Reader = ExecuteReader(Command);
                if (Reader.Read())
                {
                    Dictionary<string, object> Items = new Dictionary<string, object>();
                    for (int i = 0; i < Reader.FieldCount; i++)
                        Items.Add(Reader.GetName(i), Reader[i]);
                    return Items;
                }
                else
                    return null;
            }
            finally
            {
                if (Reader != null)
                    Reader.Close();
            }

        }

        /// <summary>
        /// Get the current running job number.
        /// </summary>
        public int GetRunningJobID()
        {
            string SQL = "SELECT TOP 1 ID FROM BuildJobs WHERE Status = 'Running' ORDER BY ID DESC";
            SqlDataReader Reader = null;
            SqlCommand Command = new SqlCommand(SQL, Connection);
            try
            {
                Reader = ExecuteReader(Command);
                if (Reader.Read())
                    return Convert.ToInt32(Reader[0]);
                else
                    return -1;
            }
            finally
            {
                if (Reader != null)
                    Reader.Close();
            }
        }

        /// <summary>
        /// Get the current running job number.
        /// </summary>
        public string GetPatchFileName()
        {
            string SQL = "SELECT TOP 1 PatchFileName FROM BuildJobs WHERE Status = 'Running'";

            SqlCommand Command = new SqlCommand(SQL, Connection);
            SqlDataReader Reader = null;
            try
            {
                Reader = ExecuteReader(Command);
                if (Reader.Read())
                    return Reader[0].ToString();
                else
                    return "";
            }
            finally
            {
                if (Reader != null)
                    Reader.Close();
            }
        }

        /// <summary>
        /// Update the status of the specified build job.
        /// </summary>
        public void UpdateStatus(int JobID, string NewStatus)
        {
            string prefix = "";
            if (Environment.MachineName.ToUpper() != "BOB") prefix = Environment.MachineName;

            string SQL = "UPDATE BuildJobs SET " + prefix + "Status = '" + NewStatus + "' WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }

        /// <summary>
        /// Update the status of the specified build job.
        /// </summary>
        public void UpdateStartDateToNow(int JobID)
        {
            string NowString = DateTime.Now.ToString("yyyy-MM-dd hh:mm tt");
            string SQL = "UPDATE BuildJobs SET StartTime = '" + NowString + "' WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }

        /// <summary>
        /// Update the status of the specified build job.
        /// </summary>
        public void UpdateEndDateToNow(int JobID)
        {
            string NowString = DateTime.Now.ToString("yyyy-MM-dd hh:mm tt");
            string SQL = "UPDATE BuildJobs SET FinishTime = '" + NowString + "' WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }

        /// <summary>
        /// Update the revision number for the specified build job.
        /// </summary>
        public void UpdateRevisionNumber(int JobID, int RevisionNumber)
        {
            string SQL = "UPDATE BuildJobs SET RevisionNumber = " + RevisionNumber.ToString() + " WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }

        /// <summary>
        /// Update the paths for all the revision number for the specified build job.
        /// </summary>
        public void UpdateDiffFileName(int JobID, string DiffsFileName)
        {
            string prefix = "";
            if (Environment.MachineName.ToUpper() != "BOB") prefix = Environment.MachineName;

            string SQL = "UPDATE BuildJobs SET " + prefix + "DiffsFileName = '" + DiffsFileName + "'" +
                                                " WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }

        /// <summary>
        /// Set the number of diffs.
        /// </summary>
        public void SetNumDiffs(int JobID, int NumDiffs)
        {
            string prefix = "";
            if (Environment.MachineName.ToUpper() != "BOB") prefix = Environment.MachineName;
            string SQL = "UPDATE BuildJobs SET " + prefix + "NumDiffs = " + NumDiffs.ToString() + " WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }


        /// <summary>
        /// Updates a field in the database for the specified job with the specified value.
        /// </summary>
        public void UpdateField(int JobID, string FieldName, string FieldValue)
        {
            string SQL = "UPDATE BuildJobs SET " + FieldName + " = '" + FieldValue + "'" +
                                                " WHERE ID = " + JobID.ToString();

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }


        /// <summary>
        /// Find the next job to run.
        /// </summary>
        public int FindNextJob(string prefix)
        {
            int JobID = -1;
            string SQL = "SELECT ID FROM BuildJobs WHERE " + prefix + "Status = 'Queued' ORDER BY ID";

            SqlCommand Command = new SqlCommand(SQL, Connection);
            SqlDataReader Reader = ExecuteReader(Command);
            if (Reader.Read())
                JobID = Convert.ToInt32(Reader["ID"]);
            Reader.Close();
            return JobID;
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
            //string SQL = "ALTER TABLE BuildJobs ADD DoCommit int";
            string SQL = "ALTER TABLE BuildJobs ADD linuxDiffsFileName varchar";

            SqlCommand Command = new SqlCommand(SQL, Connection);
            ExecuteNonQuery(Command);
        }



    }

}