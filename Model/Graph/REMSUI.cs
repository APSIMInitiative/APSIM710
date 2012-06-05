using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.IO;
using System.Data.OleDb;

namespace Graph
{
    public partial class REMSUI : Graph.DataUserInterface
    {
        private List<string> ExperimentNames = new List<string>();
        private List<int> ExperimentIDs = new List<int>();
        private List<string> TreatmentNames = new List<string>();
        private List<int> TreatmentIDs = new List<int>();

        public REMSUI()
        {
            InitializeComponent();
        }

        public override void OnRefresh()
        {
            base.OnRefresh();

            DataGrid.BringToFront();

            string St = XmlHelper.Value(Data, "FileName");
            if (St != FileNameBox.Text)
                FileNameBox.Text = XmlHelper.Value(Data, "FileName");

            St = XmlHelper.Value(Data, "Experiment");
            if (St != ExperimentCmboBox.Text)
            {
                ExperimentCmboBox.Text = XmlHelper.Value(Data, "Experiment");
                LookupExperimentNames();
            }
            St = XmlHelper.Value(Data, "Treatment");
            if (St != TreatmentComboBox.Text)
            {
                TreatmentComboBox.Text = XmlHelper.Value(Data, "Treatment");
                LookupTreatmentNames();
            }

            St = XmlHelper.Value(Data, "DataSource");
            if (St != DataSourceComboBox.Text)
                DataSourceComboBox.Text = XmlHelper.Value(Data, "DataSource");

            if (DataSourceComboBox.Items.Count == 0)
            {
                DataSourceComboBox.Items.Add("Plot");
                DataSourceComboBox.Items.Add("Crop");
            }
            // need to populate drop downs from MDB

        }

        private void BrowseButton_Click(object sender, EventArgs e)
        {
            if (OpenFileDialog.ShowDialog() == DialogResult.OK)
                FileNameBox.Text = OpenFileDialog.FileName;
        }

        private void FileNameBox_TextChanged(object sender, EventArgs e)
        {
            XmlHelper.SetValue(Data, "FileName", FileNameBox.Text);
            Save();
            OnRefresh();
            LookupExperimentNames();
        }

        private void ExperimentCmboBoxChanged(object sender, EventArgs e)
        {
            XmlHelper.SetValue(Data, "Experiment", ExperimentCmboBox.Text);
            Save();
            OnRefresh();
            LookupTreatmentNames();
        }

        private void TreatmentComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            XmlHelper.SetValue(Data, "Treatment", TreatmentComboBox.Text);
            Save();
            OnRefresh();
        }

        private void DataSourceComboBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            XmlHelper.SetValue(Data, "DataSource", DataSourceComboBox.Text);
            Save();
            OnRefresh();
        }

        void Save()
        {
            ApsimFile.Component C = Controller.ApsimData.Find(NodePath);
            C.Contents = Data.OuterXml;
        }

        void LookupExperimentNames()
        {
            ExperimentCmboBox.Items.Clear();
            string fileName = FileNameBox.Text;
            if (File.Exists(fileName))
            {
                OleDbConnection con = null;
                try
                {
                    string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
                    fileName + ";Persist Security Info=False";
                    con = new OleDbConnection(provider);  // connection string change database name and password here.
                    con.Open(); //connection must be openned
                    OleDbCommand cmd = new OleDbCommand("SELECT Experiments.ExpID, Experiments.Experiment FROM Experiments;", con); // creating query command
                    OleDbDataReader reader = cmd.ExecuteReader(); // executes query
                    while (reader.Read()) // if can read row from database
                    {
                        string ExperimentName = reader.GetValue(1).ToString();
                        ExperimentNames.Add(ExperimentName);
                        ExperimentIDs.Add(Convert.ToInt32(reader.GetValue(0)));
                        ExperimentCmboBox.Items.Add(ExperimentName);
                    }
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK);
                }
                finally
                {
                    con.Close();  // finally closes connection
                }
                

            }
        }


        void LookupTreatmentNames()
        {
            string fileName = FileNameBox.Text;
            string ExperimentID = ExperimentCmboBox.Text;
            TreatmentComboBox.Items.Clear();
            if (File.Exists(fileName) && ExperimentID != "")
            {
                OleDbConnection con = null;
                try
                {
                    string provider = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=" +
                    fileName + ";Persist Security Info=False";
                    con = new OleDbConnection(provider);  // connection string change database name and password here.
                    con.Open(); //connection must be openned

                    String SQL = "TRANSFORM First(Levels.Level) AS FirstOfLevel " +
                                 "SELECT Experiments.ExpID, Treatments.TreatmentName, Designs.TreatmentID " +
                                 "FROM (Experiments INNER JOIN Treatments ON Experiments.ExpID = " +
                                 "Treatments.ExpID) INNER JOIN ((Factors INNER JOIN Levels ON " +
                                 "Factors.FactorID = Levels.FactorID) INNER JOIN Designs ON Levels.LevelID = " +
                                 "Designs.LevelID) ON Treatments.TreatmentID = Designs.TreatmentID " +
                                 "WHERE (Experiments.Experiment=\"" + ExperimentID + "\") " +
                                 "GROUP BY Experiments.ExpID, Treatments.TreatmentName, Designs.TreatmentID " +
                                 "ORDER BY Designs.TreatmentID " +
                                 "PIVOT Factors.Factor;";

                    OleDbCommand cmd = new OleDbCommand(SQL, con); // creating query command
                    OleDbDataReader reader = cmd.ExecuteReader(); // executes query
                    while (reader.Read()) // if can read row from database
                    {
                        string TreatmentName = reader.GetValue(1).ToString();
                        TreatmentNames.Add(TreatmentName);
                        TreatmentIDs.Add(Convert.ToInt32(reader.GetValue(2)));
                        TreatmentComboBox.Items.Add(TreatmentName);
                    }
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK);
                }
                finally
                {
                    con.Close();  // finally closes connection
                }
            }
        }



    }
}

