using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using System.Collections.Specialized;

namespace CSUserInterface
{
    public partial class Factor : Controllers.BaseView
    {
        DataTable Table = null;

        public Factor()
        {
            InitializeComponent();
            MyHelpLabel.Visible = false;
        }
        protected override void OnLoad()
        {
            FactorTargets.OnLoad(Controller, NodePath, Data.OuterXml);
        }
        public override void OnRefresh()
        {
            FactorTargets.OnRefresh();
            //ApsimFile.Component comp = Controller.ApsimData.Find(NodePath);
            if (Table != null)
            {
                Table.Clear();
                Table.Columns.Clear();
            }
            LoadManagerVariables();
        }
        public override void OnSave()
        {
            FactorTargets.OnSave();
            XmlDocument doc = new XmlDocument();
            doc.LoadXml(FactorTargets.GetData());
            Data.InnerXml = doc.DocumentElement.InnerXml;

            if (Table != null)
            {
                XmlNode variablesNode = Data.SelectSingleNode("//vars");
                if (variablesNode == null)
                    variablesNode = Data.AppendChild(Data.OwnerDocument.CreateElement("vars"));
                variablesNode.RemoveAll();
                if (Table.Rows.Count >= 10000)
                {
                    string[] values = DataTableUtility.GetColumnAsStrings(Table, Table.Columns[0].ColumnName);
                    string csvValue = "";
                    foreach (string value in values)
                    {
                        if (value != "")
                        {
                            if (csvValue != "")
                                csvValue += ",";
                            csvValue += value;
                        }
                        else
                            break;
                    }
                    XmlNode varNode = variablesNode.AppendChild(Data.OwnerDocument.CreateElement(Table.Columns[0].ColumnName));
                    varNode.InnerText = csvValue;
                }
                else
                {
                    foreach (DataRow row in Table.Rows)
                    {
                        if ((bool)row[0])
                        {
                            XmlNode varNode = variablesNode.AppendChild(Data.OwnerDocument.CreateElement(row[1].ToString()));
                            varNode.InnerText = row[3].ToString();
                        }
                    }
                }
            }
        }

        public override bool OnDropData(StringCollection SourcePaths, string FullXML)
        {
            //if there are no children, then add the droppednodes as targets
            //don't add to targets if there is existing children
            //don't add to targets if the source is from the Factorial Tree
            ApsimFile.Component thisComp = Controller.ApsimData.Find(NodePath);
            //if (thisComp.ChildNodes.Count == 0)
            {
                FactorTargets.AddTargets(SourcePaths);
            }
            thisComp.Add(FullXML);
            OnRefresh();
            return true;
        }
        public void LoadVariableTypes(XmlNode metNode, List<string> variableTypes)
        {
            if (metNode != null && variableTypes != null)
            {
                foreach (XmlNode varNode in XmlHelper.ChildNodes(metNode, "facvar"))
                {
                    variableTypes.Add(varNode.InnerText.ToLower());
                }
            }
        }

        public void LoadManagerVariables()
        {
            //if there is a single manager component as a child, then allow parameters to be defined
            //parameters will be stored in a separate node "variables"
            //we don't allow combination similar to complexfactor... yet.
            ApsimFile.Component thisComp = Controller.ApsimData.Find(NodePath);
            if (thisComp.ChildNodes.Count == 1)
            {
                ApsimFile.Component childComp = thisComp.ChildNodes[0];

                List<string> variableTypes = new List<string>();
                LoadVariableTypes(Types.Instance.MetaDataNode("Factor", "FactorVariables"), variableTypes);
                if (variableTypes.Contains(childComp.Type.ToLower()))
                {
                    Table = CreateTable(childComp);
                }
            }
            //gridManager.DataSourceTable = Table;
            gridManager.DataSource = Table;



            //Size the grid columns sensibly
            gridManager.AutoSizeColumnsMode = DataGridViewAutoSizeColumnsMode.None;
            foreach (DataGridViewColumn col in gridManager.Columns)
            {
                // It would be clearer to use the Column.GetPreferredWidth function, but this is
                // broken on Mono (always returns 0), so instead we can temporarily let the column
                // auto-size itself, get it's width, then turn off auto-sizing and apply the width.
                // We don't want to leave auto-sizing on, since that disables the user's ability
                // to resize the columns
                col.AutoSizeMode = DataGridViewAutoSizeColumnMode.AllCells;
                int w = col.Width;
                col.AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
                col.Width = w;
                col.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleLeft;
            }
            if (gridManager.Columns.Count > 3)
            {
                //gridManager.Columns[1].Visible = false;
                //gridManager.Columns[2].AutoSizeMode = DataGridViewAutoSizeColumnMode.Fill;
                gridManager.Columns[2].AutoSizeMode = DataGridViewAutoSizeColumnMode.None;
            }

        }
        public DataTable CreateTable(ApsimFile.Component childComp)
        {
            DataTable Table = new DataTable();


            XmlNodeList uiNodes = childComp.ContentsAsXML.SelectNodes("//ui/* | //CustomUI/*");
            bool isManager = true;
            if (uiNodes.Count == 0)
            {
                uiNodes = childComp.ContentsAsXML.ChildNodes;
                isManager = false;
            }

            bool useSingleColumnTable = uiNodes.Count == 1 || childComp.Type == "siloinput";

            if (useSingleColumnTable)
            {
                csvLabel.Visible = false;

                XmlNodeList foundNodes = Data.SelectNodes("//vars/" + uiNodes[0].Name);
                string[] values = null;
                if (foundNodes.Count == 1)
                    values = foundNodes[0].InnerText.Split(",".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                else
                    values = new string[0];
                DataTableUtility.AddColumn(Table, uiNodes[0].Name, values);
                // If there is only 1 variable, then turn the table into lots of rows so that the user can
                // enter one value per row rather than all on one line csv style.
                for (int i = 0; i < 10000; i++)
                    Table.Rows.Add(Table.NewRow());

            }
            else
            {
                csvLabel.Visible = true;
                Table.Columns.Add(" ", typeof(System.Boolean));
                Table.Columns.Add("names", typeof(string));
                Table.Columns.Add("Variables", typeof(string));
                Table.Columns.Add("Parameters", typeof(string));
                foreach (XmlNode ui in uiNodes)
                {
                    DataRow newRow = Table.NewRow();
                    newRow[0] = false;

                    string varName = "";
                    string text = "";
                    if (isManager)
                    {
                        varName = ui.Name;
                        if (ui.Attributes["description"] != null)
                            text = ui.Attributes["description"].Value;
                        else
                            text = varName;
                    }
                    else
                    {
                        varName = XmlHelper.Name(ui);
                    }
                    newRow[1] = varName;
                    newRow[2] = text;
                    newRow[3] = ui.InnerText;

                    //look for corresponding node in the variables node
                    if (varName != "#comment")
                    {
                        XmlNodeList foundNodes = Data.SelectNodes("//vars/" + varName);
                        if (foundNodes.Count == 1)
                        {
                            XmlNode varNode = foundNodes[0];
                            if (varNode != null)
                            {
                                newRow[0] = true;
                                newRow[3] = varNode.InnerText;
                            }
                        }
                        Table.Rows.Add(newRow);
                    }
                }
            }




            return Table;
        }
    }
}
