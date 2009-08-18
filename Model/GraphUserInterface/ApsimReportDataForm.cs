
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

using System.Xml;

using Controllers;
using CSGeneral;
using UIUtility;
using ApsimFile;


namespace GraphUserInterface
    {
    public partial class ApsimReportDataForm : Form
        {
        private UIUtility.DataProcessor Processor;
        public ApsimReportDataForm()
            {
            InitializeComponent();
            }

        public void Go(string CommandLine)
            {
            if (CommandLine == "")
                MessageBox.Show("Invalid command line to ApsimReportData: " + CommandLine);
            else
                {
                Application.EnableVisualStyles();
                Application.DoEvents();
                Application.DoEvents();

                PlugIns.Load(Configuration.ApsimDirectory() + "\\UserInterface\\ApsimReport.xml");
                Processor = new UIUtility.DataProcessor(Convert.ToUInt32(CommandLine));
                ChartPage.Processor = Processor;


                BaseController Controller = new BaseController(null, "ApsimUI", false);
                Controller.ApsimData.New(Processor.XML());
                ChartPage.OnLoad(Controller, "/Data", Processor.XML());
                ChartPage.OnRefresh();
                this.ShowDialog();
                }
            }

        private void OnFormClosing(object sender, FormClosingEventArgs e)
            {
            ChartPage.OnClose(); 
            string Contents = ChartPage.GetData();
            XmlDocument Doc = new XmlDocument();
            Doc.LoadXml(Contents);
            foreach (XmlNode Child in XmlHelper.ChildNodes(Doc.DocumentElement, ""))
                ChartPage.Processor.SetNoRefresh(Child.OuterXml);

            }


        }
    }