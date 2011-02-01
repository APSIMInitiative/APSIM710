using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using System.Xml;
using System.Threading;

namespace Graph
   {
   /// <summary>
   /// This class is a report class that will animate child charts. It does this
   /// by looking for a DateFilter component somewhere under each chart. When
   /// found it will set the DateFilter to: date = #M/dd/yyyy# and do this
   /// repeatedly for all dates.
   /// </summary>
   public partial class AnimatedReportUI : Graph.GraphReportUI
      {
      private Thread WorkerThread = null;
      private bool Quit;
      private bool Forward;
      DateTime StartDate;
      DateTime EndDate;
      DateTime CurrentDate;
      private TimeSpan OneDay = new TimeSpan(1, 0, 0, 0);
      private ApsimFile.Component OurComponent;
      private XmlDocument Doc;
      private List<ApsimFile.Component> DateFilterNodes;
      private TimeSpan Period;

      /// <summary>
      /// Constructor
      /// </summary>
      public AnimatedReportUI()
         {
         InitializeComponent();
         this.TrackBar.Scroll += new System.EventHandler(this.OnTrackbarScroll);
         }

      /// <summary>
      /// Refresh this report. This method will create a worker thread to animate the graphs.
      /// </summary>
      public override void OnRefresh()
         {
         // Refresh the base report.
         base.OnRefresh();

         // Make sure play is selected by default.
         OnPlayClick(null, null);
         Quit = false;
         Forward = true;

         // Find our ApsimFile.Component class. We'll need this to get the XML we're
         // working with and to find all DateFilter components underneath this report.
         OurComponent = Controller.ApsimData.Find(NodePath);

         // Go create a DataProcessor object. We'll need this to find some data later.
         Doc = new XmlDocument();
         Doc.LoadXml(OurComponent.FullXMLNoShortCuts());
         DataProcessor Processor = new DataProcessor();
         List<string> DefaultFileNames = new List<string>();
         UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, DefaultFileNames);
         Processor.DefaultOutputFileNames = DefaultFileNames;

         // Now go and try to find some data. We need data so that we can determine the
         // start and end period for the animation.
         DataTable PlotData = null;
         foreach (XmlNode Child in Doc.DocumentElement.ChildNodes)
            {
            PlotData = Processor.Go(Child, "");
            if (PlotData != null)
               break;
            }

         // If some data was found then work out the start and end date periods, locate
         // all child DataFilter nodes, and create a worker thread for the animation.
         if (PlotData != null && PlotData.Rows.Count > 0)
            {
            StartDate = DataTableUtility.GetDateFromRow(PlotData.Rows[0]);
            EndDate = DataTableUtility.GetDateFromRow(PlotData.Rows[PlotData.Rows.Count-1]);
            Period = EndDate - StartDate;
            CurrentDate = StartDate;

            // Go find all DateFilter components - we'll need them later.
            DateFilterNodes = new List<ApsimFile.Component>();
            FindAllRecursively(OurComponent, "DateFilter", ref DateFilterNodes);

            WorkerThread = new Thread(DoAnimation);
            WorkerThread.Start();
            }
         }


      /// <summary>
      /// Go locate all DateFilter components. Uses recursion.
      /// </summary>
      private void FindAllRecursively(ApsimFile.Component Comp, string Name, ref List<ApsimFile.Component> DateFilterComponents)
         {
         if (Comp.Name == Name)
            DateFilterComponents.Add(Comp);

         foreach (ApsimFile.Component Child in Comp.ChildNodes)
            FindAllRecursively(Child, Name, ref DateFilterComponents);
         }

      /// <summary>
      /// We need to trap the OnSave so that we can kill the worker thead and tell the base 
      /// report to save all changes the user may have made.
      /// </summary>
      public override void OnSave()
         {
         // Kill the worker thread.
         OnPauseClick(null, null);
         Quit = true;
         if (WorkerThread != null)
            WorkerThread.Abort();

         // Remove the filters we put in
         if (DateFilterNodes != null)
            {
            foreach (ApsimFile.Component DateFilter in DateFilterNodes)
               {
               XmlNode DateFilterNode = DateFilter.ContentsAsXML;
               XmlHelper.SetValue(DateFilterNode, "Filter", "");
               DateFilter.Contents = DateFilterNode.OuterXml;
               }
            }
         // Tell the base report to save any changes the user may have made.
         base.OnSave();
         }


      /// <summary>
      /// The main worker thread method.
      /// </summary>
      public void DoAnimation()
         {
         while (!Quit)
            {
            if (PlayButton.Checked)
               {
               // Update the graph with the current date.
               Invoke(new EventHandler(UpdateReport));
               }
            Thread.Sleep(20);
            }
         WorkerThread = null;
         }

      /// <summary>
      /// This method will do the actual updating of the report for the CurrentDate.
      /// </summary>
      private void UpdateReport(object o, System.EventArgs e)
         {
         // Go change all "DateFilter" objects to reflect the CurrentDate.
         foreach (ApsimFile.Component DateFilter in DateFilterNodes)
            {
            XmlNode DateFilterNode = DateFilter.ContentsAsXML;
            XmlHelper.SetValue(DateFilterNode, "Filter", "Date = #" + CurrentDate.ToString("M/d/yyyy") + "#");
            DateFilter.Contents = DateFilterNode.OuterXml;
            }

         // Tell all series to refresh themselves.
         RefreshSeries();

         // Update the DateLabel.
         DateLabel.Text = CurrentDate.ToShortDateString();
         
         // Update the TrackBar.
         this.TrackBar.Scroll -= new System.EventHandler(this.OnTrackbarScroll);
         TimeSpan CurrentPeriod = CurrentDate - StartDate;
         double Fraction = 1.0 * CurrentPeriod.Days / Period.Days;
         TrackBar.Value = (int) (Fraction * 100);
         this.TrackBar.Scroll += new System.EventHandler(this.OnTrackbarScroll);

         // Increment (or decrement) the CurrentDate.
         if (Forward)
            CurrentDate += OneDay;
         else
            CurrentDate -= OneDay;
         if (CurrentDate < StartDate)
            {
            CurrentDate = StartDate;
            OnPauseClick(null, null);
            }
         if (CurrentDate > EndDate)
            {
            CurrentDate = EndDate;
            OnPauseClick(null, null);
            }
         panel1.BringToFront();
         }

      /// <summary>
      /// User has clicked pause.
      /// </summary>
      private void OnPauseClick(object sender, EventArgs e)
         {
         PlayButton.Checked = false;
         PauseButton.Checked = true;
         }

      /// <summary>
      /// User has clicked play.
      /// </summary>
      private void OnPlayClick(object sender, EventArgs e)
         {
         PlayButton.Checked = true;
         PauseButton.Checked = false;
         }

      /// <summary>
      /// User has clicked forward.
      /// </summary>
      private void OnForwardClick(object sender, EventArgs e)
         {
         Forward = true;
         OnPlayClick(null, null);
         }

      /// <summary>
      /// User has clicked rewind.
      /// </summary>
      private void OnRewindClick(object sender, EventArgs e)
         {
         Forward = false;
         OnPlayClick(null, null);
         }

      /// <summary>
      /// User has moved the trackbar.
      /// </summary>
      private void OnTrackbarScroll(object sender, EventArgs e)
         {
         double Fraction = TrackBar.Value / 100.0;
         TimeSpan NumDaysIntoPeriod = new TimeSpan((int)(Period.Days * Fraction), 0, 0, 0);
         CurrentDate = StartDate + NumDaysIntoPeriod;
         if (!PlayButton.Checked)
            UpdateReport(null, null);
         }
      }
   }
