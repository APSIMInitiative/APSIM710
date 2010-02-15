using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using ApsimFile;

namespace ConToApsim
   {
   public partial class Form1 : Form
      {

      /// <summary>
      /// Constructor
      /// </summary>
      public Form1()
         {
         InitializeComponent();
         }


      public void SetText(string St)
         {
         string[] Lines = St.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
         foreach (string Line in Lines)
            ListBox.Items.Add(Line);
         }
      }
   }
