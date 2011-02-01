using System;
using System.Collections.Generic;
using System.Text;
using System.Drawing.Printing;
using Controllers;
using System.Windows.Forms;
using CSGeneral;
using System.IO;
using System.Drawing;
using TMGDevelopment.Windows.Forms;

namespace Graph
   {

   public class GraphActions
      {

      public static void New(BaseController Controller)
         {
         if (Controller.FileSaveAfterPrompt())
            Controller.ApsimData.New("<folder name=\"Graphs\"/>");
         }

      public static void GraphWizardAction(BaseController Cntr)
         {
         GraphWizardForm F = new GraphWizardForm();
         F.Go(Cntr);
         F.ShowDialog();
         }

      }


   }