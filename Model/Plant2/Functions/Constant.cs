using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;

[Description("returns the specified value")]
public class Constant : Function
   {
   [Param(Name = "Value") ] private string k = "0";
   [Output]
   public override double Value { get { return Convert.ToDouble(k); } }
   public override string ValueString { get { return k; } }

   #region This section contains a nasty cludge I have put in to enable overwriting of the expected yeield parameter in MCSP model
   private bool IsOverwritten = false;
    [EventHandler]
    public void OnSow(SowPlant2Type Sow)
    {
        if ((IsOverwritten == false) && (Sow.SkipPlant != 0) && (this.Name == "ExpectedYield"))
        {
            k = Convert.ToString(Sow.SkipPlant);
            IsOverwritten = true;
        }
    }
   #endregion
   }
