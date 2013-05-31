using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class for computing the solute inhibition effect (on soil processes)
/// </summary>
/// <remarks>
/// This class does not compute inhibition, various approaches describing the process are implemented in derived classes
/// The outputs are in the derived classes as well
/// </remarks>
public class SoluteInhibition
{
	[Link()]
	InhibitionAction InhibitionAction;

	#region Initialisation

	///// <summary>
	///// Performs the initial checks and setup
	///// </summary>
	//[EventHandler()]
	//public virtual void OnInitialised()
	//{
	//    // Get the amount of inhibitor in each layer
	//    //InhibitorAmount = Inhibitor.Amount_kgha();

	//    // Initialise the arrays:
	//    InhibitorEffect = new double[dlayer.Length];

	//    // Let user know about this module
	//    Console.WriteLine();
	//    Console.WriteLine("    The solute " + "Inhibitor.Name" + " is an soil process inhibitor");
	//    Console.WriteLine("    Inhibition factor: " + "None");
	//    Console.WriteLine();
	//}

	#endregion

	#region Process calculations

	///// <summary>
	///// Set on the calculations for each time-step 
	///// </summary>
	//[EventHandler()]
	//public void OnPrepare()
	//{
	//    // Get the amount of inhibitor in each layer
	//    //InhibitorAmount = Inhibitor.Amount_kgha();

	//    // Initialise the arrays:
	//    InhibitorEffect = new double[dlayer.Length];

	//    if (InhibitorAmount.Sum() > 0.0)
	//    {
	//        // there is some inhibitor, compute the inhibition factor
	//        for (int Layer = 0; Layer < dlayer.Length; Layer++)
	//            if (InhibitionAction != null)
	//                InhibitorEffect[Layer] = InhibitionAction.FactorValue(InhibitorAmount[Layer]);
	//    }
	//}

	public double[] InhibitionEffect(double[] InhibitorAmount)
	{
		double[] Result = new double[InhibitorAmount.Length];
		for (int Layer = 0; Layer < InhibitorAmount.Length; Layer++)
			if (InhibitionAction != null)
				Result[Layer] = InhibitionAction.FactorValue(InhibitorAmount[Layer]);
		return Result;
	}

	#endregion
}
