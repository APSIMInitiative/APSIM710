using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class to describe a solute
/// </summary>
public partial class Solute
{
	[Link]
	Component My = null;

	#region Parameters needed for initialisation

	/// <summary>
	/// The molecular weight of the solute (g/mol)
	/// </summary>
	[Param()]
	[Output()]
	[Units("g/mol")]
	[Description("The molecular weight of the solute")]
	public double MolecularWeight
	{ get; set; }

	private double[] MyInitialContent;
	/// <summary>
	/// The initial content of the solute (mg solute/kg soil)
	/// </summary>
	[Param()]
	[Output()]
	[Units("mg/kg")]
	[Description("The initial solute content")]
	private double[] InitialContent
	{
		get { return MyInitialContent; }
		set { MyInitialContent = value; }
	}

	/// <summary>
	/// The upper bound for the solute amount (mol/kg soil)
	/// </summary>
	[Param()]
	[Units("mol/kg")]
	private double MaximumConcentration
	{ get; set; }
	
	/// <summary>
	/// The lower bound for the solute amount (mol/kg soil)
	/// </summary>
	[Param()]
	[Units("mol/kg")]
	private double MinimumConcentration
	{ get; set; }

	/// <summary>
	/// The tolerance level for values above or below the bounds
	/// </summary>
	[Param(IsOptional = true)]
	[Units("")]
	private double ToleranceValue = 0.00001;

	#endregion

	#region Inputs from ASPIM

	/// <summary>
	/// The thickness of each soil layer (mm)
	/// </summary>
	[Input()]
	private double[] dlayer = null;

	/// <summary>
	/// The soil bulk density (kg/L = g/cm3)
	/// </summary>
	[Input()]
	private double[] bd = null;

	#endregion

	#region Links to various processes that the solute might have

	/// <summary>
	/// Initialises the class that contains the solute's basic outputs, which vary according to its type
	/// </summary>
	[Link()]
	public SoluteType SoluteType;

	/// <summary>
	/// Switch to the class describing the solute adsortion onto soil particles
	/// </summary>
	[Link(IsOptional = true)]
	SoluteAdsorption SoluteAdsorption;

	/// <summary>
	/// Switch to the class describing the solute difussion in soil due to differences in concentration
	/// </summary>
	[Link(IsOptional = true)]
	SoluteDiffusion SoluteDiffusion;

	/// <summary>
	/// Switch to the class describing the solute degradation (tranformation into another substance: product)
	/// </summary>
	[Link(IsOptional = true)]
	SoluteDegradation SoluteDegradation;

	/// <summary>
	/// Swith to the class describing the inhibition effect the solute has on soil processes
	/// </summary>
	[Link(IsOptional = true)]
	SoluteInhibition SoluteInhibition;

	#endregion

	#region Event raised by this module

	/// <summary>
	/// Event used to advertise this solute to the APSIM world
	/// </summary>
	[Event]
	public event NewSoluteDelegate NewSolute;

    /// <summary>
    /// Event used to send mass flows to sysbal
    /// </summary>
    [Event]
    public event ExternalMassFlowDelegate ExternalMassFlowSolute;

	#endregion

	#region Internal variables

	/// <summary>
	/// The amount of the solute for each layer (mol/kg)
	/// </summary>
	private double[] Amount;

	#endregion

	#region Procedures and event handlers

	/// <summary>
	/// Performs the initial checks and setup
	/// </summary>
	[EventHandler()]
	public void OnInitialised()
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

		// Initialise SoluteType - Solute name and outputs
		SoluteType.InitialiseSoluteType(this);

		// Let other modules know about this solute
		AdvertiseThisSolute();

		// Let user know of this solute
		WriteSummary();
	}

	/// <summary>
	/// Verify whether the array with the amount of solute has been initialised. If not, do so.
	/// </summary>
	private void CheckInitialAmount()
	{
		if (Amount == null)
		{
			// initialise the array
			Amount = new double[dlayer.Length];

			// verify that initial content is given to all layers, if not will assume they are zero
			if (MyInitialContent.Length < dlayer.Length)
				Console.WriteLine("  - Values for solute amount were not supplied for all layers, these will be assumed zero");
			else if (MyInitialContent.Length > dlayer.Length)
				Console.WriteLine("  - Values for solute amount were supplied in excess of number of layers, these will be ignored");

			Array.Resize(ref MyInitialContent, dlayer.Length);

			// Set initial solute content
            for (int Layer = 0; Layer < dlayer.Length; Layer++)
                Amount[Layer] = MyInitialContent[Layer] / (MolecularWeight * 1000);  // convert from ppm to mol/kg
		}
	}

	/// <summary>
	/// Write summary with the amounts of solute in each layer
	/// </summary>
	private void WriteSummary()
	{
		Console.WriteLine();
		Console.WriteLine("              Soil profile " + SoluteType.Name);
		Console.WriteLine(@"
          --------------------------
           Layer        Amount
                    (ppm)   (kg/ha)
          --------------------------");
		for (int Layer = 0; Layer < dlayer.Length; ++Layer)
		{
			Console.WriteLine("           {0,4:d1}     {1,5:F1}   {2,6:F2}",
			Layer + 1, Amount_mgkg(Layer), Amount_kgha(Layer));
		}
		Console.WriteLine("          --------------------------");
		Console.WriteLine("           Totals           {0,6:F2}", Amount_kgha().Sum());
		Console.WriteLine("          --------------------------");
		Console.WriteLine();
	}

	/// <summary>
	/// Receives the changes in the amount of solute in the soil
	/// </summary>
	/// <param name="SoluteChanges">Data about solute changes (name and amount)</param>
	[EventHandler()]
	public void OnSoluteChanged(SoluteChangedType SoluteChanges)
	{
		if (SoluteChanges.SoluteName == SoluteType.Name)
		{
			// The amount of this solute has changed, check values and make changes
			changeSoluteAmount(SoluteChanges);
		}
	}

	/// <summary>
	/// Updates the the values for the amount of solute, following changes
	/// </summary>
	/// <remarks>
	/// The amount is stored in mol/kg, so the changes should be convereted approapriately
	/// </remarks>
	/// <param name="SoluteChanges">Data about solute changes (name and amount)</param>
	private void changeSoluteAmount(SoluteChangedType SoluteChanges)
	{
        double convFactor = 0.0;
		for (int Layer = 0; Layer < dlayer.Length; Layer++)
		{
			if (SoluteChanges.DeltaSolute[Layer] != 0.0)
			{
				// The amount has changed, check units and apply delta
				double DeltaAmount = 0.0;
				switch (SoluteChanges.SoluteUnits.ToLower())
				{
					case "kg/ha":
                        convFactor = 10 * MolecularWeight * dlayer[Layer] * bd[Layer];
						DeltaAmount = SoluteChanges.DeltaSolute[Layer] / convFactor;		// convert to mol/kg
						break;
					case "ppm":
					case "mg/kg":
                        convFactor = 1000 * MolecularWeight;
						DeltaAmount = SoluteChanges.DeltaSolute[Layer] / convFactor;	// convert to mol/kg
						break;
					case "mol/kg":
						DeltaAmount = SoluteChanges.DeltaSolute[Layer];		// already correct
						break;
					default:
						throw new Exception("The units for " + SoluteType.Name + " are not recognised, values passed by " + SoluteChanges.Sender);
				}
				double Result = Amount[Layer] + DeltaAmount;

				// Check bounds
				if (Result < MinimumConcentration - ToleranceValue)
					throw new Exception("Attempt to set the value of " + SoluteType.Name + "to a value below its lower bound");
				else if (Result > MaximumConcentration + ToleranceValue)
					throw new Exception("Attempt to set the value of " + SoluteType.Name + "to a value above its upper bound");
				else
					Amount[Layer] = Math.Max(MinimumConcentration, Math.Min(MaximumConcentration, Result));
			}
		}

		// Update values in SoluteType
        SoluteType.NewSoluteAmount(Amount_kgha(), Amount_mgkg());
	}

	/// <summary>
	/// Send information about this solute to the APSIM world
	/// </summary>
	private void AdvertiseThisSolute()
	{
		// Need to convert the name to an array
		string[] solute_names = new string[1];
		solute_names[0] = SoluteType.Name;

		NewSoluteType SoluteData = new NewSoluteType();
		SoluteData.solutes = solute_names;
		NewSolute.Invoke(SoluteData);
	}

	/// <summary>
	/// Calculations for each time-step 
	/// </summary>
	[EventHandler()]
	public void OnProcess()
	{
		// Check whether each of the process is simulated 
		//  case positive, get the values and pass on for outputing
		if (SoluteAdsorption != null)
			SoluteType.AdsorptionAmount(SoluteAdsorption.SoluteAdsorbed());
        if (SoluteDiffusion != null)
        {
            SoluteType.NewDiffusionAmount(SoluteDiffusion.deltaSoluteDiffused(Amount_kgha()));
            SoluteType.MakeSoluteDiffusionEffective();
        }
		if (SoluteDegradation != null)
		{
			double[] DegradationFraction = SoluteDegradation.fractionDegradation();
			double[] DegradationAmount = new double[dlayer.Length];
			for (int Layer = 0; Layer < dlayer.Length; Layer++)
				DegradationAmount[Layer] = -Amount_kgha(Layer) * DegradationFraction[Layer];
			SoluteType.NewDegradationFraction(DegradationFraction);
			SoluteType.NewDegradationAmount(DegradationAmount);

			// effectivate the solute changes due to degradation
			SoluteType.MakeSoluteDegradationEffective();
		}
		if (SoluteInhibition != null)
			SoluteType.NewInhibitionEffect(SoluteInhibition.InhibitionEffect(Amount_mgkg()));
	}

	#endregion

	#region Public methods

	#region Amounts in mol/kg

	/// <summary>
	/// Gets the amount of solute in the soil layers in mol/kg
	/// </summary>
	/// <returns>The amount of solute in each layer (mol/kg)</returns>
	public double[] Amount_molkg()
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

		return Amount;
	}

	/// <summary>
	/// Gets the amount of solute in the layer in mol/kg
	/// </summary>
	/// <param name="Layer">The layer from which amount is to be supplied</param>
	/// <returns>The amount of solute in the layer (mol/kg)</returns>
	public double Amount_molkg(int Layer)
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

		return Amount[Layer];
	}

	#endregion

	#region Amounts in kg/ha

	/// <summary>
	/// Gets the amount of solute in the soil layers in kg/ha
	/// </summary>
	/// <returns>The amount of solute in each layer (kg/ha)</returns>
	public double[] Amount_kgha()
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

		double[] Result = new double[dlayer.Length];
		double convFactor = 0.0;

		for (int Layer = 0; Layer < dlayer.Length; Layer++)
		{
            convFactor = 10 * MolecularWeight * dlayer[Layer] * bd[Layer];	// converts from mol/kg to kg/ha
			Result[Layer] = Amount[Layer] * convFactor;
		}
		return Result;
	}

	/// <summary>
	/// Gets the amount of solute in the layer in kg/ha
	/// </summary>
	/// <param name="Layer">The layer from which amount is to be supplied</param>
	/// <returns>The amount of solute in the layer (kg/ha)</returns>
	public double Amount_kgha(int Layer)
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

        double convFactor = 10.0 * MolecularWeight * bd[Layer] * dlayer[Layer];	// converts from mol/kg to kg/ha
		return Amount[Layer] * convFactor;
	}

	#endregion

	#region Amounts in mg/kg (ppm)

	/// <summary>
	/// Gets the amount of solute in the soil layers in mg/kg
	/// </summary>
	/// <returns>The amount of solute in each layer(mg/kg)</returns>
	public double[] Amount_mgkg()
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

		double[] Result = new double[dlayer.Length];
		double convFactor = 0.0;

		for (int Layer = 0; Layer < dlayer.Length; Layer++)
		{
			convFactor = 1000 * MolecularWeight;	// converts from mol/kg to mg/kg
			Result[Layer] = Amount[Layer] * convFactor;
		}
		return Result;
	}

	/// <summary>
	/// Gets the amount of solute in the layer in mg/kg
	/// </summary>
	/// <param name="Layer">The layer from which amount is to be supplied</param>
	/// <returns>The amount of solute in the layer (mg/kg)</returns>
	public double Amount_mgkg(int Layer)
	{
		// Check whether amount has been initialised
		CheckInitialAmount();

		double convFactor = 1000 * MolecularWeight;	// converts from mol/kg to mg/kg
		return Amount[Layer] * convFactor;
	}

	#endregion

	#endregion

}
