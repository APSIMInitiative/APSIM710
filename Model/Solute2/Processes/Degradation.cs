using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class for computing solute transformation, or degradation
/// </summary>
/// <remarks>
/// This class does not compute degradation, various approaches describing the process are implemented in derived classes
/// </remarks>
public class SoluteDegradation
{
	#region Parameters and Inputs

	/// <summary>
	/// Potential degradation of solute at optimum conditions (%/day)
	/// </summary>
	[Param(MinVal = 0.0, MaxVal = 100.0)]
	private double PotentialDegradationRate;

	/// <summary>
	/// The thickness of each soil layer (mm)
	/// </summary>
	[Input()]
	private double[] dlayer = null;

	#endregion

	#region Links to factor functions

	[Link(IsOptional=true)]
	public TemperatureLimitingFactor TemperatureLimitingFactor;
	[Link(IsOptional = true)]
	public MoistureLimitingFactor MoistureLimitingFactor;
	[Link(IsOptional = true)]
	public pHLimitingFactor pHLimitingFactor;
	[Link(IsOptional = true)]
	public CarbonLimitingFactor CarbonLimitingFactor;

	#endregion

	/// <summary>
	/// Gets the fraction of solute transformed in this time step for each layer (0-1)
	/// </summary>
	/// <returns>Fraction of solute transformed (a value between 0 and 1)</returns>
	public virtual double[] fractionDegradation()
	{
		// default values for limiting factor is one (no effect)
		double TempFactor = 1.0;
		double MoistFactor = 1.0;
		double pHFactor = 1.0;
		double CarbonFactor = 1.0;

		double[] Degradation = new double[dlayer.Length];
		for (int Layer = 0; Layer < dlayer.Length; Layer++)
		{
			// Attempt to get new values for the factors
			if (TemperatureLimitingFactor != null)
				TempFactor = TemperatureLimitingFactor.FactorValue(Layer);
			if (MoistureLimitingFactor != null)
				MoistFactor = MoistureLimitingFactor.FactorValue(Layer);
			if (pHLimitingFactor != null)
				pHFactor = pHLimitingFactor.FactorValue(Layer);
			if (CarbonLimitingFactor != null)
				CarbonFactor = CarbonLimitingFactor.FactorValue(Layer);

			Degradation[Layer] = PotentialDegradationRate * 0.01 * TempFactor * MoistFactor * pHFactor * CarbonFactor;
		}
		return Degradation;
	}

	/// <summary>
	/// Gets the fraction of solute transformed in this time step for especific layer (0-1)
	/// </summary>
	/// <param name="Layer">The layer which the transformation is calculated for</param>
	/// <returns>Fraction of solute transformed (a value between 0 and 1)</returns>
	public virtual double fractionDegradation(int Layer)
	{
		// default values for limiting factor is one (no effect)
		double TempFactor = 1.0;
		double MoistFactor = 1.0;
		double pHFactor = 1.0;
		double CarbonFactor = 1.0;

		// Attempt to get new values for the factors
		if (TemperatureLimitingFactor != null)
			TempFactor = TemperatureLimitingFactor.FactorValue(Layer);
		if (MoistureLimitingFactor != null)
			MoistFactor = MoistureLimitingFactor.FactorValue(Layer);
		if (pHLimitingFactor != null)
			pHFactor = pHLimitingFactor.FactorValue(Layer);
		if (CarbonLimitingFactor != null)
			CarbonFactor = CarbonLimitingFactor.FactorValue(Layer);

		double Degradation = PotentialDegradationRate * 0.01 * TempFactor * MoistFactor * pHFactor * CarbonFactor;
		return Degradation;
	}
}

#region Output the product of degradation

///// <summary>
///// Base class for outputing the product of solute degradation
///// </summary>
///// /// <remarks>
///// This class does not output values, various outputs are implemented in derived classes
///// </remarks>
//public class DegradationProduct
//{ 
//    /// <summary>
//    /// Amount of solute (product) resulting from degradation
//    /// </summary>
//    public double[] deltaProductOfDegradation;
//    [Output()]
//    [Units("kg/ha")]
//    [Description("amount of product derived from solute degradation")]
//    private double[] deltaProductOfSoluteDegradation
//    { get { return deltaProductOfDegradation; } }
//}

//#region Derived methods

///// <summary>
///// Outputs the values of the product of solute degradation: GenericProduct
///// </summary>
//public class Product_GenericProduct : DegradationProduct
//{
//    [Output()]
//    [Units("kg/ha")]
//    [Description("amount of GenericProduct derived from degradation of solute")]
//    private double[] GenericProduct
//    { get { return deltaProductOfDegradation; } }

//    /// <summary>
//    /// Event used to send changes in solute amount to APSIM
//    /// </summary>
//    [Event]
//    public event SoluteChangedDelegate SoluteChanged;
	
//    /// <summary>
//    /// Publish the SoluteChanged event with the values of the degradation product
//    /// </summary>
//    [EventHandler()]
//    public void OnProcess()
//    {
//        SoluteChangedType SoluteData = new SoluteChangedType();
//        SoluteData.Sender = this.GetType().Name;
//        SoluteData.SoluteName = "GenericProduct";
//        SoluteData.SoluteUnits = "kg/ha";
//        SoluteData.DeltaSolute = deltaProductOfDegradation;
//        SoluteChanged.Invoke(SoluteData);
//    }
//}

//#endregion

#endregion