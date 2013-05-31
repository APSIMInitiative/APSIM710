using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class to compute the value of the limiting factor due to soil carbon content
/// </summary>
/// <remarks>
/// This class does not compute the factor, various approaches describing it are implemented in derived classes
/// </remarks>
public class CarbonLimitingFactor
{
	#region Parameters and Inputs

	/// <summary>
	/// The value of the limiting factor defined by user (a constant)
	/// </summary>
	[Param(IsOptional = true)]
	public double UserDefinedValue = 1.0;

	/// <summary>
	/// The current values of the soil carbon, total
	/// </summary>
	public double[] TotalSoilCarbon = null;
	[Input(IsOptional = true)]
	public double[] carbon_tot
	{ set { TotalSoilCarbon = value; } }

	/// <summary>
	/// The current values of the soil carbon, only inert
	/// </summary>
	public double[] InertSoilCarbon = null;
	[Input(IsOptional = true)]
	public double[] inert_c
	{ set { InertSoilCarbon = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. No calculations here, only checks whether user input is within bounds
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The available carbon limiting factor (a value between 0.0 and 1.0)</returns>
	public virtual double FactorValue(int Layer)
	{
		return Math.Max(0.0, Math.Min(1.0, UserDefinedValue));
	}

	/// <summary>
	/// Compute the dissolved, or active, carbon content
	/// </summary>
	/// <remarks>
	/// Not fully implemented yet.
	/// This could be an output from SoilNitrogen
	/// </remarks>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The amount of dissolved organic carbon</returns>
	public double DissolvedOrganicCarbon(int Layer)
	{
		double DOC_Parm01 = 1.0;
		double DOC_Parm02 = 1.0;
		double LabileCarbon = Math.Max(0.0, TotalSoilCarbon[Layer] - InertSoilCarbon[Layer]);
		return DOC_Parm01 * Math.Pow(LabileCarbon, DOC_Parm02);
	}
}

#region Derived methods for carbon content factor

/// <summary>
/// Compute the value of the limiting factor due to soil carbon based on RCichota brocken stick function
/// </summary>
public class CarbonFactor_BrockenStickFunction : CarbonLimitingFactor
{
	#region Parameters

	private double[] xValues;
	/// <summary>
	/// The series of x-axis values of the function (here is dissolved organic carbon)
	/// </summary>
	[Param()]
	private double[] scf_xValues
	{ set { xValues = value; } }

	private double[] yValues;
	/// <summary>
	/// The series of y-axis values of the function (here the limiting factor value)
	/// </summary>
	[Param()]
	private double[] scf_yValues
	{ set { yValues = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use linear interpolation, based on a series of data points 
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The available carbon limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		bool didInterpolate;
		double DOC = DissolvedOrganicCarbon(Layer);
		double TheFactor = MathUtility.LinearInterpReal(DOC, xValues, yValues, out didInterpolate);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

/// <summary>
/// Compute the value of the limiting factor due to soil carbon using the Michaelis-Menten equation
/// </summary>
public class CarbonFactor_MichaelisMentenFunction : CarbonLimitingFactor
{
	#region Parameters

	private double k_MichaelisMenten;
	/// <summary>
	/// The Michaelis-Menten coefficient for carbon effect
	/// </summary>
	[Param()]
	private double scf_kMichaelisMenten
	{ set { k_MichaelisMenten = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use the Michaelis-Menten equation
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The available carbon limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		double DOC = DissolvedOrganicCarbon(Layer);
		double TheFactor = DOC / (k_MichaelisMenten + DOC);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

#endregion
