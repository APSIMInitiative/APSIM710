using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class to compute the value of the limiting factor due to soil pH
/// </summary>
/// <remarks>
/// This class does not compute the factor, various approaches describing it are implemented in derived classes
/// </remarks>
public class pHLimitingFactor
{
	#region Parameters and Inputs

	/// <summary>
	/// The value of the limiting factor defined by user (a constant)
	/// </summary>
	[Param(IsOptional = true)]
	private double UserDefinedValue = 1.0;

	/// <summary>
	/// The current values of the soil pH
	/// </summary>
	public double[] SoilpH = null;
	[Input(IsOptional = true)]
	public double[] pH
	{ set { SoilpH = value; } }

	/// <summary>
	/// The thickness of each soil layer (mm)
	/// </summary>
	[Input()]
	public double[] dlayer = null;

	#endregion

	/// <summary>
	/// Initialisation - check whether pH has been supplied
	/// </summary>
	[EventHandler()]
	public void OnInitialised()
	{
		// if ph is not supplied, assume 6.0 (might need to externalise this)
		if (SoilpH == null)
		{
			SoilpH = new double[dlayer.Length];
			for (int Layer = 0; Layer < dlayer.Length; Layer++)
				SoilpH[Layer] = 6.0;
		}
	}

	/// <summary>
	/// The computed limiting factor value. No calculations here, only checks whether user input is within bounds
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The pH limiting factor (a value between 0.0 and 1.0)</returns>
	public virtual double FactorValue(int Layer)
	{
		return Math.Max(0.0, Math.Min(1.0, UserDefinedValue));
	}
}

#region Derived methods for soil pH factor

/// <summary>
/// Compute the value of the limiting factor due to soil moisture based on RCichota brocken stick function
/// </summary>
public class pHFactor_BrockenStickFunction : pHLimitingFactor
{
	#region Parameters

	private double[] xValues;
	/// <summary>
	/// The series of x-axis values of the function (here is soil pH)
	/// </summary>
	[Param()]
	private double[] phf_xValues
	{ set { xValues = value; } }

	private double[] yValues;
	/// <summary>
	/// The series of y-axis values of the function (here the limiting factor value)
	/// </summary>
	[Param()]
	private double[] phf_yValues
	{ set { yValues = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use linear interpolation, based on a series of data points 
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The pH limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		bool didInterpolate;
		double TheFactor = MathUtility.LinearInterpReal(SoilpH[Layer], xValues, yValues, out didInterpolate);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

#endregion

