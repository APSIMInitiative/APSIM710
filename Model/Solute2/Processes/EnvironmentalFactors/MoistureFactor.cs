using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class to compute the value of the limiting factor due to soil moisture
/// </summary>
/// <remarks>
/// This class does not compute the factor, various approaches describing it are implemented in derived classes
/// </remarks>
public class MoistureLimitingFactor
{
	#region Parameters and Inputs

    double UserDefinedValue = 1.0;
    /// <summary>
	/// The value of the limiting factor defined by user (assumed constant)
	/// </summary>
	[Param(IsOptional = true)]
	public double swf_UserDefinedValue
    { set { UserDefinedValue = value; } }

	/// <summary>
	/// The current values of the soil moisture
	/// </summary>
	public double[] SoilWaterContent = null;
	[Input()]
	public double[] sw
	{ set { SoilWaterContent = value; } }

	/// <summary>
	/// The values of the soil moisture at saturation
	/// </summary>
	public double[] SWaterSaturation = null;
	[Input()]
	public double[] sat
	{ set { SWaterSaturation = value; } }

	/// <summary>
	/// The values of the soil moisture at field capacity, or drained upper limit
	/// </summary>
	public double[] SWaterFieldCapacity = null;
	[Input()]
	public double[] dul
	{ set { SWaterFieldCapacity = value; } }

	/// <summary>
	/// The values of the soil moisture at permanent wilting point, or plant available lower limit
	/// </summary>
	public double[] SWaterWiltingPoint = null;
	[Input()]
	public double[] ll15
	{ set { SWaterWiltingPoint = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. No calculations here, only checks whether user input is within bounds
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The moisture limiting factor (a value between 0.0 and 1.0)</returns>
	public virtual double FactorValue(int Layer)
	{
		return Math.Max(0.0, Math.Min(1.0, UserDefinedValue));
	}

	/// <summary>
	/// Compute the normalised value of soil moisture (assuming 0=dry; 1=LL15, 2=DUL, 3=SAT)
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The normalised soil moisture (a value between 0.0 and 3.0)</returns>
	public double NormaliseSoilMoisture(int Layer)
	{
		double[] xVals = { 0.0, SWaterWiltingPoint[Layer], SWaterFieldCapacity[Layer], SWaterSaturation[Layer] };
		double[] yVals = { 0.0, 1.0, 2.0, 3.0 };
		bool didInterpolate;
		return MathUtility.LinearInterpReal(SoilWaterContent[Layer], xVals, yVals, out didInterpolate);
	}
}

#region Derived methods for moisture factor

/// <summary>
/// Compute the value of the limiting factor due to soil moisture based on RCichota Broken stick function
/// </summary>
/// <remarks>
/// It is assumed that the moisture is nomalised: 0=dry; 1=LL15, 2=DUL, 3=SAT
/// </remarks>
public class MoistFactor_BrokenStickFunction : MoistureLimitingFactor
{
	#region Parameters

	private double[] xValues;
	/// <summary>
	/// The series of x-axis values of the function (here they are the normalised moisture)
	/// </summary>
	[Param()]
	private double[] swf_xValues
	{ set { xValues = value; } }

	private double[] yValues;
	/// <summary>
	/// The series of y-axis values of the function (the limiting factor values corresponding to given moisture)
	/// </summary>
	[Param()]
	private double[] swf_yValues
	{ set { yValues = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use linear interpolation, based on a series of data points 
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The moisture limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		bool didInterpolate;
		double NormalisedMoisture = NormaliseSoilMoisture(Layer);
		double TheFactor = MathUtility.LinearInterpReal(NormalisedMoisture, xValues, yValues, out didInterpolate);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

#endregion
