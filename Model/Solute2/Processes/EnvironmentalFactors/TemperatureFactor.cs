using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class to compute the value of the limiting factor due to soil temperature
/// </summary>
/// <remarks>
/// This class does not compute the factor, various approaches describing it are implemented in derived classes
/// </remarks>
public class TemperatureLimitingFactor
{
	#region Parameters and Inputs

	/// <summary>
	/// The value of the limiting factor defined by user (a constant)
	/// </summary>
	[Param(IsOptional = true)]
	private double UserDefinedValue = 1.0;

	/// <summary>
	/// The values of the soil temperature (for each layer) actually used by this method
	/// </summary>
	public double[] TSoil = null;

	/// <summary>
	/// The values of the soil temperature, for each layer, from SoilNitrogen module (to be phased out)
	/// </summary>
	[Input(IsOptional = true)]
	public double[] st
	{ set { TSoil = value; } }

	/// <summary>
	/// The values of the soil temperature, for each layer, from the SoilTemperature module
	/// </summary>
	[Input(IsOptional = true)]
	public double[] ave_soil_temp
	{ set { TSoil = value; } }

	#endregion

	/// <summary>
	/// Initialisation - check whether temperature has been supplied
	/// </summary>
	[EventHandler()]
	public void OnInitialised()
	{
		// check whether temperature has been supplied
		if (TSoil == null)
			throw new Exception("Soil temperature values are missing");
	}

	/// <summary>
	/// The computed limiting factor value. No calculations here, only checks whether user input is within bounds
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The temperature limiting factor (a value between 0.0 and 1.0)</returns>
	public virtual double FactorValue(int Layer)
	{
		return Math.Max(0.0, Math.Min(1.0, UserDefinedValue));
	}
}

#region Derived methods for temperature factor

/// <summary>
/// Compute the value of the limiting factor due to soil temperature based on RCichota brocken stick function
/// </summary>
public class TempFactor_BrockenStickFunction : TemperatureLimitingFactor
{
	#region Parameters

	private double[] xValues;
	/// <summary>
	/// The series of x-axis values of the function (here is temperature)
	/// </summary>
	[Param()]
	private double[] stf_xValues
	{ set { xValues = value; } }

	private double[] yValues;
	/// <summary>
	/// The series of y-axis values of the function (here the limiting factor value)
	/// </summary>
	[Param()]
	private double[] stf_yValues
	{ set { yValues = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use linear interpolation, based on a series of data points 
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The temperature limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		bool didInterpolate;
		double TheFactor = MathUtility.LinearInterpReal(TSoil[Layer], xValues, yValues, out didInterpolate);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

/// <summary>
/// Compute the value of the limiting factor due to soil temperature based on RCichota bending stick function
/// </summary>
public class TempFactor_BendingStickFunction : TemperatureLimitingFactor
{
	#region Parameters

	private double TemperatureAtOptimum;
	/// <summary>
	///  The temperature at optimum conditions
	///  At this value and above the limiting factor is equal to one
	/// </summary>
	[Param()]
	private double stf_TemperatureAtOptimum
	{ set { TemperatureAtOptimum = value; } }

	private double FactorValueAtZeroDegrees;
	/// <summary>
	/// The value of the limiting factor when temperature reaches zero degrees
	/// </summary>
	[Param()]
	private double stf_FactorValueAtZeroDegrees
	{ set { FactorValueAtZeroDegrees = value; } }

	private double ExponentValue;
	/// <summary>
	/// The exponent value which defines the function curvature
	/// </summary>
	[Param()]
	private double stf_ExponentValue
	{ set { ExponentValue = value; } }

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations are based  on the bending function proposed by RCichota
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The temperature limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		//double Tmin = FactorValueAtZeroDegrees * (Math.Pow(TemperatureAtOptimum, 1 / ExponentValue)) /
		//    (Math.Pow(TemperatureAtOptimum, 1 / ExponentValue) - 1);
		double Tmin = TemperatureAtOptimum * (Math.Pow(FactorValueAtZeroDegrees, 1 / ExponentValue)) /
			(Math.Pow(FactorValueAtZeroDegrees, 1 / ExponentValue) - 1);
		double LineSlope = 1 / (TemperatureAtOptimum - Tmin);
		return Math.Min(1.0, Math.Pow(LineSlope * Math.Max(0.0, TSoil[Layer] - Tmin), ExponentValue));
	}
}

/// <summary>
/// Compute the value of the limiting factor due to soil temperature using an exponential function
/// </summary>
public class TempFactor_ExponentialFunction : TemperatureLimitingFactor
{
	#region Parameters

	/// <summary>
	/// The value of the limiting factor when temperature reaches zero degrees
	/// </summary>
	[Param()]
	private double FactorValueAtZeroDegrees;

	/// <summary>
	/// The value of the coefficient for the exponential function
	/// </summary>
	[Param()]
	private double ExponentialCoefficient;

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use an exponential function 
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The temperature limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		double TheFactor = FactorValueAtZeroDegrees * Math.Exp(TSoil[Layer] * ExponentialCoefficient);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

/// <summary>
/// Compute the value of the limiting factor due to soil temperature using the Q10 approach
/// </summary>
public class TempFactor_Q10Function : TemperatureLimitingFactor
{
	#region Parameters

	/// <summary>
	/// The value of the Q10 coefficient
	/// </summary>
	[Param()]
	private double Q10Coefficient;

	/// <summary>
	/// The temperature at optimum conditions
	/// </summary>
	[Param()]
	private double TemperatureAtOptimum;

	#endregion

	/// <summary>
	/// The computed limiting factor value. Calculations use a Q10 function 
	/// </summary>
	/// <param name="Layer">The layer which the factor is calculated for</param>
	/// <returns>The temperature limiting factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(int Layer)
	{
		double TempExp = (TSoil[Layer] - TemperatureAtOptimum) / 10;
		double TheFactor = Math.Pow(Q10Coefficient, TempExp);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

#endregion
