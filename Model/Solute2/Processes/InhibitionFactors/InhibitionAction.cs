using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class to compute the value of the inhibition factor
/// </summary>
/// <remarks>
/// This class does not compute the factor, various approaches describing it are implemented in derived classes
/// The factor is non specific, i.e. it could be used for inhibiting any soil process
/// (the name of the factor, and therefore the inhibited process, is given elsewhere)
/// </remarks>
public class InhibitionAction
{
	#region Parameters and Inputs

	double UserDefinedValue;
	/// <summary>
	/// The value of the inhibition factor defined by user (a constant)
	/// </summary>
	[Param(IsOptional = true)]
	private double sif_UserDefinedValue
	{ set { UserDefinedValue = value; } }

	#endregion

	/// <summary>
	/// Gets the inhibition factor. No calculations here, only checks whether user input is within bounds
	/// </summary>
	/// <param name="SoluteAmount">The amount of solute at the layer which the factor is calculated for</param>
	/// <returns>The inhibition factor (a value between 0.0 and 1.0)</returns>
	public virtual double FactorValue(double SoluteAmount)
	{
		return Math.Max(0.0, Math.Min(1.0, UserDefinedValue));
	}
}

#region Derived methods for computing the inhibitor action

/// <summary>
/// Compute the value of the inhibition factor using a straight line function
/// </summary>
public class InhibitionAction_LinearFunction : InhibitionAction
{
	#region Parameters

	/// <summary>
	/// The intercept of the linear relationship between solute content and inhibition
	/// </summary>
	[Param(IsOptional = true)]
	private double Intercept = 0.0;

	/// <summary>
	/// The slope of the linear relationship between solute content and inhibition
	/// </summary>
	[Param()]
	private double Slope;

	#endregion

	/// <summary>
	/// This method computes the inhibition factor using a linear relationship
	/// </summary>
	/// <param name="SoluteAmount">The solute amount in the layer for which the calculation is made</param>
	/// <returns>The inhibition factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(double SoluteAmount)
	{
		double TheFactor = Intercept + SoluteAmount * Slope;
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

/// <summary>
/// Class to compute the value of the inhibition factor based on RCichota brocken stick function
/// </summary>
public class InhibitionAction_BrockenStickFunction : InhibitionAction
{
	#region Parameters

	private double[] xValues;
	/// <summary>
	/// The series of x-axis values of the function (here is solute concentration)
	/// </summary>
	[Param()]
	private double[] sif_xValues
	{ set { xValues = value; } }

	private double[] yValues;
	/// <summary>
	/// The series of y-axis values of the function (here the limiting factor value)
	/// </summary>
	[Param()]
	private double[] sif_yValues
	{ set { yValues = value; } }

	#endregion

	/// <summary>
	/// This method computes the inhibition factor using linear interpolation, base on a series of data points 
	/// </summary>
	/// <param name="SoluteAmount">The solute amount in the layer for which the calculation is made</param>
	/// <returns>The inhibition factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(double SoluteAmount)
	{
		bool didInterpolate;
		double TheFactor = MathUtility.LinearInterpReal(SoluteAmount, xValues, yValues, out didInterpolate);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

/// <summary>
/// Class to compute the value of the inhibition factor based on a Michaelis-Menten equation
/// </summary>
public class InhibitionAction_MichaelisMentenFunction : InhibitionAction
{
	#region Parameters

	private double k_MichaelisMenten;
	/// <summary>
	/// The Michaelis-Menten coefficient for inhibition effect
	/// </summary>
	[Param()]
	private double sif_kMichaelisMenten
	{ set { k_MichaelisMenten = value; } }

	#endregion

	/// <summary>
	/// This method computes the inhibition factor using the Michaelis-Menten equation
	/// </summary>
	/// <param name="SoluteAmount">The solute amount in the layer for which the calculation is made</param>
	/// <returns>The inhibition factor (a value between 0.0 and 1.0)</returns>
	public override double FactorValue(double SoluteAmount)
	{
		double TheFactor = SoluteAmount / (k_MichaelisMenten + SoluteAmount);
		return Math.Max(0.0, Math.Min(1.0, TheFactor));
	}
}

#endregion
