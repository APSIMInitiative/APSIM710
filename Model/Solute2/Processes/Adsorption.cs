using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class for computing solute adsorption/fixation onto soil particles
/// </summary>
/// <remarks>
/// This class does not compute adsorption, various approaches describing the process are implemented in derived classes
/// This process has not been implemented yet 
/// </remarks>
public class SoluteAdsorption
{
	#region Parameters and Inputs

	/// <summary>
	/// The value of the limiting factor defined by user (a constant)
	/// </summary>
	[Param(IsOptional = true)]
	private double AdsorptionConstant = 1.0;

	/// <summary>
	/// The thickness of each soil layer (mm)
	/// </summary>
	[Input()]
	private double[] dlayer = null;

	#endregion

	/// <summary>
	/// Get the amount of solute adsorbed for each layer
	/// </summary>
	/// <returns>The solute's adsorbed amount</returns>
	public virtual double[] SoluteAdsorbed()
	{
		double[] Result = new double[dlayer.Length];
		return Result;
	}

	/// <summary>
	/// Get the amount of solute adsorbed for specific layer
	/// </summary>
	/// <param name="Layer">The layer which the adsorption is calculated for</param>
	/// <returns>The solute's adsorbed amount</returns>
	public virtual double SoluteAdsorbed(int Layer)
	{
		return AdsorptionConstant;
	}
}

#region Derived classes to haldle alternative adsorption approaches

#endregion
