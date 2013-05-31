using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class for computing solute diffusion between soil layer due to differences in concentration
/// </summary>
/// <remarks>
/// This class does not compute diffusion, various approaches describing the process are implemented in derived classes
/// This process has not been implemented yet, but it will collect the diffusivity values
/// </remarks>
public class SoluteDiffusion
{
	#region Parameters and Inputs

	/// <summary>
	/// The solute's diffusivity in water, for each layer (mm2/day)
	/// </summary>
	private double[] DiffusivityCoefficient;
	[Param()]
	[Output()]
	[Units("mm^2/day")]
	[Description("The solute's diffusivity in water, for each layer")]
	public double[] MolecularDiffusivity
	{
		get { return DiffusivityCoefficient; }
		set { DiffusivityCoefficient = value; }
	}

	/// <summary>
	/// The thickness of each soil layer (mm)
	/// </summary>
	[Input()]
	private double[] dlayer = null;

	#endregion

	/// <summary>
	/// Performs the initial checks and setup
	/// </summary>
	[EventHandler()]
	public void OnInitialised()
	{
		// Check that the there are values for all layers, will ignore extra values.
		// We will do this by setting the size of the diffusivity array equal to the size of the soil,
		//  it will thus set to zero the diffusivity in any layer to which the value was not given
		if (DiffusivityCoefficient.Length < dlayer.Length)
			Console.WriteLine("  - Diffusivity values were not supplied for all layers, these will be assumed zero");
		else if (DiffusivityCoefficient.Length > dlayer.Length)
			Console.WriteLine("  - Diffusivity values were supplied in excess of number of layers, these will be ignored");

		Array.Resize(ref DiffusivityCoefficient, dlayer.Length);
	}

	/// <summary>
	/// Get the variation of solute content for each layer due to diffusion
	/// </summary>
	public virtual double[] deltaSoluteDiffused()
	{
		double[] SoluteMoved = new double[dlayer.Length];
		return SoluteMoved;
	}
}

#region Derived methods for computing the diffusion of solute

#endregion