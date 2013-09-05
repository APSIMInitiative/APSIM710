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
	private double DiffusivityCoefficient;
	[Param()]
	[Output()]
	[Units("mm^2/day")]
	[Description("The solute's diffusivity in water")]
	public double MolecularDiffusivity
	{
		get { return DiffusivityCoefficient; }
		set { DiffusivityCoefficient = value; }
	}

	/// <summary>
	/// The thickness of each soil layer (mm)
	/// </summary>
	[Input()]
    public double[] dlayer = null;

    /// <summary>
    /// The thickness of each soil layer (mm)
    /// </summary>
    [Input()]
    public double[] sat = null;

    /// <summary>
    /// The thickness of each soil layer (mm)
    /// </summary>
    [Input()]
    public double[] sw = null;

    /// <summary>
    /// The amount of solute moved via diffusion
    /// </summary>
    public double[] SoluteFlux;

	#endregion

	/// <summary>
	/// Performs the initial checks and setup
	/// </summary>
	[EventHandler()]
	public void OnInitialised()
	{
        // Initialise the internal variables
        SoluteFlux = new double[dlayer.Length];
    }

	/// <summary>
	/// Get the variation of solute content for each layer due to diffusion
	/// </summary>
	public virtual double[] deltaSoluteDiffused(double[] Amount)
	{
		double[] SoluteMoved = new double[dlayer.Length];
		return SoluteMoved;
	}
}

#region Derived methods for computing the diffusion of solute

public class SoluteDiffusion_MillingtonQuirk : SoluteDiffusion
{

    public override double[] deltaSoluteDiffused(double[] amount)
    {
        DoDiffusion(amount);
        return SoluteFlux;
    }


    public void DoDiffusion(double[] SoluteAmount)
    {

        double Tortuosity = 1.0;
        // Compute solute flux in the soil. Uses Fick's law
        for (int Layer = 0; Layer < dlayer.Length; Layer++)
        {
            // Get the values for tortuosity 
            Tortuosity = Math.Pow(sw[Layer] / sat[Layer], 2);

            // Compute the flux
            if (Layer < dlayer.Length - 1)
            {
                double avgThickness = (dlayer[Layer] + dlayer[Layer + 1]) / 2;
                double SoluteGradient = (SoluteAmount[Layer] - SoluteAmount[Layer + 1]) / avgThickness;
                SoluteFlux[Layer] = MolecularDiffusivity * Tortuosity * sw[Layer] * SoluteGradient;
            }
            else
                SoluteFlux[Layer] = 0.0;
        }
    }
}
#endregion