using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using ModelFramework;
using CSGeneral;

/// <summary>
/// Base class for coontrolling the inhibition effect type
/// </summary>
/// <remarks>
/// This class does not output inhibition, outputs are implemented in derived classes
/// </remarks>
public class InhibitionEffectType
{

	public string Name = "None";

	/// <summary>
	/// The value of the inhibition factor (vary from 0.0 to 1.0)
	/// </summary>
	public double[] InhibitionEffect;
}

#region Derived inhibition effects

/// <summary>
/// Outputs the values of GenericEffect
/// </summary>
public class Effect_GenericEffect : InhibitionEffectType
{
	Effect_GenericEffect()
	{ Name = "GenericEffect"; }

	[Output]
	[Units("0-1")]
	[Description("Inhibition factor due to the presence of GenericInhibitor")]
	private double[] GenericEffect
	{ get { return InhibitionEffect; } }
}

/// <summary>
/// Outputs the values of nitrification_inhibition
/// </summary>
public class Effect_NitrificationInhibition : InhibitionEffectType
{
	void Effect_GenericEffect()
	{ Name = "nitrification_inhibition"; }

	[Output]
	[Units("0-1")]
	[Description("Inhibition factor due to the presence of nitrification inhibitor")]
	private double[] nitrification_inhibition
	{ get { return InhibitionEffect; } }

}

#endregion