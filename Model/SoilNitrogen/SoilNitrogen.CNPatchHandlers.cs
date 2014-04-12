using System;
using System.Reflection;
using System.Collections.Generic;
using System.Xml.Serialization;
using System.Xml;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;

/// <remarks>
/// This partial class contains the various methods to handle patches
/// </remarks>
public partial class SoilNitrogen
{
	/// <summary>
	/// Handles the addition of new CNPatches
	/// </summary>
	/// <param name="PatchtoAdd">Patch data</param>
	private void AddNewCNPatch(AddSoilCNPatchType PatchtoAdd)
	{
		// Data passed from OnAddSoilCNPatch event:
		//.Sender: the name of the module that raised this event
		//.DepositionType: the type of deposition:
		//  - ToAllPaddock: No patch is created, add stuff as given to all patches. It is the default;
		//  - ToSpecificPatch: No patch is created, add stuff to given patches;
		//		(recipient patch is given using its index or name; if not supplied, defaults to homogeneous)
		//  - ToNewPatch: create new patch based on an existing patch, add stuff to created patch;
		//		- recipient or base patch is given using index or name; if not supplied, new patch will be based on the base/Patch[0];
		//      - patches are only created is area is larger than a minimum (minPatchArea);
		//      - new areas are proportional to existing patches;
		//  - NewOverlappingPatches: create new patch(es), these overlap with all existing patches, add stuff to created patches;
		//		(new patches are created only if their area is larger than a minimum (minPatchArea))
		//.AffectedPatches_id (AffectedPatchesByIndex): the index of the existing patches to which urine will be added
		//.AffectedPatches_nm (AffectedPatchesByName): the name of the existing patches to which urine will be added
		//.AreaFraction: the relative area of the patch (0-1)
		//.PatchName: the name(s) of the patch(es) being created
		//.Urea: amount of urea to add per layer (kgN/ha)
		//.NH4: amount of ammonium to add per layer (kgN/ha)
		//.NO3: amount of nitrate to add per layer (kgN/ha)
		//.FOM_C: amount of carbon in fom (all pools) to add per layer (kgC/ha)  - if present, the entry for pools will be ignored
		//.FOM_C_pool1: amount of carbon in fom_pool1 to add per layer (kgC/ha)
		//.FOM_C_pool2: amount of carbon in fom_pool2 to add per layer (kgC/ha)
		//.FOM_C_pool3: amount of carbon in fom_pool3 to add per layer (kgC/ha)
		//.FOM_N.: amount of nitrogen in fom to add per layer (kgN/ha)


		List<int> idPatchesJustAdded = new List<int>();	// list of IDs of patches created (exclude patches that would be too small)
		List<int> idPatchesToDelete = new List<int>();	//list of IDs of existing patches that became too small and need to be deleted
		List<int> idPatchesAffected;					//list of IDs of patches affected by new addition



		// 1. get the list of id's of patches which are affected by this addition, and the area affected
		double AreaAffected = 0;
		if (PatchtoAdd.DepositionType.ToLower() == "ToNewPatch".ToLower())
		{  // check which patches are affected
			idPatchesAffected = CheckPatchIDs(PatchtoAdd.AffectedPatches_id, PatchtoAdd.AffectedPatches_nm);
			for (int i = 0; i < idPatchesAffected.Count; i++)
				AreaAffected += Patch[idPatchesAffected[i]].RelativeArea;
		}
		else if (PatchtoAdd.DepositionType.ToLower() == "NewOverlappingPatches".ToLower())
		{  // all patches are affected
			idPatchesAffected = new List<int>();
			for (int k = 0; k < Patch.Count; k++)
				idPatchesAffected.Add(k);
			AreaAffected = 1.0;
		}
		else
		{
			idPatchesAffected = new List<int>();
		}

		// check that total area of affected patches is larger than new patch area
		if (AreaAffected < PatchtoAdd.AreaFraction)
		{
			// Existing area is smaller than new patch area, cannot continue
			writeMessage(" AddSoilCNPatch - area of selected patches (" + AreaAffected.ToString("#0.00#")
							   + ") is smaller than area of new patch(" + PatchtoAdd.AreaFraction.ToString("#0.00#") + "). Command will be ignored");
		}
		else
		{  // check the area for each patch
			for (int i = 0; i < idPatchesAffected.Count; i++)
			{
				double OldPatch_OldArea = Patch[idPatchesAffected[i]].RelativeArea;
				double NewPatch_NewArea = PatchtoAdd.AreaFraction * (OldPatch_OldArea / AreaAffected);
				double OldPatch_NewArea = OldPatch_OldArea - NewPatch_NewArea;
				if (NewPatch_NewArea < MinimumPatchArea)
				{  // area to create is too small, patch will not be created
					Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year="
						+ Clock.Today.DayOfYear.ToString() + "), SoilNitrogen.AddCNPatch:");
					Console.WriteLine("   attempt to create a new patch with area too small or negative ("
						+ NewPatch_NewArea.ToString("#0.00#") + "). The patch will not be created.");
				}
				else if (OldPatch_NewArea < MinimumPatchArea)
				{  // remaining area is too small or negative, patch will be created but old one will be deleted
					Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year="
						+ Clock.Today.DayOfYear.ToString() + "), SoilNitrogen.AddCNPatch:");
					Console.WriteLine(" attempt to set the area of existing patch(" + idPatchesAffected[i].ToString()
						+ ") to a value too small or negative (" + OldPatch_NewArea.ToString("#0.00#")
						+ "). The patch will be eliminated.");

					// mark old patch for deletion
					idPatchesToDelete.Add(idPatchesAffected[i]);

					// create new patch based on old one - the original one will be deleted later
					ClonePatch(idPatchesAffected[i]);
					int k = Patch.Count - 1;
					if (PatchtoAdd.AreaFraction > 0)
					{  // a name was supplied
						Patch[k].PatchName = PatchtoAdd.AreaFraction + "_" + i.ToString();
					}
					else
					{  // use default naming
						Patch[k].PatchName = "Patch" + k.ToString();
					}
					idPatchesJustAdded.Add(k);
				}
				else
				{
					// create new patch by spliting an existing one
					ClonePatch(idPatchesAffected[i]);
					Patch[idPatchesAffected[i]].RelativeArea = OldPatch_NewArea;
					int k = Patch.Count - 1;
					Patch[k].RelativeArea = NewPatch_NewArea;
					if (PatchtoAdd.PatchName.Length > 0)
					{  // a name was supplied
						Patch[k].PatchName = PatchtoAdd.AreaFraction + "_" + i.ToString();
					}
					else
					{  // use default naming
						Patch[k].PatchName = "Patch" + k.ToString();
					}
					idPatchesJustAdded.Add(k);
					Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year=" + Clock.Today.DayOfYear.ToString()
						+ "), SoilNitrogen.AddPatch:");
					Console.WriteLine(" create new patch, with area = " + NewPatch_NewArea.ToString("#0.00#") + ", based on existing patch("
						+ idPatchesAffected[i].ToString() + ") - Old area = " + OldPatch_OldArea.ToString("#0.00#") + ", new area = "
						+ OldPatch_NewArea.ToString("#0.00#"));
				}
			}
		}

		// add the stuff to patches just created
		AddStuffToPatches(idPatchesJustAdded, PatchtoAdd);

		// delete the patches in excess
		if (idPatchesToDelete.Count > 0)
			DeletePatches(idPatchesToDelete);

	}

	/// <summary>
	/// Clone an existing patch. That is, creates a new patch (k) based on an existing one (j)
	/// </summary>
	/// <param name="k">id of patch to be cloned</param>
	private void ClonePatch(int j)
	{
		// create new patch
		soilCNPatch newPatch = new soilCNPatch(this);
		Patch.Add(newPatch);
		int k = Patch.Count - 1;

		// set the size of arrays
		Patch[k].ResizeLayeredVariables(dlayer.Length);

		// copy the state variables from original patch in to the new one
		CopyValuesFromPatch(k, j);
	}

	/// <summary>	
	/// Controls the merging of a list of patches into a single one
	/// </summary>
	/// <param name="PatchesToMerge">List of patches to merge</param>
	private void AmalgamatePatches(List<int> PatchesToMerge)
	{
		while (PatchesToMerge.Count > 1)
		{
			//MergePatches(PatchesToMerge[0], PatchesToMerge[1]); // merge patch_1 into patch_0
			double mFactor = Patch[1].RelativeArea/Patch[0].RelativeArea;
			CopyValuesFromPatch(0, 1, mFactor);					// copy values from patch_1 into patch_0
			PatchesToMerge.RemoveAt(1);                         // delete reference to patch_1
		}
	}

	/// <summary>
	/// Delete patches in the list
	/// </summary>
	/// <param name="PatchesToDelete">List of patches to delete</param>
	private void DeletePatches(List<int> PatchesToDelete)
	{
		// go backwards so that the id of patches to delete do not change after each deletion
		for (int i = PatchesToDelete.Count; i >= 0; i--)
		{
			Patch.RemoveAt(PatchesToDelete[i]);
		}
	}

	/// <summary>
	/// Copy the state variables from one patch (j) to another one (k), using a multiplying factor
	/// </summary>
	/// <param name="k">The id of patch where values are copied to</param>
	/// <param name="j">The id of patch where values are copied from</param>
	/// <param name="MultiplyingFactor">A multiplying factor (optional)</param>
	private void CopyValuesFromPatch(int k, int j, double MultiplyingFactor = 1.0)
	{
		for (int layer = 0; layer < dlayer.Length; layer++)
		{
			// Mineral N
			Patch[k].urea[layer] += Patch[j].urea[layer] * MultiplyingFactor;
			Patch[k].nh4[layer] += Patch[j].nh4[layer] * MultiplyingFactor;
			Patch[k].no3[layer] += Patch[j].no3[layer] * MultiplyingFactor;
			Patch[k].TodaysInitialNH4[layer] += Patch[j].TodaysInitialNH4[layer] * MultiplyingFactor;
			Patch[k].TodaysInitialNO3[layer] += Patch[j].TodaysInitialNO3[layer] * MultiplyingFactor;

			// Organic C and N
			for (int pool = 0; pool < 3; pool++)
			{
				Patch[k].fom_c[pool][layer] += Patch[j].fom_c[pool][layer] * MultiplyingFactor;
				Patch[k].fom_n[pool][layer] += Patch[j].fom_n[pool][layer] * MultiplyingFactor;
			}
			Patch[k].biom_c[layer] += Patch[j].biom_c[layer] * MultiplyingFactor;
			Patch[k].biom_n[layer] += Patch[j].biom_n[layer] * MultiplyingFactor;
			Patch[k].hum_c[layer] += Patch[j].hum_c[layer] * MultiplyingFactor;
			Patch[k].hum_n[layer] += Patch[j].hum_n[layer] * MultiplyingFactor;
			Patch[k].inert_c[layer] += Patch[j].inert_c[layer] * MultiplyingFactor;
			Patch[k].inert_n[layer] += Patch[j].inert_n[layer] * MultiplyingFactor;
		}
	}

	/// <summary>
	/// Checks whether two patches can be considered equal
	/// </summary>
	/// <param name="k">Patch used as reference</param>
	/// <param name="j">Patch being compared to reference</param>
	/// <returns>TRUE if patches are similar enough, FALSE otherwise</returns>
	private bool PatchesAreEqual(int k, int j)
	{
		bool Result = false;
		// go through a series of criteria to evaluate whether the two patches can be considered equal
		if ((Math.Abs(Patch[k].carbon_tot[0] - Patch[j].carbon_tot[0]) < epsilon) &&
			(Math.Abs(Patch[k].nit_tot[0] - Patch[k].nit_tot[0]) < epsilon) &&
			(Math.Abs(Patch[k].biom_c[0] - Patch[k].biom_c[0]) < epsilon) &&
			(Math.Abs(Patch[k].fom_n[0][0] - Patch[k].fom_n[0][0]) < epsilon) &&
			(Math.Abs(Patch[k].fom_n[1][0] - Patch[k].fom_n[1][0]) < epsilon) &&
			(Math.Abs(Patch[k].no3[0] - Patch[k].no3[0]) < epsilon))
		{
			Result = true;
		}

		return Result;
	}

	/// <summary>
	/// Controls the addition of several variables to the especified patches
	/// </summary>
	/// <param name="PatchesToAdd">The list of patches to which the stuff will be added</param>
	/// <param name="StuffToAdd">The values of the variables to add (supplied as deltas)</param>
	private void AddStuffToPatches(List<int> PatchesToAdd, AddSoilCNPatchType StuffToAdd)
	{
		// Data passed from OnAddSoilCNPatch event - these are all considered deltas:
		//.Urea: amount of urea to add per layer (kgN/ha)
		//.Urea: amount of urea to add (per layer) - Do we need other N forms?
		//.NH4: amount of ammonium to add per layer (kgN/ha)
		//.NO3: amount of nitrate to add per layer (kgN/ha)
		//.FOM_C: amount of carbon in fom (all pools) to add per layer (kgC/ha)  -  If present, the pools will be ignored
		//.FOM_C_Pool1: amount of carbon in fom_pool1 to add per layer (kgC/ha)
		//.FOM_C_Pool2: amount of carbon in fom_pool2 to add per layer (kgC/ha)
		//.FOM_C_Pool3: amount of carbon in fom_pool3 to add per layer (kgC/ha)
		//.FOM_N: amount of nitrogen in fom to add per layer (kgN/ha)

		for (int i = PatchesToAdd.Count - 1; i >= 0; i--)
		{
			if ((StuffToAdd.Urea != null) && SumDoubleArray(StuffToAdd.Urea) > epsilon)
				Patch[PatchesToAdd[i]].dlt_urea = StuffToAdd.Urea;
			if ((StuffToAdd.NH4 != null) && SumDoubleArray(StuffToAdd.NH4) > epsilon)
				Patch[PatchesToAdd[i]].dlt_nh4 = StuffToAdd.NH4;
			if ((StuffToAdd.NO3 != null) && SumDoubleArray(StuffToAdd.NO3) > epsilon)
				Patch[PatchesToAdd[i]].dlt_no3 = StuffToAdd.NO3;
			//if ((StuffToAdd.FOM_C != null) && SumDoubleArray(StuffToAdd.FOM_C) > epsilon)
			//{
			//    Patch[PatchesToAdd[i]].dlt_fom_c[0] = StuffToAdd.FOM_C;
			//    Patch[PatchesToAdd[i]].dlt_fom_c[1] = StuffToAdd.FOM_C;
			//    Patch[PatchesToAdd[i]].dlt_fom_c[2] = StuffToAdd.FOM_C;
			//}
			//else
			//{
			//    if ((StuffToAdd.FOM_C != null) && (StuffToAdd.FOM_C_pool1.Sum() > epsilon))
			//        Patch[PatchesToAdd[i]].dlt_fom_c[0] = StuffToAdd.FOM_C_pool1;
			//    if ((StuffToAdd.FOM_C != null) && (StuffToAdd.FOM_C_pool2.Sum() > epsilon))
			//        Patch[PatchesToAdd[i]].dlt_fom_c[1] = StuffToAdd.FOM_C_pool2;
			//    if ((StuffToAdd.FOM_C != null) && (StuffToAdd.FOM_C_pool3.Sum() > epsilon))
			//        Patch[PatchesToAdd[i]].dlt_fom_c[2] = StuffToAdd.FOM_C_pool3;
			//}

			//if ((StuffToAdd.FOM_N != null) && (StuffToAdd.FOM_N.Sum() > epsilon))
				//Patch[PatchesToAdd[i]].dlt_fom_n = StuffToAdd.FOM_N;
		}

	}

	/// <summary>
	/// Check the list of patch names and IDs passed by 'AddSoilCNPatch' event
	/// </summary>
	/// <remarks>
	/// Tasks performed by this method:
	///  - Verify whether there are replicates in the list given
	///  - Verify whether the IDs and/or names given correspond to existing patches
	///  - Eliminate replicates and consolidate lists of IDs and names (merge both)
	/// </remarks>
	/// <param name="IDsToCheck">List of IDs or indices of patches</param>
	/// <param name="NamesToCheck">List of names of patches</param>
	/// <returns>List of patch IDs (negative if no ID is found)</returns>
	private List<int> CheckPatchIDs(int[] IDsToCheck, string[] NamesToCheck)
	{
		// List of patch IDs for output
		List<int> SelectedIDs = new List<int>();

		// 1. Check names
		if (NamesToCheck.Length > 0)
		{  // at least one name was given, check for existence and get ID
			for (int i_name = 0; i_name < NamesToCheck.Length; i_name++)
			{
				bool isReplicate = false;
				if (i_name > 0)
				{
					// check for replicates
					for (int i = 0; i < i_name; i++)
						if (NamesToCheck[i] == NamesToCheck[i_name])
							isReplicate = true;

					if (!isReplicate)
					{
						// Check for patch existence
						for (int k = 0; k < Patch.Count; k++)
						{
							if (NamesToCheck[i_name] == Patch[k].PatchName)
							{
								// found the patch, add to list
								SelectedIDs.Add(k);
								k = Patch.Count;
							}
							// else{}  continue looking for next name
						}
					}
				}
			}
		}
		// else{} No names were given

		// 1. Check IDs
		if (IDsToCheck.Length > 0)
		{  // at least one ID was given, check for existence and get ID
			for (int i_id = 0; i_id < IDsToCheck.Length; i_id++)
			{
				bool isReplicate = false;
				if (SelectedIDs.Count > 0)
				{ // there are ID i sthe list already, check for replicates
					for (int i = 0; i < SelectedIDs.Count; i++)
						if (SelectedIDs[i] == IDsToCheck[i_id])
						{ // already selected
							isReplicate = true;
							i = SelectedIDs.Count;
						}
				}
				// check for replicates in list given
				for (int i = 0; i < i_id; i++)
					if (IDsToCheck[i] == IDsToCheck[i_id])
						isReplicate = true;
				if (!isReplicate)

					// Check for patch existence
					for (int k = 0; k < Patch.Count; k++)
					{
						if (IDsToCheck[i_id] == k)
						{
							// found the patch, add to list
							SelectedIDs.Add(k);
							k = Patch.Count;
						}
						// else{}  continue looking for next name
					}
			}
		}
		// else{} No IDs were given

		if (SelectedIDs.Count == 0)
		{ // no valid patch was found, notify user
			string myMessage = " No valid patch was found to base the new patch being added - operation will be ignored";
			writeMessage(myMessage);
		}
		return SelectedIDs;
	}

	/// <summary>
	/// calculate how the dlt's (C and N) are partitioned amongst patches
	/// </summary>
	/// <param name="incomingDelta">The dlt to be partioned amongst patches</param>
	/// <param name="SoluteName">The solute or pool that is changing</param>
	/// <returns>The values of dlt partitioned for each existing patch</returns>
	private double[][] partitionDelta(double[] incomingDelta, string SoluteName, string PartitionType)
	{
		// 1. initialise the result array
		double[][] Result = new double[Patch.Count][];
		for (int k = 0; k < Patch.Count; k++)
			Result[k] = new double[dlayer.Length];

		try
		{
			// 2- gather how much solute is already in the soil
			double[][] alreadyThere = new double[Patch.Count][];
			for (int k = 0; k < Patch.Count; k++)
			{
				switch (SoluteName)
				{
					case "Urea":
						alreadyThere[k] = Patch[k].urea;
						break;
					case "NH4":
						alreadyThere[k] = Patch[k].nh4;
						break;
					case "NO3":
						alreadyThere[k] = Patch[k].no3;
						break;
					default:
						throw new Exception(" The solute " + SoluteName
							+ " is not recognised by SoilNitrogen -  solute partition");
				}
			}

			// 3- calculations are done for each layer 
			for (int layer = 0; layer < (dlayer.Length); layer++)
			{
				// 3.1- compute the total solute amount, over all patches
				double totalSolute = 0.0;
				double[] patchSolute = new double[Patch.Count];
				if ((PartitionType == "BasedOnLayerConcentration".ToLower()) ||
					(PartitionType == "BasedOnConcentrationAndDelta".ToLower() & incomingDelta[layer] <= 0))
				{
					for (int k = 0; k < Patch.Count; k++)
					{
						patchSolute[k] += alreadyThere[k][layer];
						totalSolute += alreadyThere[k][layer] * Patch[k].RelativeArea;
					}
				}
				else if ((PartitionType == "BasedOnSoilConcentration".ToLower()) ||
						 (PartitionType == "BasedOnConcentrationAndDelta".ToLower() & incomingDelta[layer] > 0))
				{
					for (int k = 0; k < Patch.Count; k++)
						for (int z = layer; z >= 0; z--)
						{
							patchSolute[k] += alreadyThere[k][z];
							totalSolute += alreadyThere[k][z] *Patch[k].RelativeArea;
						}
				}

				// 3.2- calculations for each patch
				for (int k = 0; k < Patch.Count; k++)
				{
					// 3.2.1- compute the weights (based on existing solute amount)
					double weight = 1.0;
					if (totalSolute > 0)
						weight = MathUtility.Divide(patchSolute[k], totalSolute, 0.0);

					// 3.2.2- partition the dlt's for each patch
					Result[k][layer] = incomingDelta[layer] * weight;
				}
			}
		}
		catch (Exception e)
		{
			throw new Exception(" problems with partitioning " + SoluteName + "- " + e.ToString());
		}

		return Result;
	}


}
