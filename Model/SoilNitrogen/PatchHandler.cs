using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using CSGeneral;
using System.Xml;

/// <summary>
/// This partial class contains the various methods to handle patches
/// </summary>

    public partial class SoilNitrogen
    {

        /// <summary>
		/// Split an existing patch in two. That is, creates a new patch (k) based on an existing one (j)
        /// </summary>
        /// <param name="j"></param>
		private void SplitPatch(int j)
        {
            // create new patch
            soilCNPatch newPatch = new soilCNPatch(this);
            Patch.Add(newPatch);
            int k = Patch.Count - 1;

            // set the size of arrays
            Patch[k].ResizeLayerArrays(dlayer.Length);

            // set C and N variables to the same state as the 'mother' patch
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Patch[k].urea[layer] = Patch[j].urea[layer];
                Patch[k].nh4[layer] = Patch[j].nh4[layer];
                Patch[k].no3[layer] = Patch[j].no3[layer];
                Patch[k].inert_c[layer] = Patch[j].inert_c[layer];
                Patch[k].biom_c[layer] = Patch[j].biom_c[layer];
                Patch[k].biom_n[layer] = Patch[j].biom_n[layer];
                Patch[k].hum_c[layer] = Patch[j].hum_c[layer];
                Patch[k].hum_n[layer] = Patch[j].hum_n[layer];
                Patch[k].fom_c_pool1[layer] = Patch[j].fom_c_pool1[layer];
                Patch[k].fom_c_pool2[layer] = Patch[j].fom_c_pool2[layer];
                Patch[k].fom_c_pool3[layer] = Patch[j].fom_c_pool3[layer];
                Patch[k].fom_n_pool1[layer] = Patch[j].fom_n_pool1[layer];
                Patch[k].fom_n_pool2[layer] = Patch[j].fom_n_pool2[layer];
                Patch[k].fom_n_pool3[layer] = Patch[j].fom_n_pool3[layer];
            }

            // store today's values
            Patch[k].InitCalc();
        }

		/// <summary>
		/// Merges two patches
		/// </summary>
		/// <param name="recipient">Patch which will recieve the areas and the status of the disappearing patch</param>
		/// <param name="disappearing">Patch that will no longer exist</param>
        private void MergePatches(int recipient, int disappearing)
        {
            // get the weighted average for each variable and assign to the recipient patch
            double[] newValue = new double[dlayer.Length];
            for (int layer = 0; layer < dlayer.Length; layer++)
            {
                Patch[recipient].urea[layer] = (Patch[recipient].urea[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].urea[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].nh4[layer] = (Patch[recipient].nh4[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].nh4[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].no3[layer] = (Patch[recipient].no3[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].no3[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].inert_c[layer] = (Patch[recipient].inert_c[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].inert_c[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].biom_c[layer] = (Patch[recipient].biom_c[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].biom_c[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].biom_n[layer] = (Patch[recipient].biom_n[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].biom_n[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].hum_c[layer] = (Patch[recipient].hum_c[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].hum_c[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].hum_n[layer] = (Patch[recipient].hum_n[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].hum_n[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].fom_c_pool1[layer] = (Patch[recipient].fom_c_pool1[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].fom_c_pool1[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].fom_c_pool2[layer] = (Patch[recipient].fom_c_pool2[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].fom_c_pool2[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].fom_c_pool3[layer] = (Patch[recipient].fom_c_pool3[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].fom_c_pool3[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].fom_n_pool1[layer] = (Patch[recipient].fom_n_pool1[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].fom_n_pool1[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].fom_n_pool2[layer] = (Patch[recipient].fom_n_pool2[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].fom_n_pool2[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
                Patch[recipient].fom_n_pool3[layer] = (Patch[recipient].fom_n_pool3[layer] * Patch[recipient].RelativeArea 
					+ Patch[disappearing].fom_n_pool3[layer] * Patch[disappearing].RelativeArea) / Patch[recipient].RelativeArea;
            }

            // delete disappearing patch
            Patch.RemoveAt(disappearing);
        }

        /// <summary>
		/// Check the list of patch names and ids passed by 'AddSoilCNPatch' event
        /// </summary>
		/// <remarks>
		/// Verify whether the names correspond to existing patches, verify whether there are replicates
		/// </remarks>
		/// <documentation>
		/// With the AddSoilCNPatch event the user can tell the index or the name of the patch to which urine (or whatever) is added;
		///  this function will then check whether the patch(es) exist or not and filter out any replicates.
		///  User can pass ids (indices) or names of patches, or even both. This function will check both, eliminate replicates and
		///   non-existent references. The output is only the indices of selected patches
		/// </documentation>
        /// <param name="IDsToCheck">IDs or indices of patches</param>
        /// <param name="NamesToCheck">Name of patches</param>
        /// <returns>List of patch IDs, or indices</returns>
		private int[] CheckPatchIDs(int[] IDsToCheck, string[] NamesToCheck)
        {
            // List of patch ids for output
            List<int> SelectedIDs = new List<int>();

            if (Math.Max(IDsToCheck.Length, NamesToCheck.Length) > 0)
            {  // at least one patch has been selected
                if (NamesToCheck.Length > 0)
                {  // at least one name was selected, check value and get id
                    for (int pName = 0; pName < NamesToCheck.Length; pName++)
                    {
                        for (int k = 0; k < Patch.Count; k++)
                        {
                            if (NamesToCheck[pName] == Patch[k].PatchName)
                            {  // found the patch, check for replicates
								if (SelectedIDs.Count < 1)
								{ // this is the first patch, store the id
									SelectedIDs.Add(k);
									k = Patch.Count;
								}
								else
								{
									for (int i = 0; i <= SelectedIDs.Count; i++)
									{
										if (SelectedIDs[i] == k)
										{  // id already selected
											i = SelectedIDs.Count;
											k = Patch.Count;
										}
										else
										{  // store the id
											SelectedIDs.Add(k);
											i = SelectedIDs.Count;
											k = Patch.Count;
										}
									}
								}
                            }
                            else
                            {  // name passed did not correspond to any patch
                                Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year=" 
									+ Clock.Today.DayOfYear.ToString() + "), SoilNitrogen.AddPatch:");
                                Console.WriteLine("  the patch name '" + NamesToCheck[pName] + "' did not correspond to any existing patch." 
									+ " Patch will be ignored.");
                            }
                        }
                    }
                }
                if (IDsToCheck.Length > 0)
                {  // at least one ID was selected, check value
                    for (int pId = 0; pId < IDsToCheck.Length; pId++)
                    {
                        for (int k = 0; k < Patch.Count; k++)
                        {
                            if (IDsToCheck[pId] == k)
                            {  // found the patch, check for replicates
								if (SelectedIDs.Count < 1)
								{ // this is the first patch, store the id
									SelectedIDs.Add(k);
									k = Patch.Count;
								}
								else
								{
									for (int i = 0; i < SelectedIDs.Count; i++)
									{
										if (SelectedIDs[i] == k)
										{  // id already selected
											i = SelectedIDs.Count;
											k = Patch.Count;
										}
										else
										{  // store the id
											SelectedIDs.Add(k);
											i = SelectedIDs.Count;
											k = Patch.Count;
										}
									}
								}
                            }
                            else
                            {  // id passed did not correspond to any patch
                                Console.WriteLine(Clock.Today.ToString("dd MMMM yyyy") + "(Day of year=" + Clock.Today.DayOfYear.ToString() 
									+ "), SoilNitrogen.AddPatch:");
                                Console.WriteLine("  the patch id '" + IDsToCheck[pId] + "' did not correspond to any existing patch." 
									+ " Patch will be ignored.");
                            }
                        }
                    }
                }
            }
            else
            {  // no patch was indicated, use 'base'
                SelectedIDs.Add(0);
            }
            // pass data into an array to return as result
            int[] result = new int[SelectedIDs.Count];
            for (int i = 0; i < SelectedIDs.Count; i++)
                result[i] = SelectedIDs[i];
            return result;
        }

        /// <summary>
		/// Compare the diffs for all existing patches
        /// </summary>
		/// <returns>The IDs of pairs that can be merged</returns>
		private PatchIDs ComparePatches()
        {
            // Note:
            //	The code added so far is only tentative, better rulles to decide which patches are similar need to be developed

            PatchIDs Patches = new PatchIDs();
            List<int> recipient = new List<int>();
            List<int> disappearing = new List<int>();

            for (int k1 = 0; k1 < Patch.Count; k1++)
            {
                for (int k2 = k1 + 1; k2 < Patch.Count; k2++)
                {
                    // go through a series of criteria t evaluate whether the two patches can be merged
                    if (Math.Abs(Patch[k1].carbon_tot[0] - Patch[k2].carbon_tot[0]) < EPSILON)
                    {
                        if (Math.Abs(Patch[k1].no3[0] - Patch[k2].no3[0]) < EPSILON)
                        {
                            recipient.Add(k1);
                            disappearing.Add(k2);
                        }
                    }
                }
            }
            Patches.recipient = recipient;
            Patches.disappearing = disappearing;
            return Patches;
        }

        /// <summary>
		/// calculate how the dlt's (C and N) are partitioned amongst patches
        /// </summary>
        /// <param name="incoming">The dlt to be partioned amongst patches</param>
        /// <param name="SoluteName">The solute or pool that is changing</param>
        /// <returns>The values of dlt for each existing patch</returns>
		private double[][] partitionDelta(double[] incoming, string SoluteName, string PartitionType)
        {
            // 1- initialise the result to zero
            double[][] result = new double[Patch.Count][];
            for (int k = 0; k < Patch.Count; k++)
                result[k] = new double[dlayer.Length];

            try
            {
                // 2- gather how much solute is already in the soil
                double[][] alreadyThere = new double[Patch.Count][];
                for (int k = 0; k < Patch.Count; k++)
                {
					switch (SoluteName)
					{
						case "urea":
							alreadyThere[k] = Patch[k].urea;
							break;
						case "nh4":
							alreadyThere[k] = Patch[k].nh4;
							break;
						case "no3":
							alreadyThere[k] = Patch[k].no3;
							break;
						default:
							throw new System.InvalidOperationException(" The solute" + SoluteName 
								+ " is not recognised by SoilNitrogen -  solute partition");
					}

					//alreadyThere[k] = new double[dlayer.Length];
					//if (SoluteName == "urea")
					//    for (int layer = 0; layer < dlayer.Length; layer++)
					//        alreadyThere[k][layer] = Patch[k].urea[layer];
					//else if (SoluteName == "nh4")
					//    for (int layer = 0; layer < dlayer.Length; layer++)
					//        alreadyThere[k][layer] = Patch[k].nh4[layer];
					//else if (SoluteName == "no3")
					//    for (int layer = 0; layer < dlayer.Length; layer++)
					//        alreadyThere[k][layer] = Patch[k].no3[layer];
                }

                // 3- calculations are done for each layer 
                for (int layer = 0; layer < (dlayer.Length); layer++)
                {
                    // 3.1- compute the total solute amount, over all patches
                    double totalSolute = 0.0;
					if (PartitionType == "BasedOnLayerConcentration")
					//if ((PartitionType == "BasedOnLayerConcentration") || (PartitionType == "BasedOnConcentrationAndDelta" & incoming[layer]<=0))
					{
						for (int k = 0; k < Patch.Count; k++)
							totalSolute += alreadyThere[k][layer] * Patch[k].RelativeArea;
					}
					else if ((PartitionType == "BasedOnSoilConcentration") || (PartitionType == "BasedOnConcentrationAndDelta" & incoming[layer] > 0))
					{
						for (int k = 0; k < Patch.Count; k++)
							for (int z = layer; z >= 0; z--)
								totalSolute += alreadyThere[k][z] * Patch[k].RelativeArea;
					}

					// 3.2- calculations for each patch
                    for (int k = 0; k < Patch.Count; k++)
                    {
                        // 3.2.1- compute the weights (based on existing solute amount)
                        double weight = 1.0;
                        if (totalSolute > 0)
                            weight = alreadyThere[k][layer] / totalSolute;

                        // 3.2.2- partition the dlt's for each patch
                        result[k][layer] = incoming[layer] * weight;
                    }
                }
            }
            catch (Exception e)
            {
                throw new System.InvalidOperationException(" problem with " + SoluteName + "- " + e.ToString());
            }
            return result;
        }
    }
