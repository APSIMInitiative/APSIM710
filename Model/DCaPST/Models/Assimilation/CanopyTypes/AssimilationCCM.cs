using DCAPST.Interfaces;

namespace DCAPST
{
    /// <summary>
    /// Defines the pathway functions for a CCM canopy
    /// </summary>
    public class AssimilationCCM : Assimilation
    {
        public AssimilationCCM(ICanopyParameters canopy, IPathwayParameters parameters) : base(canopy, parameters)
        { }

        /// <inheritdoc/>
        public override void UpdateIntercellularCO2(AssimilationPathway pathway, double gt, double waterUseMolsSecond)
        {
            pathway.IntercellularCO2 = ((gt - waterUseMolsSecond / 2.0) * canopy.AirCO2 - pathway.CO2Rate) / (gt + waterUseMolsSecond / 2.0);
        }

        /// <inheritdoc/>
        protected override void UpdateMesophyllCO2(AssimilationPathway pathway, TemperatureResponse leaf)
        {
            pathway.MesophyllCO2 = pathway.IntercellularCO2 - pathway.CO2Rate / leaf.GmT;
        }

        /// <inheritdoc/>
        protected override void UpdateChloroplasticO2(AssimilationPathway pathway)
        {
            pathway.ChloroplasticO2 = parameters.PS2ActivityFraction * pathway.CO2Rate / (canopy.DiffusivitySolubilityRatio * pathway.Gbs) + canopy.AirO2;
        }

        /// <inheritdoc/>
        protected override void UpdateChloroplasticCO2(AssimilationPathway pathway, AssimilationFunction func)
        {
            var a = (pathway.MesophyllCO2 * func.X[3] + func.X[4] - func.X[5] * pathway.CO2Rate - func.MesophyllRespiration - func.X[6]);
            pathway.ChloroplasticCO2 = pathway.MesophyllCO2 + a * func.X[7] / pathway.Gbs;
        }

        /// <inheritdoc/>
        protected override AssimilationFunction GetAc1Function(AssimilationPathway pathway, TemperatureResponse leaf)
        {
            var x = new double[9];

            x[0] = leaf.VcMaxT;
            x[1] = leaf.Kc / leaf.Ko;
            x[2] = leaf.Kc;
            x[3] = leaf.VpMaxT / (pathway.MesophyllCO2 + leaf.Kp);
            x[4] = 0.0;
            x[5] = 0.0;
            x[6] = pathway.ChloroplasticCO2 * leaf.VcMaxT / (pathway.ChloroplasticCO2 + (leaf.Kc / leaf.Ko) * pathway.ChloroplasticO2 + leaf.Kc);
            x[7] = 1.0;
            x[8] = 1.0;

            var func = new AssimilationFunction()
            {
                X = x,

                MesophyllRespiration = leaf.GmRd,
                HalfRubiscoSpecificityReciprocal = leaf.Gamma,
                FractionOfDiffusivitySolubilityRatio = 0.1 / canopy.DiffusivitySolubilityRatio,
                BundleSheathConductance = pathway.Gbs,
                Oxygen = canopy.AirO2,
                Respiration = leaf.RdT
            };

            return func;
        }

        /// <inheritdoc/>
        protected override AssimilationFunction GetAc2Function(AssimilationPathway pathway, TemperatureResponse leaf)
        {
            var x = new double[9];

            x[0] = leaf.VcMaxT;
            x[1] = leaf.Kc / leaf.Ko;
            x[2] = leaf.Kc;
            x[3] = 0.0;
            x[4] = pathway.Vpr;
            x[5] = 0.0;
            x[6] = pathway.ChloroplasticCO2 * leaf.VcMaxT / (pathway.ChloroplasticCO2 + leaf.Kc * (1 + pathway.ChloroplasticO2 / leaf.Ko));
            x[7] = 1.0;
            x[8] = 1.0;

            var func = new AssimilationFunction()
            {
                X = x,

                MesophyllRespiration = leaf.GmRd,
                HalfRubiscoSpecificityReciprocal = leaf.Gamma,
                FractionOfDiffusivitySolubilityRatio = 0.1 / canopy.DiffusivitySolubilityRatio,
                BundleSheathConductance = pathway.Gbs,
                Oxygen = canopy.AirO2,
                Respiration =leaf.RdT
            };

            return func;
        }

        /// <inheritdoc/>
        protected override AssimilationFunction GetAjFunction(AssimilationPathway pathway, TemperatureResponse leaf)
        {
            var x = new double[9];

            x[0] = (1 - parameters.MesophyllElectronTransportFraction) * parameters.ATPProductionElectronTransportFactor * leaf.J / 3.0;
            x[1] = 7.0 / 3.0 * leaf.Gamma;
            x[2] = 0.0;
            x[3] = 0.0;
            x[4] = parameters.MesophyllElectronTransportFraction * parameters.ATPProductionElectronTransportFactor * leaf.J / parameters.ExtraATPCost;
            x[5] = 0.0;
            x[6] = pathway.ChloroplasticCO2 * (1 - parameters.MesophyllElectronTransportFraction) * parameters.ATPProductionElectronTransportFactor * leaf.J / (3 * pathway.ChloroplasticCO2 + 7 * leaf.Gamma * pathway.ChloroplasticO2);
            x[7] = 1.0;
            x[8] = 1.0;

            var func = new AssimilationFunction()
            {
                X = x,

                MesophyllRespiration = leaf.GmRd,
                HalfRubiscoSpecificityReciprocal = leaf.Gamma,
                FractionOfDiffusivitySolubilityRatio = 0.1 / canopy.DiffusivitySolubilityRatio,
                BundleSheathConductance = pathway.Gbs,
                Oxygen = canopy.AirO2,
                Respiration = leaf.RdT
            };

            return func;
        }
    }
}
