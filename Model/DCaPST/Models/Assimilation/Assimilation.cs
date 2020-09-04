using DCAPST.Interfaces;

namespace DCAPST
{
    /// <summary>
    /// Tracks the state of an assimilation type
    /// </summary>
    public abstract class Assimilation : IAssimilation
    {
        /// <summary>
        /// The parameters describing the canopy
        /// </summary>
        protected ICanopyParameters canopy;

        /// <summary>
        /// The parameters describing the pathways
        /// </summary>
        protected IPathwayParameters parameters;

        public Assimilation(ICanopyParameters canopy, IPathwayParameters parameters)
        {
            this.canopy = canopy;
            this.parameters = parameters;
        }       

        /// <summary>
        /// Factory method for accessing the different possible terms for assimilation
        /// </summary>
        public AssimilationFunction GetFunction(AssimilationPathway pathway, TemperatureResponse leaf)
        {
            if (pathway.Type == PathwayType.Ac1) return GetAc1Function(pathway, leaf);
            else if (pathway.Type == PathwayType.Ac2) return GetAc2Function(pathway, leaf);
            else return GetAjFunction(pathway, leaf);
        }

        /// <inheritdoc/>
        public void UpdatePartialPressures(AssimilationPathway pathway, TemperatureResponse leaf, AssimilationFunction function)
        {
            UpdateMesophyllCO2(pathway, leaf);
            UpdateChloroplasticO2(pathway);
            UpdateChloroplasticCO2(pathway, function);
        }

        /// <inheritdoc/>
        public virtual void UpdateIntercellularCO2(AssimilationPathway pathway, double gt, double waterUseMolsSecond) 
        { /*C4 & CCM overwrite this.*/ }

        /// <summary>
        /// Updates the mesophyll CO2 parameter
        /// </summary>
        protected virtual void UpdateMesophyllCO2(AssimilationPathway pathway, TemperatureResponse leaf) 
        { /*C4 & CCM overwrite this.*/ }

        /// <summary>
        /// Updates the chloroplastic O2 parameter
        /// </summary>
        protected virtual void UpdateChloroplasticO2(AssimilationPathway pathway) 
        { /*CCM overwrites this.*/ }

        /// <summary>
        /// Updates the chloroplastic CO2 parameter
        /// </summary>
        protected virtual void UpdateChloroplasticCO2(AssimilationPathway pathway, AssimilationFunction func) 
        { /*CCM overwrites this.*/ }

        /// <summary>
        /// Retrieves a function describing assimilation along the Ac1 pathway
        /// </summary>
        protected abstract AssimilationFunction GetAc1Function(AssimilationPathway pathway, TemperatureResponse leaf);

        /// <summary>
        /// Retrieves a function describing assimilation along the Ac2 pathway
        /// </summary>
        protected abstract AssimilationFunction GetAc2Function(AssimilationPathway pathway, TemperatureResponse leaf);

        /// <summary>
        /// Retrieves a function describing assimilation along the Aj pathway
        /// </summary>
        protected abstract AssimilationFunction GetAjFunction(AssimilationPathway pathway, TemperatureResponse leaf);
    }
}
