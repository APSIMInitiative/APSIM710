using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DCAPST.Interfaces
{
    /// <summary>
    /// Represents a model that simulates daily photosynthesis
    /// </summary>
    public interface IPhotosynthesisModel
    {
        void DailyRun(double lai, double sln, double soilWater, double RootShootRatio);
        //void DailyRun(double lai, double greenN, double nLowLimit, double nLAITrigger, double soilWater, double RootShootRatio);
    }
}
