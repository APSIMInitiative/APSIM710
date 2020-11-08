using System;

namespace DCAPST.Interfaces
{
    /// <summary>
    /// Represents a model that simulates daily photosynthesis
    /// </summary>
    public interface IPhotosynthesisModel
    {
        void DailyRun(double lai, double sln, double soilWater, double RootShootRatio);
    }
}
