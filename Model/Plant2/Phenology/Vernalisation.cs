

public class Vernalisation : Instance
   {
   [Param] private string StartStage = "";
   [Param] private string EndStage = "";
   [Output] private double CumulativeVD = 0;

   /// <summary>
   /// Trap the NewMet event.
   /// </summary>
   [EventHandler]
   public void OnNewMet(NewMetType NewMet)
      {
      Plant Plant = (Plant)Root;
      if (Plant.Phenology.Between(StartStage, EndStage))
         DoVernalisation(NewMet.maxt, NewMet.mint);
      }

   /// <summary>
   /// Initialise everything
   /// </summary>
   public override void Initialising()
      {
      CumulativeVD = 0;
      }

   /// <summary>
   /// Do our vernalisation
   /// </summary>
   public void DoVernalisation(double Maxt, double Mint)
      {
      TemperatureFunction VDModel = (TemperatureFunction)Children["VDModel"];
      CumulativeVD += VDModel.Value;
      }


   }