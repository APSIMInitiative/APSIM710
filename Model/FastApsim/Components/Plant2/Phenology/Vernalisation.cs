

public class Vernalisation : Instance
   {
   [Param] private string StartStage = "";
   [Param] private string EndStage = "";
   [Output] private double CumulativeVD = 0;
   [Ref("parent(Plant).Phenology")] Phenology Phenology;
   [Ref("VDModel")] TemperatureFunction VDModel;

   /// <summary>
   /// Trap the NewMet event.
   /// </summary>
   [EventHandler]
   public void OnNewMet(NewMetType NewMet)
      {
      if (Phenology.Between(StartStage, EndStage))
         DoVernalisation(NewMet.maxt, NewMet.mint);
      }

   /// <summary>
   /// Initialise everything
   /// </summary>
   [EventHandler]
   public void OnInitialised()
      {
      CumulativeVD = 0;
      }

   /// <summary>
   /// Do our vernalisation
   /// </summary>
   public void DoVernalisation(double Maxt, double Mint)
      {
      CumulativeVD += VDModel.Value;
      }


   }