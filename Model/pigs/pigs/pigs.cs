using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;

 public class pigs :animal
 {
    [Link]
    private Component My;
    [Link]
    private Component theProducts;
    [Output]
    public double CinCH4;
    [Output]
    public double CinCO2;
    [Param]
     public double FE_need;
     [Param]
    public double NumberPrYear;
     [Param]
     public double K_growth;

    [Param]
    double N_growth;                   // N partitioned to tissue growth
    [Param]
    string fileInfo;        
    List<feedItemType> feedItemList;
     public pigs():base()
     {
         feedItemList = new List<feedItemType>();
         Console.WriteLine("con pig");
     }
     public string GetName()
     {
         return My.Name;
     }
     [Output]
     public double CinFeed;
     [Output]
     double CinSoldPig;
     [Output]
     double CinManure ;

     [EventHandler]
     public void OnProcess()
     {
        // Console.WriteLine(MyPaddock.ChildrenAsObjects.Count);
        // Console.WriteLine("con pig"+ My.ChildrenAsObjects.Count());
            FeedItem afeedItem;



      



         for (int i = 0; i < My.ChildrenAsObjects.Count; i++)
         {
             afeedItem = (FeedItem)My.ChildrenAsObjects[i];
             Console.WriteLine(afeedItem.pigFeedUnitsPerItemUnit +" dfdfdf");
             FeedItem storedfeedItem = new FeedItem();
           //  FeedItem storedfeedItem = (FeedItem)theProducts.GetCopyStoredFeedProduct( afeedItem);    // returns a copy of the product if it is found in storage

            
             if ((storedfeedItem.getAmount() > 0.0))
             {
                 storedfeedItem.setAmount(afeedItem.getAmount());
                // theProducts.SubtractProduct(storedfeedItem);    // subtract feed from storage
                 if (currentfeed.getAmount()!=0)
                    currentfeed = currentfeed + storedfeedItem;
                 else
                     currentfeed =storedfeedItem;

                
             }
             else
             {
                 if (afeedItem.GetproteinN_digestibility() < 0.0)
                 {

                     throw new System.ArgumentException("attempt to access feed with protein digestibility less than zero, code =" + afeedItem.GetCode(), "attempt to access feed with protein digestibility less than zero, code =" + afeedItem.GetCode());
                 }

                 //theProducts.SubtractProduct(afeedItem);    // subtract feed from storage


                 if (currentfeed.getAmount() != 0)
                     currentfeed = currentfeed + storedfeedItem;
                 else
                     currentfeed = afeedItem;
             }
         }
         Console.WriteLine(currentfeed.pigFeedUnitsPerItemUnit + "  currentfeed.pigFeedUnitsPerItemUnit");
         if (currentfeed.getAmount() == 0)
         {
         
             throw new System.ArgumentException("Pig:: No feed provided!", "Pig:: No feed provided!");
         }

         double surplusN = ProduceManure(fluidManurePrDay, solidManurePrDay);    // calculates the amount of manure pr day;

         // while (surplusN<0) //iteration necessary as feed quality could vary with amount requested from store, if several stores contain the desired feed
         if (surplusN < 0)
         {

             throw new System.ArgumentException("Pig:: Insufficient protein in feed for " + GetName(), "Pig:: Insufficient protein in feed for " + GetName());
         }

         manurePrDay = fluidManurePrDay;
         manurePrDay = manurePrDay + solidManurePrDay;

   

         // fudge - should calculate heat from diet + latent heat loss
         // GetStableSection()->AddanimalHeatOp(NumberPrDay*heatProduction);
         CinFeed = currentfeed.getAmount() * currentfeed.GetC_content();
         CinSoldPig = NumberPrDay * C_growth;
         CinManure = manurePrDay.GetAmount() * manurePrDay.GetC_content();
         CinCH4 = 0.01 * GetGrossEnergyinDryMatter() * currentfeed.getAmount() * currentfeed.GetdryMatter() / 55.55;

         Console.WriteLine(NumberPrDay+" "+C_growth);

         double CinCO2 = CinFeed - (CinSoldPig + CinManure + CinCH4);

         if (CinCO2 < 0.0)
         {
         
             throw new System.ArgumentException("pig::DailyUpdate - CO2 production cannot be negative", "pig::DailyUpdate - CO2 production cannot be negative");
         }

        
     }
     double GetGrossEnergyinDryMatter()
     {
         return 0;
     }
     double ProduceManure(manure fluidManure, manure solidManure)
    {
         
    
        double surplus_digestibleN = 0.0, Nconc = 0.0;
        double totalAmountPrDay;
        if (NumberPrDay > 1e-10)
        {
            // Fluid manure
            double amountfeed = currentfeed.amount;
            double energyInput = currentfeed.amount * currentfeed.pigFeedUnitsPerItemUnit;
         
            if (energyInput < 0.75 * (NumberPrDay * FE_need)) // BMP changed this january 2007
            {
                if (energyInput < 0.50 * (NumberPrDay * FE_need))
                {
      
                    Console.Write( "Energy as percentage of requirement = ");
                    Console.WriteLine(100.0 * energyInput / (NumberPrDay * FE_need));
                 
                      throw new System.ArgumentException("pig::ProduceManure -  Very low amount of energy in feed");

                }
                else
                    throw new System.ArgumentException("pig::ProduceManure -  Insufficient energy in feed");
                
            }

            if (energyInput > 1.6 * (NumberPrDay * FE_need))
            {
                double energyPercent = 100.0 * energyInput / (NumberPrDay * FE_need);
                 Console.Write(energyPercent);
                 Console.WriteLine("% excess energy in feed ");
            }

            double totalNInput = amountfeed * (currentfeed.orgN_content + currentfeed.NH4_content + currentfeed.NO3_content);
            double protein_N_conc = currentfeed.orgN_content;
            double proteinNDigested = amountfeed * protein_N_conc * (currentfeed.proteinN_digestibility);
            double ammoniumRatio; 					// CHECK!!!!  value taken from "Kvï¿½lstof i husdyrgï¿½dning", 1990, J.F. Hansen et al.
            double other_N = amountfeed * ((currentfeed.orgN_content + currentfeed.NH4_content + currentfeed.NO3_content) - protein_N_conc);
 
            double totalCInput = amountfeed * currentfeed.C_content;
 
            //urine N is the difference between the digested N (protein + other) and the N used for growth
            surplus_digestibleN = other_N + proteinNDigested - NumberPrDay * N_growth;
            /*
            cout << Alias << " N in feed per day per individual (kg N) " << (1000*totalNInput/NumberPrDay) << endl;  // Test - remove !!!
            cout << "N growth per day per individual (kg N) " << N_growth*1000.0 << endl;
            cout << "Protein digestibility " << currentfeed->GetproteinN_digestibility() << endl;
            cout << endl;         // Test - remove !!!
            */
            if (surplus_digestibleN >= 0)
            {
                //solid manure
                totalAmountPrDay = amountSolid * NumberPrDay / 1000.0; //convert to tonnes
                ammoniumRatio = 0.0;     //CHECK
                //faecal N is the undigested protein N
                Nconc = (amountfeed * protein_N_conc - proteinNDigested) / totalAmountPrDay;
                if (Nconc < 0.0)
                    throw new System.ArgumentException("pig::protein N digested greater than protein N present" + proteinNDigested);

                //         solidManure->SetfromAnimal(animalName);
                solidManure.amount=totalAmountPrDay;
                solidManure.orgN_content = (1.0 - ammoniumRatio) * Nconc;
                solidManure.NH4_content = (ammoniumRatio * Nconc);
                solidManure.NO3_content = (0.0);
                double faecalCarbon = amountfeed * currentfeed.C_content * (1 - currentfeed.OMD);
                solidManure.C_content = (faecalCarbon / totalAmountPrDay);
                solidManure.P_content = (amountfeed * (currentfeed.P_content) * (1.0 - currentfeed.P_digest) / totalAmountPrDay);
                solidManure.K_content=((amountfeed * (currentfeed.K_content) - NumberPrYear * K_growth) / totalAmountPrDay);
                solidManure.pH=(7.6);

                ammoniumRatio = 1.0;     //CHECK !!!

                totalAmountPrDay = amountFluid * NumberPrDay / 1000.0; //convert to tonnes
                fluidManure.amount=(totalAmountPrDay);
                //        fluidManure->SetfromAnimal(animalName);
                Nconc = surplus_digestibleN / totalAmountPrDay;
                double urineCconc = Nconc * 60.0 / 28.0;   //assumes all N in urine is urea (ratio of C to N in urea (2*(NH2) + CO))
                fluidManure.orgN_content = ((1.0 - ammoniumRatio) * Nconc);
                fluidManure.NH4_content = (ammoniumRatio * Nconc);
                fluidManure.NO3_content = (0.0);
                fluidManure.C_content = (urineCconc);
                fluidManure.P_content = ((amountfeed * (currentfeed.P_content) * (currentfeed.P_digest) - NumberPrDay * P_growth) / totalAmountPrDay);
                fluidManure.K_content = (0.0);                      // rettes senere
                fluidManure.pH=(8.0);

                double balance = totalNInput - (fluidManure.amount * (fluidManure.orgN_content + fluidManure.NH4_content + fluidManure.NO3_content) +
                                 solidManure.amount * (solidManure.orgN_content + solidManure.NH4_content + solidManure.NO3_content) + NumberPrDay * N_growth);
                if (balance > 0.0001)
                    throw new System.ArgumentException("pig::ProduceManure - error in N balance in pig manure partitioning");
            
            }
            else
                throw new System.ArgumentException("pig::ProduceManure -  required additional protein in feed");
               
            double Nefficiency = NumberPrDay * N_growth / totalNInput;
            if (Nefficiency > 0.7)
                throw new System.ArgumentException("pig::ProduceManure - N efficiency over 70%");
     
        }
        else
        {                                                             // sets the default values for manure

            fluidManure.amount=(0.0);
            solidManure.amount=(0.0);
        }
        return surplus_digestibleN;
    }
 }

