#ifndef LeafLERH
#define LeafLERH
#include "PlantComponents.h"
#include "Utilities.h"
#include "Leaf.h"

namespace Maize 
   {
   //------------------------------------------------------------------------------------------------
   class PlantLeaf
      {
      public:
         double potentialWidth;              // width that this leaf can achieve
         double length, width;                // length and width in mm
         double area;                         // calculated area (l x w x 0.75)
         double exposedArea;                  // area of the leaf that is exposed to light

         double LERCoef;                     // weighting by rank of the leaf expansion rate (LER)
         double fracSenesced;

         // thermal time targets for leaf development
         double initTT;                       // TT of initiation of each leaf
         double startExpTT;                   // TT at start of expansion (initTT + 100)
         double tipTT;                        // TT of leaf tip appearance
         double fullyExpTT;                   // TT at initiation, when fully expanded (liguleTT - 50)
         double liguleTT;                     // TT at ligule appeareance

         double fracPopn;
         PlantLeaf(void)
            {
            length = 0.0;width = 0.0;area = 0.0;exposedArea = 0.0;fracSenesced = 0.0;fracPopn = 0.0;
            };
         double CalcArea(void)
            {
            area = length * width * 0.75;
            return area;
            }
         void setWidth(double _width){width = _width;}
      };
   //------------------------------------------------------------------------------------------------
   class PlantLeaves
      {
      private:
         int SLnumber;        // number of small leaves (3)
         double SLsize;        // size of small leaves (15 mm)
         int BLrankFLN;       // rank of largest leaf relative to FLN (FLN - 5)
         double BLsize;        // size of largest leaf (100 mm)

      public:
         vector<PlantLeaf> leaf;
         int nLeaves(void){ return leaf.size();}

         // leaf widths
         void potentialWidths(int fln);
         void SetLeafWidthParams(int _SLnumber, double _SLsize, int _BLrankFLN, double _BLsize)
            {
            SLnumber = _SLnumber;
            SLsize = _SLsize;
            BLrankFLN = _BLrankFLN;
            BLsize = _BLsize;
            };
         double totalArea(void)
            {
            double area = 0.0;
            for(unsigned i=0;i < leaf.size();i++)
               area += leaf[i].area * leaf[i].fracPopn;
            return area;
            };
         double exposedArea(void)
            {
            double area = 0.0;
            for(unsigned i=0;i < leaf.size();i++)
               area += leaf[i].exposedArea * leaf[i].fracPopn;
            return area;
            };
         double senescedArea(void)
            {
            double area = 0.0;
            for(unsigned i=0;i < leaf.size();i++)
               area += leaf[i].area * leaf[i].fracSenesced * leaf[i].fracPopn;
            return area;
            };
         double deadLeaves(void)
            {
            double leaves = 0.0;
            for(unsigned i=0;i < leaf.size();i++)
               leaves += leaf[i].fracSenesced * leaf[i].fracPopn;
            return leaves;
            }
         void setDeadLeaves(double deadLeaves)
            {
            for(unsigned i=0;i < leaf.size();i++)
               if((int)i < deadLeaves)
                  leaf[i].fracSenesced = Min(1.0,deadLeaves - i);
            }
      };
   //------------------------------------------------------------------------------------------------
   class CalcLeafWidth
      {
      int SLnumber;        // number of small leaves (3)
      double SLsize;        // size of small leaves (15 mm)
      int BLrankFLN;       // rank of largest leaf relative to FLN (FLN - 5)
      double BLsize;        // size of largest leaf (100 mm)
      public:
         void SetLeafWidthParams(int _SLnumber, double _SLsize, int _BLrankFLN, double _BLsize)
            {
            SLnumber = _SLnumber;
            SLsize = _SLsize;
            BLrankFLN = _BLrankFLN;
            BLsize = _BLsize;
            };
         void SetWidths(vector<double> &widths,int fln)
            {
            // calculate the vector of leaf widths and assign them to the leafs
            double *width = new double [fln];
            for(int i=0;i < SLnumber;i++)
               {
               width[i] = SLsize;
               }
            for(int i = 0;i < 3;i++)
               {
               width[fln - BLrankFLN - 2 + i] = BLsize;
               }
            // get slope of leaf size between smallest and largest
            double deltaY = BLsize - SLsize;
            double deltaX = (fln - BLrankFLN - 1) - SLnumber;
            double slope = divide(deltaY,deltaX);
            for(int i = SLnumber; i < (fln - BLrankFLN - 1);i++)
               width[i] = SLsize + slope * (i + 1 - SLnumber);
            for(int i = (fln - BLrankFLN + 1); i < fln;i++)
               width[i] = BLsize - slope * (i + 1 - (fln - BLrankFLN + 1));

            // assign to leaves
            widths.clear();
            for(int i=0;i < fln;i++)
               widths.push_back(width[i]);
            delete [] width;
            }

      };
   //--------------------------------------------------------------------------------------------
   class LeafLER : public Leaf
      {
      protected:
         // Temp Params
         double latitude;
         double maxFlux;
         double maxLag;
         double nightCoef;
         double minLag;

         vector <double> hRadn;
         vector <double> TAirParam;
         vector <double> TAir;
         vector <double> TSoilParam;
         vector <double> TSoil;
         vector <double> SVP;
         vector <double> RH;
         vector <double> VPDair;
         vector <double> transDemand;
         vector <double> transDemandFluxLimited;
         vector <double> hBio;

         //Leaf Params
         int useProfileFTSW;                          // 1 if whole profile FTSW is used else FTSW

         double leafNoInitEmerg;                      //number of leaves initiated at plant emergence
         double ttInitToGrowth;                       //shift between the initiation of a leaf and
         //      the time it began to expand
         double ttEndExpToLigule;                      // oC before ligule appearance that growth stops
         double leaf_no_tip_at_emerg;                  //number of leaves having an emerged tip, at plant emergence
         double leaf_no_ligu_at_emerg;                 //number of ligulated leaves at plant emergence
         double LIR;                                   //leaf initiation rate
         double LTAR;                                  //leaf tip apparance rate
         double LLAR1;                                 //leaf ligule apparance rate for the first leaves
         double LLAR2;                                 //leaf ligule apparance rate for the midle leaves
         double LLAR3;                                 //leaf ligule apparance rate for the last leaves

         // start of leaf expansion
         double leaf_no_begin_exp_at_emerg;
         double leaf_begin_exp_rate1;
         double leaf_begin_exp_rate2;
         double leaf_no_change_begin_exp;


         double leaf_no_change1;                       //
         double leaf_no_change2;                       //
         double a;                                     //genotype DEA
         double b;                                     //
         double c;                                     //
         double T0;                                    //

         int useLERCoef;                              // if 1 use function else use LER
         double aScaleLER;                             // parameters to calculate coef for LER by rank
         double bScaleLER;
         double LeafNoMaxLER;

         // --------------------  leaf widths
         int useLeafWidthFn;                          // if 1 use function else use observed
         CalcLeafWidth leafWidths;                    // class to calculate leaf widths from function
         vector<double> LeafWidth;                     // observed leaf widths

         TableFn LeafTemp;                            // calculate leaf temperatures

         double diam;                                  //
         double hm;                                    //height of the meristem
         double Pc;                                    //psychrometric constant
         double cp;                                    //heat capacity of the air
         double a_Psi;                                 //Psi=a_Psi+b_Psi * ln(FTSW)
         double b_Psi;                                 //
         double a_gs;                                  //gs = a_gs + b_gs * exp(-Psi / c_gs)
         double b_gs;                                  //
         double c_gs;                                  //

         //  LER model variables
         double nInitLeaves;                           // number of initiales leaves
         double nLeafTips;                             // number of leaves with emerged tip
         double nLeafExp;                              // number of leaves that have started to expand
         double nLeafFullyExp;                         // "Number of leaves that have fully expanded"
         double nLigules;                              // number of ligules - expanded leaves
         double nLigulesRep;                           // number of ligules for reporting purposes
         double LER;                                   // leaf emergence rate
         vector<double> RankLER;                       // vector of LER weighted by rank
         double psi;

         PlantLeaves plantLeaves;
         void UpdateLeaves(void);
         void GrowLeaves(void);

         double dltLERlai;

         vector <double> demand;
         vector <double> supply;
         vector <double> TLeaf;
         vector <double> VPDairLeaf;
         vector <double> VPDeq;
         vector <double> hLER;

         vector <double> leafSize;  // potential leaf size     (Col's)

         virtual void doRegistrations();
         virtual void initialize();
         virtual void readParams();
         virtual void readTParams();

			// replace functions
			virtual void   calcLeafNo();
			virtual void   calcPotentialArea();
			virtual double calcStressedLeafArea();
			virtual double calcEmergFlagTT();

			// new functions
			void   updateLeaves();
			void   growLeaves();
         double calcLER();

         // functions that may later go into UTILS
         void CalcTDemand(vector <double>, double, double, double, vector<double>, double, vector<double> &);
         void CalcHLER(double, double, double, double, vector <double>, vector <double>, double, vector<double> &);

      public:
         LeafLER(ScienceAPI2 &, Plant *p);
         ~LeafLER();
      };
   }
#endif