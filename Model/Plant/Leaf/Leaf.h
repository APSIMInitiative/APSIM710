#ifndef LeafH
#define LeafH
#include "../SimplePart.h"
#include "../Co2Modifier.h"
#include "../Photosynthesis/PhotosynthesisModel.h"
#include "../Photosynthesis/DCaPST.h"

// Abstract class for leaf objects
class Leaf : public SimplePart
{
public:
	Leaf(ScienceAPI& scienceAPI, plantInterface* p, const string& name);

	virtual ~Leaf() { delete Photosynthesis; };

	virtual void onHarvest(float height, float remove_fr,
		vector<string>& dm_type,
		vector<float>& dlt_crop_dm,
		vector<float>& dlt_dm_n,
		vector<float>& dlt_dm_p,
		vector<float>& fraction_to_residue) = 0;

	virtual void onEndCrop(vector<string>& dm_type,
		vector<float>& dlt_crop_dm,
		vector<float>& dlt_dm_n,
		vector<float>& dlt_dm_p,
		vector<float>& fraction_to_residue);

	virtual void zeroAllGlobals(void) = 0;
	virtual void zeroDeltas(void) = 0;
	virtual void checkBounds(void) = 0;
	//virtual void onInit1(protocol::Component *) =0;
	virtual void readConstants(protocol::Component*, const string&) = 0;
	virtual void readSpeciesParameters(protocol::Component* system, vector<string>& search_order) = 0;
	virtual void onEmergence(void) = 0;
	virtual void onKillStem(void) = 0;
	virtual void onCanopyWaterBalance(protocol::CanopyWaterBalanceType& v);
	virtual float getLAI(void) = 0;                             //
	virtual float getSLAI(void) = 0;                            //
	virtual float getLeafNo(void) = 0;                          // The number of leaves
	virtual float getNodeNo(void) = 0;                          // The number of nodes
	virtual float getDltNodeNo(void) = 0;                       // The change in number of nodes
	virtual float senFract(void) = 0;                          // Fraction of canopy senescing today
	virtual float dmGreenDemand(void) = 0;                       // Maximum DM this part can take today

	virtual void onInit1(protocol::Component*);

	virtual void CanopyExpansion(int option, float, float, float) = 0;             // Calculate potentials
	virtual void actual(void) = 0;                                     // Calculate actual leaf development from potential & stressed
	virtual void leaf_death(float nfact_expansion, float  dlt_tt) = 0;//
	virtual void leaf_area_sen(float) = 0;
	virtual void update(void) = 0;
	virtual void removeBiomass(void) = 0;

	virtual float dmRetransSupply(void);
	virtual void  doNConccentrationLimits(float modifier);
	virtual void doSWDemand(float SWDemandMaxFactor);
	virtual float DMSupply(void);

	virtual float coverTotal(void);
	virtual float coverGreen(void);
	virtual float coverSen(void);
	virtual void doCover(PlantSpatial& spatial);
	virtual void doDmPotRUE(void);

	struct Cover
	{
		float green;
		float sen;
	};
	Cover coverLeaf;
	float cXRowSpacing[max_table];
	float cYExtinctCoef[max_table];
	float cYExtinctCoefDead[max_table];
	int cNumRowSpacing;

	PhotosynthesisModel* Photosynthesis;
	std::vector<PhotosynthesisModel*> PsModels;

	void onSetTEFactor(float Factor);
	void onSetPSModel(std::string Factor);
	void onSetLaiTrigger(float value);
	void ChoosePsModel(string PsModelName);
	virtual float getSLN(void) = 0;

	string PsModelName;
	string PsModelName1;
	float TEFactor;
	bool ExternalSWDemand;
	bool DCaPSTLowLAI;

private:
	float DCaPSTTriggerLAI;
};

#endif

