#ifndef _WEATHER_H
#define _WEATHER_H

float DaylengthAndPhotoperiod(float fLatitude, int nJulianDay, int nID);
float SunshineHours(float fLatitude, int nDay, float fGlbRadDay, float a, float b);

float ExtraTerrestrialRadiation(float fLatitude, int nDay, float fHour);
float ExtraTerrestrialRadiationDailyTotal(float fLatitude, int nDay);
float GlobalRadiationDailyTotal(float fLatitude, int nDay, float fSunHours, float a=0.18, float b=0.55);
float GlobalRadiation(float fGlbRadDay, float fLatitude, int nDay, float fHour);
float RadiationDiffuseComponent(float fGlbRadDay, float fLatitude, int nDay, float fHour);
float DiffuseRadiation(float fGlbRadDay, float fLatitude, int nDay, float fHour);
float DirectRadiation(float fGlbRadDay, float fLatitude, int nDay, float fHour);
float RadiationDailyTotalDiffuseComponent(float fGlbRadDay, float fLatitude, int nDay);
float DiffuseRadiationDailyTotal(float fGlbRadDay, float fLatitude, int nDay);
float DirectRadiationDailyTotal(float fGlbRadDay, float fLatitude, int nDay);

float HourlyTemperature(float fHour,float fTmax,float fTmin);
float HourlyTemperature(float fLatitude, int nDay, float fHour,
			float fTmax, float fTmin,
  		        float fTmaxPrev, float fTminPrev,float fTminNext);
float DailyMeanTemperature(float fTmax,float fTmin, float fLatitude=-99.0, int nDay=-99, float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );
float DaytimeMeanTemperature(float fTmax,float fTmin, float fLatitude=-99.0, int nDay=-99, float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );
float NighttimeMeanTemperature(float fTmax,float fTmin, float fLatitude=-99.0, int nDay=-99, float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );

float SaturatedVapourPressure(float fTemp);



class CWeather //: public CObject
{
 //DECLARE_SERIAL( CWeather)

public:
	CWeather();           // protected constructor used by dynamic creation
	CWeather(int iValue);
	CWeather(int   nYear,	    int   nMonth,   int   nDay,
		 float fTempMax,    float fTempMin, float fRain,       float fRad=-1,
		 float fRelHumid=-1,float fWind=-1, float fSunshine=-1,float fCloud=-1);

	void Initialization(int   nYear,       int    nMonth,   int   nDay,
			    float fTempMax,    float  fTempMin, float fRain,        float fRad=-1,
			    float fRelHumid=-1,float fWind=-1,  float fSunshine=-1, float fCloud=-1);

	int GetYear()	{return m_nYear;};
	int GetMonth()	{return m_nMonth;};
	int GetDay()	{return m_nDay;};

	float GetMaximumTemperature()	{return m_fTempMax;};
	float GetMinimumTemperature()	{return m_fTempMin;};
	float GetRainfall()	     	{return m_fRainfall;};
	float GetWindSpeed()	     	{return m_fWindSpeed;};
	float GetCloudIndex()	     	{return m_fCloud;};

	float GlobalRadiationDailyTotal(float fLatitude=-99, float a=0.18, float b=0.55);
	float GetSunshineHours  (float fLatitude=-99, float a=0.18, float b=0.55);

	float GetRad()		{return GlobalRadiationDailyTotal();};
	float GetMaxTemp()	{return GetMaximumTemperature();};
	float GetMinTemp()	{return GetMinimumTemperature();};
	float GetRH(float fLatitude = -99.0);
	float GetVP()		{return m_fVaporPressure;};
	float GetRain()		{return GetRainfall();};
	float GetWind()		{return GetWindSpeed();};
	float GetSunshine()     {return GetSunshineHours();};
	float GetCloud()	{return GetCloudIndex();};

// Attributes
public:			//This variables still remain public but will be changed to private
	int		m_nYear;
	int		m_nMonth;
	int		m_nDay;
	float		m_fGlobalRad;
	float		m_fTempMax;
	float		m_fTempMin;
	float		m_fRelHumid;
	float		m_fVaporPressure;
	float		m_fWindSpeed;
	float		m_fRainfall;
	float		m_fEvaporation;
	float		m_fSunHours;
	float		m_fCloud;

public:

	float TemperatureTimeInNight(float fTemp, float fLatitude,float fTsstPrev, float fSstTimePrev);

	float DewPointTemperature();
        float DewPointFromVapourPressureAt0900();
	float RH80TempFromVapourPressureAt0900();

	float TemperatureTimeDuringDayBeforeNoon(float fTemp, float fLatitude);

	float DewPeriodDuringDayBeforeNoon(float fDewPointTemp, float fLatitude);
        float DewPeriodLastNight(float fDewPointTemp, float fLatitude, float fTsstPrev, float fSstTimePrev);
        float DewPeriodDailyTotal(float fDewPointTemp, float fLatitude, float fTsstPrev, float fSstTimePrev);

        float DurationTemperatureBelow(float fTemp, float fLatitude, float fTsstPrev, float fSstTimePrev);

private:
        float DewPointTemperature(float fVapourPressure);

public:
	int  GetDayOfYear();
	int  GetJulianDay();
	int  DateDay(int nDayOfYear);
	int  DateMonth(int nDayOfYear);


	float Daylength  (float fLatitude);
	float Photoperiod(float fLatitude);
	float SunriseTime(float fLatitude);
	float SunsetTime (float fLatitude);

	float DailyGlobalRadiation(float fLatitude, float a=0.18, float b=0.55);
	float DirectRadiationDailyTotal (float fLatitude);
	float DiffuseRadiationDailyTotal(float fLatitude);

	float GlobalRadiation (float fLatitude, float fHour);
	float DirectRadiation (float fLatitude, float fHour);
	float DiffuseRadiation(float fLatitude, float fHour);

	float ExtraTerrestrialRadiation(float fLatitude, float fHour);
	float ExtraTerrestrialRadiationDailyTotal(float fLatitude);

	float SunshineHours(float fLatitude, float a=0.18, float b=0.55);

	float HourlyTemperature(float fHour,float fLatitude=-99.0,float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );

	float TemperatureAtSunrise();
	float TemperatureAtSunset(float fLatitude);


	float ActualVapourPressure();
	float SaturatedVapourPressureAtTemp(float fTemp);
        float SaturatedVapourPressure(float fHour,float fLatitude=-99.0,float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );


	virtual float TemperatureAt0900        (float fLatitude);
	virtual float TemperatureDailyMean     (float fLatitude=-99.0,float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );
	virtual float TemperatureDaytimeMean   (float fLatitude=-99.0,float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );
	virtual float TemperatureNighttimeMean (float fLatitude=-99.0,float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );

	virtual float RelativeHumidityDailyMean();
	virtual float RelativeHumidityDaytimeMean();
	virtual float RelativeHumidityNighttimeMean();



	float RelativeHumidity(float fHour,float fLatitude=-99.0,float fTminPrev=-99.0, float fTmaxPrev=-99.0, float fTminNext=-99.0 );
	float RelativeHumidity(float fTemp);


        float WindSpeedDailyMean();
        float WindSpeedDaytimeMean();
        float WindSpeedNighttimeMean();

        float WindSpeed(float fHour,float fLatitude);
        

	float PotentialEvapotranspiration();


public:
    //	virtual void Serialize(CArchive& ar);   // overridden for document i/o

	// Implementation
public:
	virtual ~CWeather();

};

#endif
