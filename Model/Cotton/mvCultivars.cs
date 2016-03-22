using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Linq;
using System.Xml.Linq;
using System.Xml;
using System.Xml.XPath;

namespace ManagedComponent.MvOZCOT

{
    /// <summary>
    ///
    /// Cultivar Parameters as a Class
    ///
    /// </summary>
    public class CultivarParams
    {
        public String name;
        public Double pclint;
        public Double scboll;
        public Double respcon;
        public Double sqcon;
        public Double fcutout;
        public Double flai;
        public Double ddisq;
        public Double tipout;
        public Double[] FRUDD = new Double[9];
        public Double[] BLTME = new Double[9];
        public Double[] WT = new Double[9];
        public Double dlds_max;
        public Double rate_emergence;
        public Double popcon;
        public Double fburr;
        public Double acotyl;
        public Double rlai;
        public Double bckGndRetn;
    }

    /// <summary>
    /// This is a data structure class that holds Cotton Cultivar/Variety values.
    /// Standard varieties do not vary and so are hard coded into this class.
    /// User defined varieties can be defined using XML and included in the init section for the component.
    /// October 2011 DBJ
    /// </summary>
    public class Cultivars 
    {
        //
        // XML string of the default variety list.
        // This string is used as the data store for the cultivar details
        //
       // private static XDocument XDocCultivars;
        private  XDocument XDocCultivars;
        private static string cultivarsXML = @"
<Cultivars>
	
	<cultivar name=""CS50"">
       <percent_l description=""percent lint"">43.0 </percent_l>
       <scboll description=""g seed cotton/boll"">3.8 </scboll>
       <respcon description=""respiration const"">0.01593 </respcon>
       <sqcon description=""governs pot rate of squaring in thermal time"">0.0181 </sqcon>
       <fcutout description=""relates timing of cutout to boll load"">0.5411 </fcutout>
       <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52 </flai>
       <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">402.0 </DDISQ>
       <TIPOUT>52.</TIPOUT>
       <FRUDD description=""dd to change of category (fruit dd)"">
          <val>50.0</val>
          <val>169.0</val>
          <val>329.0</val>
          <val>356.0</val>
          <val>499.0</val>
          <val>642.0</val>
          <val>857.0</val>
          <val>1099.0</val>
       </FRUDD>
       <BLTME description=""0-1 boll time (fraction of period from"">
          <val>0.00</val> 
          <val>0.00</val> 
          <val>0.00</val> 
          <val>0.07</val> 
          <val>0.21</val> 
          <val>0.33</val> 
          <val>0.55</val> 
          <val>1.00</val> 
       </BLTME>
       <WT description=""0-1 weighting of number of bolls"">
          <val>0.0104</val> 
          <val>0.0272</val> 
          <val>0.1441</val> 
          <val>0.0988</val> 
          <val>0.5042</val> 
          <val>0.9617</val> 
          <val>1.0000</val> 
          <val>0.5785</val> 
       </WT>
       <dlds_max description=""not used"">0.12</dlds_max>
       <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
       <popcon>0.03633</popcon>
       <fburr description=""factor sc/boll to sc+burr/boll"">1.23 </fburr>
       <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
       <RLAI>0.010</RLAI>
       <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>

    <cultivar name=""CS6S"">
      <percent_l description=""percent lint"">43.0 </percent_l>
      <scboll description=""g seed cotton/boll"">4.5 </scboll>
      <respcon description=""respiration const"">0.01593 </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0221 </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411 </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52 </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">391.0 </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>  
         <val>176.</val> 
         <val>324.</val>  
         <val>371.</val>  
         <val>515.</val>  
         <val>659.</val>  
         <val>876.</val> 
         <val>1105.</val>   
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val>
         <val>0.0272</val>
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""CS8S"">
      <percent_l description=""percent lint"">43.0 </percent_l>
      <scboll description=""g seed cotton/boll"">4.5 </scboll>
      <respcon description=""respiration const"">0.01593 </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0230 </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411 </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52 </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">383.0 </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>176.</val>   
         <val>341.</val>   
         <val>371.</val>   
         <val>515.</val>   
         <val>659.</val>   
         <val>876.</val>  
         <val>1096.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>

    <cultivar  name=""DP16"">
      <percent_l description=""percent lint"">39.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.0        </scboll>
      <respcon description=""respiration const"">0.02500      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0210         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">420.0         </DDISQ>
      <TIPOUT>78.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>
         <val>180.</val> 
         <val>350.</val> 
         <val>380.</val> 
         <val>520.</val> 
         <val>660.</val> 
         <val>870.</val> 
         <val>1100.</val> 
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""DP61"">
      <percent_l description=""percent lint"">39.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.0        </scboll>
      <respcon description=""respiration const"">0.02500      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0210         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">420.0         </DDISQ>
      <TIPOUT>78.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>180.</val>   
         <val>350.</val>   
         <val>380.</val>   
         <val>520.</val>   
         <val>660.</val>   
         <val>870.</val>  
         <val>1100.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""DP90"">
      <percent_l description=""percent lint"">39.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.7        </scboll>
      <respcon description=""respiration const"">0.02306      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0195         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">408.0         </DDISQ>
      <TIPOUT>78.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>180.</val>   
         <val>344.</val>   
         <val>380.</val>   
         <val>520.</val>   
         <val>660.</val>   
         <val>870.</val>  
         <val>1126.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""Empr"">
      <percent_l description=""percent lint"">38.0     </percent_l>
      <scboll description=""g seed cotton/boll"">7.0        </scboll>
      <respcon description=""respiration const"">0.02500      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0210         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4800       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">320.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>180.</val>   
         <val>350.</val>   
         <val>380.</val>   
         <val>520.</val>   
         <val>660.</val>   
         <val>870.</val>  
         <val>1100.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>      
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""Kwam"">
      <percent_l description=""percent lint"">35.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.0        </scboll>
      <respcon description=""respiration const"">0.02306      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0206         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">420.0         </DDISQ>
      <TIPOUT>78.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>180.</val>   
         <val>350.</val>   
         <val>380.</val>   
         <val>520.</val>   
         <val>660.</val>   
         <val>870.</val>  
         <val>1100.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>  
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""L22"">
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.7        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0197         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">418.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>160.</val>   
         <val>333.</val>   
         <val>335.</val>   
         <val>481.</val>   
         <val>627.</val>   
         <val>845.</val>  
         <val>1115.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""L23"">
      <percent_l description=""percent lint"">43.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.7        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0197         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">418.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>160.</val>   
         <val>333.</val>   
         <val>335.</val>   
         <val>481.</val>   
         <val>627.</val>   
         <val>845.</val>  
         <val>1115.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>   
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>      
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""S101"">
      <percent_l description=""percent lint"">43.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.3        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0237         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">402.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>152.</val>   
         <val>285.</val>   
         <val>315.</val>   
         <val>457.</val>   
         <val>599.</val>   
         <val>812.</val>  
         <val>1030.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""S324"">
      <percent_l description=""percent lint"">43.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.1        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0237         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">406.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>152.</val>   
         <val>338.</val>   
         <val>315.</val>   
         <val>457.</val>   
         <val>599.</val>   
         <val>812.</val>  
         <val>1089.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""Sc34"">
      <percent_l description=""percent lint"">41.0     </percent_l>
      <scboll description=""g seed cotton/boll"">3.9        </scboll>
      <respcon description=""respiration const"">0.02306      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0227         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">410.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>155.</val>   
         <val>337.</val>   
         <val>323.</val>   
         <val>468.</val>   
         <val>613.</val>   
         <val>831.</val>  
         <val>1107.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""Sica""> 
    	<!--  Generic  -->
      <percent_l description=""percent lint"">40.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.5        </scboll>
      <respcon description=""respiration const"">0.02306      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0206         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">420.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>180.</val>   
         <val>350.</val>   
         <val>380.</val>   
         <val>520.</val>   
         <val>660.</val>   
         <val>870.</val>  
         <val>1100.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>      
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""ScV1"">
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.2        </scboll>
      <respcon description=""respiration const"">0.02306      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0217         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">426.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>155.</val>   
         <val>315.</val>   
         <val>323.</val>   
         <val>468.</val>   
         <val>613.</val>   
         <val>831.</val>  
         <val>1099.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""ScV2"">
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.2        </scboll>
      <respcon description=""respiration const"">0.02306      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0217         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.4789       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.87          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">426.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>155.</val>   
         <val>315.</val>   
         <val>323.</val>   
         <val>468.</val>   
         <val>613.</val>   
         <val>831.</val>  
         <val>1099.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""S189"">
      <!--   Sicot   -->
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.7        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0217         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">402.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>161.</val>   
         <val>307.</val>   
         <val>338.</val>   
         <val>484.</val>   
         <val>630.</val>   
         <val>848.</val>  
         <val>1071.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""Si14"">
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.5        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0219         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">420.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>172.</val>   
         <val>331.</val>   
         <val>362.</val>   
         <val>509.</val>   
         <val>655.</val>   
         <val>876.</val>  
         <val>1100.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""Siok"">
      <!--   Generic   -->
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.5        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0228         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">420.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>180.</val>   
         <val>350.</val>   
         <val>380.</val>   
         <val>520.</val>   
         <val>660.</val>   
         <val>870.</val>  
         <val>1100.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""V15"">
      <percent_l description=""percent lint"">41.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.3        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0219         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">435.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>159.</val>   
         <val>302.</val>   
         <val>333.</val>   
         <val>478.</val>   
         <val>624.</val>   
         <val>843.</val>  
         <val>1066.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80 </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""S71"">
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.2        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0217         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">402.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>161.</val>   
         <val>307.</val>   
         <val>338.</val>   
         <val>484.</val>   
         <val>630.</val>   
         <val>848.</val>  
         <val>1071.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>  
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.80              </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""S71BR"">
      <!--   Sicot 71BR   (Bollgard II  Roundup Ready)  -->
      <percent_l description=""percent lint"">40.0     </percent_l>
      <scboll description=""g seed cotton/boll"">5.0        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0217         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">402.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>161.</val>   
         <val>307.</val>   
         <val>338.</val>   
         <val>484.</val>   
         <val>630.</val>   
         <val>848.</val>  
         <val>1071.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.90              </BckGndRetn>
    </cultivar>
    
    <cultivar  name=""S289BR"">
      <!--   Sicot 289BR   (Bollgard II  Roundup Ready)   -->
      <percent_l description=""percent lint"">42.0     </percent_l>
      <scboll description=""g seed cotton/boll"">4.5        </scboll>
      <respcon description=""respiration const"">0.01593      </respcon>
      <sqcon description=""governs pot rate of squaring in thermal time"">0.0217         </sqcon>
      <fcutout description=""relates timing of cutout to boll load"">0.5411       </fcutout>
      <flai description=""modifying ratio of leaf area per site for this cultivar"">0.52          </flai>
      <DDISQ description=""Constable (pers. comm. 1983), 10=Empire"">402.0         </DDISQ>
      <TIPOUT>52.</TIPOUT>
      <!--  development of individual boll
         flowering                     open bolls
         1       2    3     4     5     6     7     8     9   ! related to likelihood to demage by heliothi
         -->
      <FRUDD description=""dd to change of category (fruit dd)"">
         <val>50.</val>   
         <val>161.</val>   
         <val>307.</val>   
         <val>338.</val>   
         <val>484.</val>   
         <val>630.</val>   
         <val>848.</val>  
         <val>1071.</val>    
      </FRUDD>
      <BLTME description=""0-1   boll time (fraction of period from"">
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.00</val>   
         <val>0.07</val>   
         <val>0.21</val>   
         <val>0.33</val>   
         <val>0.55</val>   
         <val>1.00</val>    
      </BLTME>
      <WT description=""0-1   weighting of number of bolls"">
         <val>0.0104</val> 
         <val>0.0272</val> 
         <val>0.1441</val> 
         <val>0.0988</val> 
         <val>0.5042</val> 
         <val>0.9617</val> 
         <val>1.0000</val> 
         <val>0.5785</val>       
      </WT>
      <dlds_max description=""not used"">0.12</dlds_max>
      <rate_emergence units=""mm/dd"" description=""Rate of emergence"">1.00</rate_emergence>
      <popcon>0.03633</popcon>
      <fburr description=""factor sc/boll to sc+burr/boll"">1.23              </fburr>
      <ACOTYL units=""mm2"" description=""area of cotyledons"">525.0</ACOTYL>
      <RLAI>0.010</RLAI>
      <BckGndRetn description=""background retention of fruit"">0.90              </BckGndRetn>
    </cultivar>

</Cultivars>
                                       ";   // end of cultivar XML definition string
        
        
        // Constructor
        public Cultivars()
        {
            //
            //  Create XDocument from xml string
            //
            XDocCultivars = XDocument.Parse(cultivarsXML);
        }

        //
        // Method to return a Cultivar by name
        //
        public CultivarParams getCultivar(string cultivarName)
        {
            //
            // find the cultivar by name and assign it to the returnValue
            //
            CultivarParams returnValue = new CultivarParams();

            // XmlDocument doc = new XmlDocument();
            // doc.LoadXml(cultivarsXML);
            // XElement xmlCultivars = XElement.Load(cultivarsXML);

            //XDocument Cultivars = XDocument.Parse(CultivarsXML);
            //XPathNavigator nav = ((XNode)xmlCultivars).CreateNavigator();
            foreach (XElement cultivar in XDocCultivars.Descendants("cultivar"))   //search only Elements that are called <cultivar>
            {
                //Console.WriteLine("Cultivar: name = {0}", cultivar.Attribute("name").Value.ToUpper());
                if (cultivar.Attribute("name").Value.ToUpper() == cultivarName.ToUpper())
                {
                    // Found the required Cultivar
                    returnValue = getCultivarDtlsFromXMLElement(cultivar);
                }
            }
            return returnValue;
        }

        // 
        // Extract CultivarParams from selected cultivar element in the xml document
        //
        private CultivarParams getCultivarDtlsFromXMLElement(XElement cultivar)
        {
            
            int i = 0;
            CultivarParams returnValue = new CultivarParams();
          
            try
                {
                returnValue.name = cultivar.Attribute("name").Value.ToUpper();
                returnValue.pclint = Convert.ToDouble(cultivar.Element("percent_l").Value);
                returnValue.scboll = Convert.ToDouble(cultivar.Element("scboll").Value);
                returnValue.respcon = Convert.ToDouble(cultivar.Element("respcon").Value);
                returnValue.sqcon = Convert.ToDouble(cultivar.Element("sqcon").Value);
                returnValue.fcutout = Convert.ToDouble(cultivar.Element("fcutout").Value);
                returnValue.flai = Convert.ToDouble(cultivar.Element("flai").Value);
                returnValue.ddisq = Convert.ToDouble(cultivar.Element("DDISQ").Value);
                returnValue.tipout = Convert.ToDouble(cultivar.Element("TIPOUT").Value);

                //FRUDD array
                XElement frudd = cultivar.Element("FRUDD");
                i = 0;
                foreach (XElement fruddVal in frudd.Elements())
                {
                    i++;
                    returnValue.FRUDD[i] = Convert.ToDouble(fruddVal.Value);
                }

                //BLTME array
                XElement bltme = cultivar.Element("BLTME");
                i = 0;
                foreach (XElement bltmeVal in bltme.Elements())
                {
                    i++;
                    returnValue.BLTME[i] = Convert.ToDouble(bltmeVal.Value);
                }

                //WT array
                XElement wt = cultivar.Element("WT");
                i = 0;
                foreach (XElement wtVal in wt.Elements())
                {
                    i++;
                    returnValue.WT[i] = Convert.ToDouble(wtVal.Value);
                }

                returnValue.dlds_max = Convert.ToDouble(cultivar.Element("dlds_max").Value);
                returnValue.rate_emergence = Convert.ToDouble(cultivar.Element("rate_emergence").Value);
                returnValue.popcon = Convert.ToDouble(cultivar.Element("popcon").Value);
                returnValue.fburr = Convert.ToDouble(cultivar.Element("fburr").Value);
                returnValue.acotyl = Convert.ToDouble(cultivar.Element("ACOTYL").Value);
                returnValue.rlai = Convert.ToDouble(cultivar.Element("RLAI").Value);
                returnValue.bckGndRetn = Convert.ToDouble(cultivar.Element("BckGndRetn").Value);
            }
            catch (Exception e)
            {
                Console.WriteLine("Get Cultivar Details Error " + e.Message);
            }
            return returnValue;
        }
        
        
        //
        // List of Cultivars and their varietal parameters
        //
        //public void cultivarsTest()
        //{
        //   List<CultivarParams> cultivarList = new List<CultivarParams>();
            
        //   // CultivarParams cultDtls = new CultivarParams();
        //   // cultDtls.name = "DP16";
        //   // cultivarList.Add(cultDtls);
        //}


        //
        // Find a Cultivar by name
        //
        public bool cultivarExists(string searchName)
        {
            bool success = false;

            // XDocCultivars is the XDocument 'list' of all cultivars and their parameters
            foreach (XElement cultivar in XDocCultivars.Descendants("cultivar"))
            {
                string cultivarName = cultivar.Attribute("name").Value;
                if (cultivarName.ToUpper() == searchName.ToUpper())
                {
                    success = true;
                    break;
                }
            }
            return success;
        }


        ///
        /// Public function to update Cultivar List
        /// 

        public void updateCultivarList(CultivarParams[] initCultivarList)
        {
            // For each cultivar in the init list
            //  if it is new, then add it
            //  otherwise check each value and update where necessary
            //  TODO: DBJ  check how to specify and update only single cultivar parameters ??
            //
            foreach (CultivarParams initCultivar in initCultivarList)
            {
                if (cultivarExists(initCultivar.name))
                {
                    try
                    {
                        updateCultivarDetails(initCultivar);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("Update of cotton Cultivar details failed. Error: " + e.Message);
                    }
                }
                else // new cultivar
                {
                    try
                    {
                        addCultivar(initCultivar);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("Addition of new cotton Cultivar failed. Error: " + e.Message);
                    }
                }
            }

        }


        ///
        /// Private function to add a new cultivar to the Cultivar List
        /// 

        private void addCultivar(CultivarParams newCultivarParams)
        {
            // Add a cultivar that has been identified as not in the cultivar list
             
            // Create FRUDD, BLTME and WT nodes first
            // Note: element [0] is not used, array values start at 1
            XElement fruddNode = new XElement("FRUDD");
            for (uint i = 1; i < newCultivarParams.FRUDD.Count(); i++)
            {
                fruddNode.Add(new XElement("val", newCultivarParams.FRUDD[i]));
            }

            XElement bltmeNode = new XElement("BLTME");
            for (uint i = 1; i < newCultivarParams.BLTME.Count(); i++)
            {
                bltmeNode.Add(new XElement("val", newCultivarParams.BLTME[i]));
            }

            XElement wtNode = new XElement("WT");
            for (uint i = 1; i < newCultivarParams.WT.Count(); i++)
            {
                wtNode.Add(new XElement("val", newCultivarParams.WT[i]));
            }

            
            XElement newCultivar = new XElement("cultivar", new XAttribute("name",newCultivarParams.name),
                                                new XElement("percent_l", newCultivarParams.pclint),
                                                new XElement("scboll", newCultivarParams.scboll),
                                                new XElement("respcon", newCultivarParams.respcon),
                                                new XElement("sqcon", newCultivarParams.sqcon),
                                                new XElement("fcutout", newCultivarParams.fcutout),
                                                new XElement("flai", newCultivarParams.flai),
                                                new XElement("DDISQ", newCultivarParams.ddisq),
                                                new XElement("TIPOUT", newCultivarParams.tipout),
                                                fruddNode,
                                                bltmeNode,
                                                wtNode,
                                                new XElement("dlds_max", newCultivarParams.dlds_max),
                                                new XElement("rate_emergence", newCultivarParams.rate_emergence),
                                                new XElement("popcon", newCultivarParams.popcon),
                                                new XElement("fburr", newCultivarParams.fburr),
                                                new XElement("ACOTYL", newCultivarParams.acotyl),
                                                new XElement("RLAI", newCultivarParams.rlai),
                                                new XElement("BckGndRetn", newCultivarParams.bckGndRetn)
                                                );

            XDocCultivars.Root.AddFirst(newCultivar);

            //Note: no need to 'save' results as this has directly updated the STATIC XDocument XDocCultivars
            //       so all reads of this XDocument will reflect the changes.


        }


        ///
        /// Private function to update an existing cultivar in the Cultivar List
        /// 

        private void updateCultivarDetails(CultivarParams modCultivarParams)
        {
            // Modify a cultivar that has been identified as being in the cultivar list
            uint i = 0;

            // Locate the cultivar element
            XElement modCultivar = (XElement)XDocCultivars.FirstNode;
            bool found = false;

            foreach (XElement cultivar in XDocCultivars.Descendants("cultivar"))
            {
                if (cultivar.Attribute("name").Value.ToUpper() == modCultivarParams.name.ToUpper())
                {
                    modCultivar = cultivar;
                    found = true;
                    break;
                }
            }

            // Update each field with the init value
            if (found)
            {
                if (modCultivarParams.pclint != 0.0) modCultivar.Element("percent_l").SetValue(modCultivarParams.pclint);
                if (modCultivarParams.scboll != 0.0) modCultivar.Element("scboll").SetValue(modCultivarParams.scboll);
                if (modCultivarParams.respcon != 0.0) modCultivar.Element("respcon").SetValue(modCultivarParams.respcon);
                if (modCultivarParams.sqcon != 0.0) modCultivar.Element("sqcon").SetValue(modCultivarParams.sqcon);
                if (modCultivarParams.fcutout != 0.0) modCultivar.Element("fcutout").SetValue(modCultivarParams.fcutout);
                if (modCultivarParams.flai != 0.0) modCultivar.Element("flai").SetValue(modCultivarParams.flai);
                if (modCultivarParams.ddisq != 0.0) modCultivar.Element("DDISQ").SetValue(modCultivarParams.ddisq);
                if (modCultivarParams.tipout != 0.0) modCultivar.Element("TIPOUT").SetValue(modCultivarParams.tipout);

                i = 1;
                foreach(XElement fruddVal in modCultivar.Element("FRUDD").Descendants("val"))
                {
                    fruddVal.SetValue(modCultivarParams.FRUDD[i]);
                    i++;
                }

                i = 1;
                foreach(XElement bltmeVal in modCultivar.Element("BLTME").Descendants("val"))
                {
                    bltmeVal.SetValue(modCultivarParams.BLTME[i]);
                    i++;
                }

                i = 1;
                foreach(XElement wtVal in modCultivar.Element("WT").Descendants("val"))
                {
                    wtVal.SetValue(modCultivarParams.WT[i]);
                    i++;
                }

                if (modCultivarParams.dlds_max != 0.0) modCultivar.Element("dlds_max").SetValue(modCultivarParams.dlds_max);
                if (modCultivarParams.rate_emergence != 0.0) modCultivar.Element("rate_emergence").SetValue(modCultivarParams.rate_emergence);
                if (modCultivarParams.popcon != 0.0) modCultivar.Element("popcon").SetValue(modCultivarParams.popcon);
                if (modCultivarParams.fburr != 0.0) modCultivar.Element("fburr").SetValue(modCultivarParams.fburr);
                if (modCultivarParams.acotyl != 0.0) modCultivar.Element("ACOTYL").SetValue(modCultivarParams.acotyl);
                if (modCultivarParams.rlai != 0.0) modCultivar.Element("RLAI").SetValue(modCultivarParams.rlai);
                if (modCultivarParams.bckGndRetn != 0.0) modCultivar.Element("BckGndRetn").SetValue(modCultivarParams.bckGndRetn);

                //Note: no need to save results as this has directly updated the STATIC XDocument XDocCultivars
                //       so all reads of this XDocument will reflect the changes.

            } // end of 'if found'


        }   // end of updateCultivarDetails
    
    
    
    
    }  // end of public class Cultivars
}  //namespace
 