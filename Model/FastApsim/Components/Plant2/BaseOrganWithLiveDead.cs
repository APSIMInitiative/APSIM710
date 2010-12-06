using System;
using System.Collections.Generic;
using System.Text;

public class BaseOrganWithLiveDead : BaseOrgan
   {
   [Ref("Live")] protected Biomass _Live;
   [Ref("Dead")] protected Biomass _Dead;

   public override Biomass Live { get { return _Live; } }
   public override Biomass Dead { get { return _Dead; } }

   }
