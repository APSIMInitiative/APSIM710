!      include 'apswim.inc'

C     Last change:  DSG  15 Jun 2000    3:40 pm
* =====================================================================
      subroutine apswim_gsurf(deqrain,surfcon)
* =====================================================================
*     Short Description:
*     gets soil surface conductance, surfcon
*

      implicit none

*     Global Variables


*     Subroutine Arguments
      double precision deqrain
      double precision surfcon

*     Internal Variables
      double precision decay_fraction
      double precision ceqrain

*     Constant Values
*     none
*
      ! Ideally, if timesteps are small we could just use
      ! dgsurf/dEqr = -1/grc x (gsurf - g0)
      ! but because this is really just a linear approximation of the
      ! curve for longer timesteps we had better be explicit and
      ! calculate the difference from the exponential decay curve.

      if (p%grc.ne.0) then
         ! first calculate the amount of Energy that must have been
         ! applied to reach the current conductance.

         decay_Fraction = ddivide(g%gsurf-p%g0,p%g1-p%g0,0d0)

         if (doubles_are_equal (decay_fraction, 0d0)) then
            ! seal is totally decayed
            surfcon = p%g0
         else

            ceqrain = -p%grc * log(decay_Fraction)

            ! now add rainfall energy for this timestep
            if (c%cover_effects.eq.'on') then
               ceqrain = ceqrain + deqrain * (1d0 - g%residue_cover)
            else
               ceqrain = ceqrain + deqrain
            endif

            ! now calculate new surface storage from new energy
            surfcon = p%g0+(p%g1-p%g0)*exp(-ceqrain/p%grc)
         endif
      else
         surfcon = g%gsurf
      endif

      return
      end subroutine
* =====================================================================
      subroutine apswim_solve(itlim,fail)
* =====================================================================
*     Short description:
*     solves for this time step
*

      implicit none

*     Global Variables


*     Subroutine Arguments
      integer itlim           !(input) limit for no. of iterations
      logical fail            !(output)fail flag

*     Internal Variables

      double precision a(-1:M)
      double precision aerr
      double precision b(-1:M)
      double precision balerr
      double precision c_(-1:M)         ! Added Underscore to make name unique
      double precision d(-1:M)
      double precision err
      integer          i
      integer          i1
      integer          i2
      integer          i0
      integer          ibpf
      integer          iroots
      integer          it
      integer          j
      integer          neq
      integer          solnum
      double precision rhs(-1:M)
      double precision dp(-1:M)
      double precision vbp(-1:M)
      double precision wpold

*     Constant Values
*     none

      it=0
      wpold=g%wp
      iroots=0
*     loop until solved or too many iterations or Thomas algorithm fails
10    continue
         it=it+1
*        get balance eqns
         call apswim_baleq(it,iroots,c%slos,g%csl,i1,i2,a,b,c_
     :                    ,rhs)
*        test for convergence to soln
cnh hey - wpf has no arguments !
cnh         g%wp=wpf(p%n,p%dx,g%th)
         g%wp = apswim_wpf()

         balerr=g%ron-g%roff-g%q(p%n+1)-g%rex-g%res+g%rssf-
     1          (g%h-g%hold+g%wp-wpold)/g%dt
         err=0.
         do 20 i=i1,i2
            aerr=abs(rhs(i))
            if(err.lt.aerr)err=aerr
20       continue
*        switch off iteration for root extraction if err small enough
         if(err.lt.c%errex*g%rex.and.iroots.eq.0)iroots=1
         if(abs(balerr).lt.c%ersoil.and.err.lt.c%ernode)then
            fail=.FALSE.
         else
            neq=i2-i1+1
            call apswim_thomas(neq,a(i1),b(i1),c_(i1),rhs(i1)
     :                        ,g%qbpd,d(i1),dp(i1),vbp,fail)
            g%work=g%work+neq
cnh            if(fail)go to 90
            if(fail) then
cnh               call warning_error(Err_internal,
cnh     :            'swim will reduce timestep to solve water movement')
                  call Write_string (
     :      'swim will reduce timestep to avoid error in water balance')
               goto 90
            endif

            fail=.TRUE.
*           limit step size_of for soil nodes
            i0=max(i1,0)
            do 30 i=i0,i2
               if(dp(i).gt.c%dppl)dp(i)=c%dppl
               if(dp(i).lt.-c%dpnl)dp(i)=-c%dpnl
30          continue
*           update solution
            j=i0
            do 40 i=i0,i2
               g%p(j)=g%p(j)+dp(i)
               if(j.gt.0.and.j.lt.p%n-1)then
                  if(doubles_are_equal(p%x(j),p%x(j+1)))then
                     j=j+1
                     g%p(j)=g%p(j-1)
                  end if
               end if
               j=j+1
40          continue
            if(i1.eq.-1)g%h=max(0d0,g%h+dp(-1))
         end if
      if(fail.and.it.lt.itlim)go to 10

      if (fail) then
         call Write_string (
     :   'Maximum iterations reached - swim will reduce timestep')
         goto 90
      endif

*     solve for solute movement

      do 80 solnum = 1,p%num_solutes
         call apswim_getsol
     :          (solnum,a(0),b(0),c_(0),d(0),rhs(0),dp(0),vbp(0),fail)
         If (fail) then
            call Write_string (
     :         'swim will reduce timestep to solve solute movement')

            goto 85
         endif
   80 continue
   85 continue
cnh

90    continue
      end subroutine
* =====================================================================
      subroutine apswim_pstat (istat,tresp)
* =====================================================================
*     Short Description:
*     gets potl evap. for soil and veg., and root length densities
*
*     g%resp,p%slupf and g%csl were renamed to tslupf,trep,tcsl as there were
*     already variables with those names in common


      implicit none

*     Subroutine Arguments
      integer istat
      double precision tresp

*     Internal Variables
      integer          i
      integer          iveg
      integer          j
      double precision rep
      double precision rldi
      double precision sep          ! soil evaporation demand
      integer          solnum
      double precision start_of_day
      double precision end_of_day
      double precision TD_Eo

*     Constant Values

      double precision pi
      parameter (pi=3.141593d0)
*
      if(istat.eq.0)then
*        calc. potl evap.
         rep=(apswim_cevap(g%t)-apswim_cevap(g%t-g%dt))/g%dt
         if (c%cover_effects.eq.'on') then
            !Use Soilwat cover effects on evaporation.
            sep = rep*g%dt * apswim_cover_eos_redn()
         else
            sep = rep*g%dt * (1d0 - g%crop_cover)
         endif

         ! Note: g%pep is passed to swim as total ep for a plant for the
         ! entire apsim timestep. so rate will be (CEp = cum EP)
         !   dCEp   Total daily EP     dEo
         !   ---- = -------------- p%x --------
         !    g%dt    Total daily Eo      g%dt

         start_of_day = apswim_time (g%year,g%day,
     :                               apswim_time_to_mins(g%apsim_time))
         end_of_day = apswim_time (g%year
     :                            ,g%day
     :                            ,apswim_time_to_mins(g%apsim_time)
     :                                +int(g%apsim_timestep))

         TD_Eo = apswim_cevap(end_of_day)-apswim_cevap(start_of_day)

         do 50 j=1,g%nveg
            g%rtp(j) = ddivide(g%pep(j),TD_Eo, 0d0)*rep
50       continue

         ! pot soil evap rate is not linked to apsim timestep
         tresp = sep/g%dt

         do 60 iveg=1,g%nveg
            do 60 i=0,p%n
               if(g%rld(i,iveg).lt.1d-20)g%rld(i,iveg)=1d-20
               rldi=g%rld(i,iveg)

               g%rc(i,iveg)=-log(pi*g%root_radius(i,iveg)**2*rldi)
     :                        /(4.*pi*rldi*p%dx(i))

60        continue

      else if(istat.eq.1)then
*        update cumulative transpiration
         do 70 i=1,g%nveg
            g%ctp(i)=g%ctp(i)+g%rtp(i)*g%dt
            g%ct(i)=g%ct(i)+g%rt(i)*g%dt
cnh
            do 65 j=0,p%n
               g%pwuptake(i,j) = g%pwuptake(i,j) + g%qr(j,i)*g%dt*10d0
                                             ! cm -> mm __/
               g%pwuptakepot(i,j)=g%pwuptakepot(i,j)
     :                           +g%qrpot(j,i)*g%dt*10d0
                                        ! cm -> mm __/
65          continue
70       continue

      else if(istat.eq.2)then
*        update cumulative solute uptake

         do 90 i=1,g%nveg
            do 80 j=0,p%n
               do 95 solnum=1,p%num_solutes
                  g%slup(i,solnum)=g%slup(i,solnum)
     :                +apswim_slupf(i,solnum)*g%csl(solnum,j)*g%qr(j,i)
cnh     :                      +p%slupf(solnum)*g%csl(solnum,j)*g%qr(j,i)
                  ! I thought g%qr was a rate ----------------/

                  g%psuptake(solnum,i,j) = g%psuptake (solnum,i,j) +
     :              apswim_slupf(i,solnum)*g%csl(solnum,j)
     :              *g%qr(j,i)/10d0*g%dt
cnh     :                   p%slupf(solnum)*g%csl(solnum,j)*g%qr(j,i)/10d0*g%dt
95             continue                                   !   /
                                                          ! ppm -> kg/ha
c        this doesn't make sense....g%csl has already been changed from it
c        was at the start of the timestep.  need to find a different way
c        of calculating it.  what about qsl???
c        or try getting g%csl at start of timestep.
c        BUT NUMBERS DO ADD UP OK????? does he then update at start of next
c        timestep??????? !!


80          continue
90       continue

      end if

      return
      end subroutine
* =====================================================================
      subroutine apswim_baleq
     :       (it,iroots,tslos,tcsl,ibegin,iend,a,b,c_,rhs)
* =====================================================================
*     Short Description:
*     gets coefficient matrix and rhs for Newton soln of balance eqns
*
*     Some variables had the same name as some global variables and so
*     these were renamed (by prefixing with g%t - for temp)
*     this include p%isol, g%csl, p%slos


      implicit none

*     Global Variables


*     Subroutine Argruments
      integer it                !(input) iteration no.
      integer iroots            !(input) root extraction flag
      double precision tslos(nsol)    !(input) osmotic pressure per unit solute
      double precision tcsl(nsol,0:p%n) !(input) solute concns
      integer ibegin            !(output) position of first equation
      integer iend              !(output) position of last equation
      double precision a(-1:p%n),b(-1:p%n),c_(-1:p%n),rhs(-1:p%n)
*     output: coeff. arrays and rhs for Newton eqns

*     Internal Variables
      double precision accept
      double precision absgf
      double precision deltax
      double precision deltap
      double precision hkd2
      double precision hkdp1
      double precision hkdp2
      double precision psip(0:M)
      double precision psipp(0:M)
      double precision thp(0:M)
      double precision hkp(0:M)
      double precision qsp(0:M)
      double precision qexp(3,0:M)
      double precision qp1(0:M+1)
      double precision qp2(0:M+1)
      double precision psios(0:M)
      double precision g_
      double precision gh
      double precision gr
      double precision hkd1
      double precision hsoil
      integer          i
      integer          i1
      integer          ifirst
      integer          ilast
      integer          j
      integer          k
      double precision q0
      double precision qbpp
      double precision qbps
      double precision qbpsp
      double precision respsi
      double precision roffd
      double precision skd
      integer          solnum
      double precision v1
      double precision value
      double precision w1
      double precision w2
      double precision wt
      logical          xidif
      logical          xipdif
      character        string*300
      double precision gfhkp
      double precision qdrain(0:M),qdrainpsi(0:M),qssofp(0:M)

      double precision headdiff ! head difference between water table and
                                ! potential at bottom of profile (cm)

      save ifirst,ilast,gr

*     Constant Values
      double precision hcon
      parameter (hcon=7.0e-7)

      double precision hair
      parameter (hair=0.5)

cnh - added initialisation to zero to eliminate ndp errors
      do 1 i=0,M
         psip(i)=0.d0
         psipp(i)=0.d0
         thp(i)=0.d0
         hkp(i)=0.d0
         qsp(i)=0.d0
         do 2 j=1,3
            qexp(j,i)=0.d0
    2    continue
         qp1(i)=0.d0
         qp2(i)=0.d0
         psios(i)=0.d0

    1 continue
      qp1(M+1) = 0.d0
      qp2(M+1) = 0.d0

cnh - end subroutine

*
***   initialise for first iteration
      if(it.eq.1)then
         ifirst=0
         ilast=p%n
         if(p%itbc.eq.2.and.g%hold.gt.0.)ifirst=-1
         if (p%ibbc.eq.0)gr = p%bbc_value

         if(p%ibbc.eq.1)then
            g%psi(p%n) = p%bbc_value
            g%p(p%n)=apswim_pf(g%psi(p%n))
         end if

      end if
***   get soil water variables and their derivatives
      do 8 i=0,p%n
         call apswim_watvar(i,g%p(i),g%psi(i),psip(i),psipp(i),g%th(i)
     :                     ,thp(i),g%hk(i),hkp(i))
8     continue
***   check boundary potls
      if(p%itbc.eq.0.and.p%isbc.eq.0.and.g%psi(0).gt.0.)then
*        infinite conductance and no ponding allowed
         g%psi(0)=0.
         g%p(0)=apswim_pf(g%psi(0))
         call apswim_watvar(0,g%p(0),v1,psip(0),psipp(0),g%th(0),thp(0),
     1               g%hk(0),hkp(0))
      end if
      if(p%ibbc.eq.3.and.g%psi(p%n).gt.p%bbc_value)then
*        seepage at bottom boundary
         g%psi(p%n)=p%bbc_value
         g%p(p%n)=apswim_pf(g%psi(p%n))
         call apswim_watvar(p%n,g%p(p%n),v1,psip(p%n),psipp(p%n)
     :                     ,g%th(p%n),thp(p%n),g%hk(p%n),hkp(p%n))
      end if
***   get fluxes between nodes
      absgf=abs(g%gf)
      do 10 i=1,p%n
         if(.not.Doubles_are_equal(p%x(i-1),p%x(i)))then
            deltax=p%x(i)-p%x(i-1)
            deltap=g%p(i)-g%p(i-1)
            hkd1=g%hk(i-1)*psip(i-1)
            hkd2=g%hk(i)*psip(i)
            hkdp1=g%hk(i-1)*psipp(i-1)+hkp(i-1)*psip(i-1)
            hkdp2=g%hk(i)*psipp(i)+hkp(i)*psip(i)
            skd=hkd1+hkd2
            if(p%swt.ge..5.and.p%swt.le.1.)then
*              use fixed space weighting on gravity flow
               w1=sign(2.*p%swt,g%gf)
            else
*              use central diffs for gravity flow if possible, else use
*                 just enough upstream weighting to avoid instability
*                 user may increase acceptable level for central diffs
*                 by setting p%swt < -1
               accept=max(1d0,-p%swt)
               wt=0.
c               if(absgf.ne.0..and.hkp(i).ne.0.)then
               gfhkp = g%gf*hkp(i)
               if(gfhkp.ne.0.)then
                  if(it.eq.1)then
c                     value=1.-accept*(skd+(g%p(i)-g%p(i-1))*hkdp2)/
c     1                   (absgf*deltax*hkp(i))

                     value=1.-accept*(skd)/(abs(gfhkp)*deltax)
c                     value=min(1d0,value)
                     g%swta(i)=sign(max(0d0,value),gfhkp)
                  end if
                  wt=g%swta(i)
               end if

               w1=1.+wt
            end if
            w2=2.-w1

            if ((w1.gt.2.0).or.(w1.lt.0.0)) then
               call warning_error(Err_Internal
     :                           ,'bad space weighting factor')
            endif

            g%q(i)=-0.5*(skd*deltap/deltax-g%gf*(w1*g%hk(i-1)
     :                   +w2*g%hk(i)))
            qp1(i)=-0.5*((hkdp1*deltap-skd)/deltax-g%gf*w1*hkp(i-1))
            qp2(i)=-0.5*((hkdp2*deltap+skd)/deltax-g%gf*w2*hkp(i))

            g%swf(i)= w1
         end if
10    continue
***   get fluxes to storage
      do 20 i=0,p%n
         g%qs(i)=(g%th(i)-g%thold(i))*p%dx(i)/g%dt
         qsp(i)=thp(i)*p%dx(i)/g%dt
20    continue
***   get uptake fluxes to roots if still in iterations
      if(iroots.lt.2)then
         do 23 i=0,p%n
            psios(i) = g%psi(i)
            do 22 solnum=1,nsol
               psios(i)=psios(i)-tslos(solnum)*tcsl(solnum,i)
   22       continue
   23    continue
         call apswim_uptake(psios,g%hk,psip,hkp,g%qex,qexp)
      end if
      g%rex=0.
      do 25 i=0,p%n
25    g%rex=g%rex+g%qex(i)

***   NIH  get subsurface fluxes
      call apswim_drain(qdrain,qdrainpsi)

      g%rssf = 0.
      do 26 i=0,p%n
         g%qssif(i) = g%SubSurfaceInFlow(i)/10./24d0 ! assumes mm and daily timestep - need something better !!!!
         g%qssof(i) = qdrain(i) ! Add outflow calc here later
         qssofp(i) = qdrainpsi(i) * psip(i)
         g%rssf = g%rssf + g%qssif(i) - g%qssof(i)
26    continue

***   get soil surface fluxes, taking account of top boundary condition
      if(p%itbc.eq.0)then
**       infinite conductance
         ifirst=0
         if(g%psi(0).lt.0.)then
            hsoil=max(hair,exp(hcon*g%psi(0)))
            g%res=g%resp*(hsoil-hair)/(1.-hair)
            respsi=g%resp*hcon*hsoil/(1.-hair)
         else
            g%res=g%resp
            respsi=0.
         end if

         if(p%isbc.eq.0)then
*           no ponding allowed
            g%h=0.
            q0=g%ron-g%res+g%hold/g%dt

            if(g%psi(0).lt.0..or.
     :             q0.lt.g%qs(0)+g%qex(0)+g%q(1)-g%qssif(0)
     :                   +g%qssof(0))then
               g%q(0)=q0
               qp2(0)=-respsi*psip(0)
               g%roff=0.
               roffd=0.d0
            else
*              const zero potl
               ifirst=1
               g%q(0)=g%qs(0)+g%qex(0)+g%q(1)-g%qssif(0)+g%qssof(0)
               g%roff=q0-g%q(0)
               roffd=-qp2(1)
            end if
         else
*           runoff zero or given by a function
            if(g%psi(0).lt.0.)then
               g%h=0.
               g%roff=0.
               g%q(0)=g%ron-g%res+g%hold/g%dt
               qp2(0)=-respsi*psip(0)
            else
               g%h=g%psi(0)
               g%roff=0.
               roffd=0.
               if(p%isbc.eq.2)then
                  call apswim_runoff (g%t,g%h,g%roff,roffd)
               endif
               g%q(0)=g%ron-g%roff-g%res-(g%h-g%hold)/g%dt
               qp2(0)=(-roffd-respsi-1./g%dt)*psip(0)
            end if
         end if
      end if
      if(p%itbc.eq.1)then
**       const potl
         ifirst=1
         if(g%psi(0).lt.0.)then
            hsoil=exp(hcon*g%psi(0))
            g%res=g%resp*(hsoil-hair)/(1.-hair)
         else
            g%res=g%resp
         end if
         g%h=max(g%psi(0),0d0)
         g%q(0)=g%qs(0)+g%qex(0)+g%q(1)-g%qssif(0)+g%qssof(0)
*        flow to source of potl treated as "runoff" (but no bypass flow)
         g%roff=g%ron-g%res-(g%h-g%hold)/g%dt-g%q(0)
      end if
      if(p%itbc.eq.2)then
**       conductance given by a function
         q0=g%ron-g%resp+g%hold/g%dt
         if(p%isbc.eq.0)then
*           no ponding allowed
            ifirst=0
            g%h=0.
            call apswim_scond(g%t,g%h,g_,gh)
            if(q0.gt.-g_*g%psi(0))then
               g%res=g%resp
               respsi=0.
               g%q(0)=-g_*g%psi(0)
               qp2(0)=-g_*psip(0)
               g%roff=q0-g%q(0)
               roffd=-qp2(0)
            else
               hsoil=exp(hcon*g%psi(0))
               g%res=g%resp*(hsoil-hair)/(1.-hair)
               respsi=g%resp*hcon*hsoil/(1.-hair)
               q0=g%ron-g%res+g%hold/g%dt
               g%q(0)=q0
               qp2(0)=-respsi*psip(0)
               g%roff=0.
            end if
         else
*           runoff zero or given by a function
            call apswim_scond(g%t,g%h,g_,gh)
            if(q0.gt.-g_*g%psi(0))then
*              initialise g%h if necessary
               if(ifirst.eq.0)g%h=max(g%psi(0),0d0)
               ifirst=-1
               g%res=g%resp
               g%roff=0.
               roffd=0.
               if(p%isbc.eq.2.and.g%h.gt.0.)then
                  call apswim_runoff(g%t,g%h,g%roff,roffd)
               endif
               g%q(0)=g_*(g%h-g%psi(0))
               qp1(0)=g_+gh*(g%h-g%psi(0))
               qp2(0)=-g_*psip(0)
               rhs(-1)=-(g%ron-g%roff-g%res-g%q(0)-(g%h-g%hold)/g%dt)
               b(-1)=-roffd-qp1(0)-1./g%dt
               c_(-1)=-qp2(0)
            else
               ifirst=0
               g%h=0.
               g%roff=0.
               hsoil=exp(hcon*g%psi(0))
               g%res=g%resp*(hsoil-hair)/(1.-hair)
               respsi=g%resp*hcon*hsoil/(1.-hair)
               g%q(0)=g%ron-g%res+g%hold/g%dt
               qp2(0)=-respsi*psip(0)
            end if
         end if
      end if
*     bypass flow?
      g%qbp=0.
      g%qbpd=0.
      qbpp=0.
      qbps=0.
      qbpsp=0.

***   bottom boundary condition
      if(p%ibbc.eq.0)then
**       zero matric potl gradient
         g%q(p%n+1)=(g%gf+gr)*g%hk(p%n)
         qp1(p%n+1)=(g%gf+gr)*hkp(p%n)
      else if(p%ibbc.eq.1)then
**       const potl
         ilast=p%n-1
         g%q(p%n+1)=g%q(p%n)-g%qs(p%n)-g%qex(p%n)+g%qssif(p%n)
     :             -g%qssof(p%n)

      else if(p%ibbc.eq.2)then
**       zero flux
         g%q(p%n+1)=0.
         qp1(p%n+1)=0.
      else if(p%ibbc.eq.3)then
**       seepage
cnh added to allow seepage to user potential at bbc
         if(g%psi(p%n).ge.p%bbc_value) then
            g%q(p%n+1)=g%q(p%n)-g%qs(p%n)-g%qex(p%n)+g%qssif(p%n)
     :                -g%qssof(p%n)
            if(g%q(p%n+1).ge.0.)then
               ilast=p%n-1
               g%qbpd=0.
            else
               ilast=p%n
            end if
         end if
         if(ilast.eq.p%n)then
            g%q(p%n+1)=0.
            qp1(p%n+1)=0.
         end if
      else if(p%ibbc.eq.4)then
**       flux calculated according to head difference from water table
         headdiff = g%psi(p%n) - p%x(p%n) + p%bbc_value/10d0
     :
         g%q(p%n+1)= headdiff*p%water_table_conductance
         qp1(p%n+1)=psip(p%n)*p%water_table_conductance
      end if
***   get Newton-Raphson equations
      i1=max(ifirst,0)
      k=i1-1
      xidif=.TRUE.
      do 30 i=i1,ilast
*        allow for two nodes at same depth
         xipdif=.TRUE.
         if(xidif)then
            k=k+1
            j=i+1
*           j is next different node, k is equation
            if(i.gt.0.and.i.lt.p%n-1)then
               if(Doubles_are_equal(p%x(i),p%x(i+1)))then
                  xipdif=.FALSE.
                  j=i+2
                  g%q(i+1)=((p%x(j)-p%x(i))*g%q(i)+(p%x(i)-p%x(i-1))*
     :                      g%q(j))/(p%x(j)-p%x(i-1))
               end if
            end if
            rhs(k)=-(g%q(i)-g%q(j))
            a(k)=qp1(i)
            b(k)=qp2(i)-qp1(j)
            c_(k)=-qp2(j)
         end if
         rhs(k)=rhs(k)+g%qs(i)+g%qex(i)-g%qssif(i)+g%qssof(i)
         b(k)=b(k)-qsp(i)-qssofp(i)

         if(iroots.eq.0)then
*            a(k)=a(k)-qexp(1,i)
            b(k)=b(k)-qexp(2,i)
*            c(k)=c(k)-qexp(3,i)
         else
            iroots=2
         end if
         xidif=xipdif
30    continue
      ibegin=ifirst
      iend=k
      end subroutine
* =====================================================================
      double precision function apswim_wpf ()
* =====================================================================
*     Short description:
*     gets water present in profile
*

      implicit none

*     Global Variables

*     Subroutine Arguments
*     none

*     Internal Variables
      integer i
      double precision wpf

*     Constant Values
*     none

*
      wpf=0.
      do 10 i=0,p%n
10    wpf=wpf+g%th(i)*p%dx(i)

      apswim_wpf = wpf

      end function
* =====================================================================
      subroutine apswim_getsol(solnum,a,b,c_,d,rhs,c1,c2,fail)
* =====================================================================
*     Short description:
*     get and solve solute balance eqns
*


      implicit none

*     Global Variables


*     Subroutine Arguments
      integer          solnum
      double precision a(0:p%n)
      double precision b(0:p%n)
      double precision c_(0:p%n)
      double precision d(0:p%n)
      double precision rhs(0:p%n)
      double precision c1(0:p%n)
      double precision c2(0:p%n)
      logical          fail

*     Internal Variables
      double precision accept
      double precision aq
      double precision cp
      double precision csl0
      double precision csln
      double precision csltemp(0:M)
      double precision d1
      double precision d2
      double precision dabs
      double precision dfac
      double precision dmax
      double precision dum1
      double precision dum2(0:M)
      double precision exco1
      double precision fq
      double precision fqc
      integer          crop
      integer          i
      integer          itcnt
      integer          j
      integer          k
      integer          kk
      integer          neq
      logical          nonlin
      double precision rinf
      double precision rovr
      double precision rslout
      double precision rslovr
      double precision thav
      double precision thi
      double precision w1
      double precision w2
      double precision wt
      double precision wtime
      double precision wtime1
      integer          solute_bbc

*     Constant Values
      integer    itmax
      parameter (itmax=20)

      integer    constant_conc
      parameter (constant_conc = 1)

      integer    convection_only
      parameter (convection_only = 2)

*     Determine type of solute BBC to use
      if (p%ibbc.eq.1) then
         ! water table boundary condition
         solute_bbc = constant_conc
      else if (((p%ibbc.eq.0).or.(p%ibbc.eq.4))
     :         .and.(g%q(p%n+1).lt.0)) then
         ! you have a gradient with flow upward
         solute_bbc = constant_conc
      else
         solute_bbc = convection_only
      endif

*
*     surface solute balance - assume evap. (g%res) comes from x0 store
      rovr=g%roff+g%qbp
      rinf=g%q(0)+g%res
      if(rinf.gt.min(c%ersoil,c%ernode))then
         g%cslsur(solnum)=(g%rslon(solnum)+g%hold*g%cslsur(solnum)/g%dt)
     :                     /(rovr+rinf+g%h/g%dt)
         g%qsl(solnum,0)=rinf*g%cslsur(solnum)
         rslovr=rovr*g%cslsur(solnum)
         if(g%slsur(solnum).gt.0.)then
            if(g%cslsur(solnum).lt.p%slsci(solnum))then
               if(g%slsur(solnum).gt.
     :                 rinf*g%dt*(p%slsci(solnum)-g%cslsur(solnum)))then
                  g%qsl(solnum,0)=rinf*p%slsci(solnum)
                  g%slsur(solnum)=g%slsur(solnum)
     :                           -rinf*g%dt*(p%slsci(solnum)
     :                           -g%cslsur(solnum))
               else
                  g%qsl(solnum,0)=rinf*g%cslsur(solnum)
     :                           +g%slsur(solnum)/g%dt
                  g%slsur(solnum)=0.
               end if
            end if
            if(g%cslsur(solnum).lt.p%slscr(solnum))then
               if(g%slsur(solnum).gt.
     :              rovr*g%dt*(p%slscr(solnum)-g%cslsur(solnum)))then
                  rslovr=rovr*p%slscr(solnum)
                  g%slsur(solnum)=g%slsur(solnum)
     :                           -rovr*g%dt*(p%slscr(solnum)
     :                           -g%cslsur(solnum))
               else
                  rslovr=rovr*g%cslsur(solnum)+g%slsur(solnum)/g%dt
                  g%slsur(solnum)=0.
               end if
               if(g%slsur(solnum).gt.
     :                g%h*(p%slscr(solnum)-g%cslsur(solnum)))then
                  g%slsur(solnum)=g%slsur(solnum)
     :                          -g%h*(p%slscr(solnum)-g%cslsur(solnum))
                  g%cslsur(solnum)=p%slscr(solnum)
               else
                  if(g%h.gt.0)g%cslsur(solnum)=g%cslsur(solnum)
     :                                     +g%slsur(solnum)/g%h
                  g%slsur(solnum)=0.
               end if
            end if
         end if
      else
         g%cslsur(solnum)=0.
         g%qsl(solnum,0)=0.
         rslovr=0.
      end if
*     get eqn coeffs
*     get production and storage components
cnh      call slprod
      do 45 i=0,p%n
         c1(i)=g%csl(solnum,i)
         thi=g%th(i)
cnh         j=indxsl(solnum,i)
         j = i
         nonlin=.FALSE.

*Peter's CHANGE 21/10/98 to ensure zero exchange is treated as linear
*         if (p%fip(solnum,j).eq.1.) then
         if ((Doubles_are_equal(p%ex(solnum,j),0.0d0)).or.
     :    (Doubles_are_equal(p%fip(solnum,j),1.0d0))) then
*           linear exchange isotherm
            c2(i)=1.
            exco1=p%ex(solnum,j)
         else
*           nonlinear Freundlich exchange isotherm
            nonlin=.TRUE.
            c2(i)=0.
            if(c1(i).gt.0.)c2(i)=c1(i)**(p%fip(solnum,i)-1.)
!``````````````````````````````````````````````````````````````````````````````````````````````````
cRC         Changed by RCichota 30/jan/2010
            exco1=p%ex(solnum,j)*c2(i)
!            exco1=p%ex(solnum,j)*p%fip(solnum,j)*c2(i)    !<---old code
!			
          end if
         b(i)=(-(thi+exco1)/g%dt)*p%dx(i)
cnh     1        apswim_slupf(1,solnum)*g%qex(i)-g%qssof(i)
     1        -g%qssof(i)
         do 44 crop=1,g%num_crops
            b(i) = b(i) - apswim_slupf(crop,solnum)*g%qr(i,crop)
   44    continue
cnh     1        p%slupf(solnum)*g%qex(i)
         rhs(i)= -(g%csl(solnum,i)*((g%thold(i)+exco1)
     :                 /g%dt))*p%dx(i)
         g%qsls(solnum,i)=
     :          -(g%csl(solnum,i)*(g%thold(i)+p%ex(solnum,j)*c2(i))/
     :          g%dt)*p%dx(i)
45    continue
*     get dispersive and convective components
*        use central diffs in time for convection, backward diffs for rest
*        use central diffs in space, but for convection may need some
*        upstream weighting to avoid instability
      do 50 i=1,p%n
         if(.not.Doubles_are_equal(p%x(i-1),p%x(i)))then
            thav=0.5*(g%th(i-1)+g%th(i))
            aq=abs(g%q(i))
            g%dc(solnum,i)=p%dcon(solnum)
     :                    *(thav-p%dthc)**p%dthp
cnh     1            +0.5*(p%dis(solnum,indxsl(solnum,i-1))
     1            +0.5*(p%dis
cnh     :            +p%dis(solnum,indxsl(solnum,i)))*(aq/thav)**p%disp(solnum)
     :            +p%dis)*(aq/thav)**p%disp
            dfac=thav*g%dc(solnum,i)/(p%x(i)-p%x(i-1))
            if(p%slswt.ge..5.and.p%slswt.le.1.)then
*              use fixed space weighting on convection
               w1=sign(2.*p%slswt,g%q(i))
            else
*              use central diffs for convection if possible, else use
*                 just enough upstream weighting to avoid oscillation
*                 user may increase acceptable level for central diffs
*                 by setting p%slswt < -1
               accept=max(1d0,-p%slswt)
               wt=0.
               if(aq.ne.0.)wt=sign(max(0d0,1.-2.*accept*dfac/aq),g%q(i))
               w1=1.+wt
            end if
            w2=2.-w1

*Peter's CHANGE 21/10/98 to remove/restore Crank-Nicolson time weighting
*for convection
*            fq=.25*g%q(i)
*            fqc=fq*(w1*g%csl(solnum,i-1)+w2*g%csl(solnum,i))
*            wtime=0.25D0
*            wtime1=1.0D0
            wtime=0.5D0
            wtime1=0.0D0
            fq=wtime*g%q(i)
            fqc=wtime1*fq*(w1*g%csl(solnum,i-1)+w2*g%csl(solnum,i))

*           get convective component from old time level
            g%qsl(solnum,i)=fqc
            b(i-1)=b(i-1)-dfac-fq*w1
            c_(i-1)=dfac-fq*w2
            a(i)=dfac+fq*w1
            b(i)=b(i)-dfac+fq*w2
            rhs(i-1)=rhs(i-1)+fqc
            rhs(i)=rhs(i)-fqc
         end if
50    continue
*     allow for bypass flow
      g%qslbp(solnum)=0.

*     impose boundary conditions
      if(p%itbc.eq.1)then
*        constant concentration
         k=1
      else
         k=0
         rhs(0)=rhs(0)-g%qsl(solnum,0)
         if(rinf.lt.-min(c%ersoil,c%ernode))then
            b(0)=b(0)+.5*rinf
            rhs(0)=rhs(0)-.5*rinf*g%csl(solnum,0)
         end if
      end if
      if(solute_bbc.eq.constant_conc)then
*        constant concentration
cnh
         g%csl(solnum,p%n) = p%cslgw(solnum)
cnh
         rhs(p%n-1)=rhs(p%n-1)-c_(p%n-1)*g%csl(solnum,p%n)
         neq=p%n

      else
*        convection only
         b(p%n)=b(p%n)-.5*g%q(p%n+1)
         rhs(p%n)=rhs(p%n)+.5*g%q(p%n+1)*g%csl(solnum,p%n)
         neq=p%n+1
      end if
*     allow for two nodes at same depth
      j=0
      do 60 i=1,p%n
         if(.not.Doubles_are_equal(p%x(i-1),p%x(i)))then
            j=j+1
            a(j)=a(i)
            b(j)=b(i)
            rhs(j)=rhs(i)
            c_(j-1)=c_(i-1)
         else
            b(j)=b(j)+b(i)
            rhs(j)=rhs(j)+rhs(i)
         end if
60    continue
*     save old g%csl(0),g%csl(p%n)
      csl0=g%csl(solnum,0)
      csln=g%csl(solnum,p%n)
      neq=neq-(p%n-j)
      itcnt=0
*     solve for concentrations
62    continue
cnh      call thomas(neq,0,a(k),b(k),c(k),rhs(k),dum,d(k),g%csl(solnum,k),
cnh     :            dum,fail)
      do 63 i=0,p%n
        csltemp(i) = g%csl(solnum,i)
   63 continue
      call apswim_thomas
     :   (neq,a(k),b(k),c_(k),rhs(k),dum1,d(k),csltemp(k),dum2,fail)
      do 64 i=0,p%n
        g%csl(solnum,i) = csltemp(i)
   64 continue

cnh end subroutine
      itcnt=itcnt+1
      g%slwork=g%slwork+neq
      if(fail)go to 90
      j=k+neq-1
      if(solute_bbc.eq.convection_only)then
         g%csl(solnum,p%n)=g%csl(solnum,j)
         j=j-1
      end if
      do 65 i=p%n-1,1,-1
         if(.not.Doubles_are_equal(p%x(i),p%x(i+1)))then
            g%csl(solnum,i)=g%csl(solnum,j)
            j=j-1
         else
            g%csl(solnum,i)=g%csl(solnum,i+1)
         end if
65    continue
      if(nonlin)then
*        test for convergence
         dmax=0.
         do 66 i=0,p%n
            dabs=abs(g%csl(solnum,i)-c1(i))
            if(dmax.lt.dabs)dmax=dabs
66       continue
         if(dmax.gt.c%slcerr)then
            if(itcnt.eq.itmax)then
               fail=.TRUE.
               go to 90
            end if
*           keep iterating using Newton-Raphson technique
*           next c^fip for Freundlich isotherm is approximated as
*              cn^fip=c^fip+p%fip*c^(p%fip-1)*(cn-c)
*                    =p%fip*c^(p%fip-1)*cn+(1-p%fip)*c^fip
            j=0
            do 67 i=0,p%n
               if(.not.Doubles_are_equal(p%x(i-1),p%x(i))) then
                  if (i.gt.0) then
                     j=j+1
                  end if
                  END if
cnh               kk=indxsl(solnum,i)
               kk = i
               if(.not.Doubles_are_equal(p%fip(solnum,i),1.0d0))then
                  cp=0.
                  if(g%csl(solnum,i).gt.0.)then
                     cp=g%csl(solnum,i)**(p%fip(solnum,i)-1.)
                  endif

!````````````````````````````````````````````````````````````````````````````````````````````````````````````````
cRC      Changed by RCichota (29/Jan/2010), original code is commented out
                  d1 = cp-c2(i)
!                  d1=p%fip(solnum,kk)*(cp-c2(i))
!                  d2=(1.-p%fip(solnum,kk))
!     :              *(g%csl(solnum,i)*cp-c1(i)*c2(i))
                  c1(i)=g%csl(solnum,i)
                  c2(i)=cp
                  b(j)=b(j)-(p%ex(solnum,kk)/g%dt)
     :                 *d1*p%dx(i)
!                  rhs(j)=rhs(j)+(p%ex(solnum,kk)/g%dt
!     :                            -p%betaex(solnum,kk))
!     :                          *d2*p%dx(i)
!````````````````````````````````````````````````````````````````````````````````````````````````````````````````
! Changes in the calc of d1 are to agree with the calc of exco1 above (no need to multiply by p%fip
! If p%fip < 1, the unkown is Cw, and is only used in the calc of b. thus rhs is commented out.
!`	 
               end if
67          continue
            go to 62
         end if
      end if
*     get surface solute balance?
      if(rinf.lt.-min(c%ersoil,c%ernode))then
*        flow out of surface
*CHANGES 6/11/98 to remove/restore Crank-Nicolson time weighting for convection
*-----
*         g%qsl(solnum,0)=.5*rinf*(csl0+g%csl(solnum,0))
         g%qsl(solnum,0)=.5*rinf*(wtime1*csl0+4D0*wtime*g%csl(solnum,0))

         rslout=-g%qsl(solnum,0)
         if(g%slsur(solnum).gt.0.)then
*           allow for surface applied solute
            if(g%csl(solnum,0).lt.p%slsci(solnum))then
               if(g%slsur(solnum).gt.
     :                  -rinf*g%dt*(p%slsci(solnum)-g%csl(solnum,0)))
     :         then
                  rslout=-rinf*p%slsci(solnum)
                  g%slsur(solnum)=g%slsur(solnum)
     :                           +rinf*g%dt*(p%slsci(solnum)
     :                                        -g%csl(solnum,0))
               else
                  rslout=rslout+g%slsur(solnum)/g%dt
                  g%slsur(solnum)=0.
               end if
            end if
         end if
*        get surface solute balance
         g%cslsur(solnum)=(g%rslon(solnum)+rslout
     :                     +g%hold*g%cslsur(solnum)/g%dt)
     :                   /(rovr+g%h/g%dt)
         rslovr=rovr*g%cslsur(solnum)
      end if

      g%rsloff(solnum)=rslovr-g%qslbp(solnum)
*     get solute fluxes
      do 70 i=1,p%n
         if(.not.Doubles_are_equal(p%x(i-1),p%x(i)))then
            dfac=0.5*(g%th(i-1)+g%th(i))*g%dc(solnum,i)
     :                                  /(p%x(i)-p%x(i-1))
            aq=abs(g%q(i))
            accept=max(1d0,-p%slswt)
            wt=0.
            if(aq.ne.0.)wt=sign(max(0d0,1.-2.*accept*dfac/aq),g%q(i))
*Peter's CHANGES 21/10/98 to remove/restore Crank-Nicolson time weighting
*for convection
*            g%qsl(solnum,i)=g%qsl(solnum,i)
*     :                    +.25*g%q(i)*((1.+wt)*g%csl(solnum,i-1)
*     :                    +(1.-wt)*g%csl(solnum,i))
*     1                    +dfac*(g%csl(solnum,i-1)-g%csl(solnum,i))
            g%qsl(solnum,i)=g%qsl(solnum,i)
     :                    +wtime*g%q(i)*((1.+wt)*g%csl(solnum,i-1)
     :                    +(1.-wt)*g%csl(solnum,i))
     1                    +dfac*(g%csl(solnum,i-1)-g%csl(solnum,i))
         end if

70    continue
      do 75 i=2,p%n-1
         if(Doubles_are_equal(p%x(i-1),p%x(i)))then
            g%qsl(solnum,i)=
     :               (p%dx(i)*g%qsl(solnum,i-1)
     :                  +p%dx(i-1)*g%qsl(solnum,i+1))
     :               /(p%dx(i-1)+p%dx(i))
         end if
75    continue

      g%rslex(solnum)=0.
      do 80 i=0,p%n
cnh         j=indxsl(solnum,i)
         j = i
         cp=1.
         if(.not.Doubles_are_equal(p%fip(solnum,i),1.0d0))then
            cp=0.
            if(g%csl(solnum,i).gt.0.)
     :            cp=g%csl(solnum,i)**(p%fip(solnum,i)-1.)
         end if
         g%cslt(solnum,i)=(g%th(i)+p%ex(solnum,j)*cp)*g%csl(solnum,i)

         do 79 crop=1,g%num_Crops
            g%rslex(solnum)=g%rslex(solnum)+g%qr(i,crop)*g%csl(solnum,i)
     :                *apswim_slupf(crop,solnum)
   79    continue
         g%qsls(solnum,i)=g%qsls(solnum,i)+
     :           (g%csl(solnum,i)*(g%thold(i)+p%ex(solnum,j)*cp)/g%dt)*
     :           p%dx(i)
80    continue
      if(solute_bbc.eq.constant_conc)then
*        constant concentration
cnh         j=indxsl(solnum,p%n)
         j = p%n
         g%qsl(solnum,p%n+1)=g%qsl(solnum,p%n)-g%qsls(solnum,p%n)
cnh     :                  -g%qex(p%n)*g%csl(solnum,p%n)*p%slupf(solnum)
cnh     :              -g%qex(p%n)*g%csl(solnum,p%n)*apswim_slupf(1,solnum)
     :              -g%qssof(p%n)*g%csl(solnum,p%n)

         do 81 crop=1,g%num_crops
            g%qsl(solnum,p%n+1)=g%qsl(solnum,p%n+1)
     :      -g%qr(p%n,crop)*g%csl(solnum,p%n)*apswim_slupf(crop,solnum)
   81    continue
      else
*        convection only
*CHANGES 6/11/98 to remove/restore Crank-Nicolson time weighting for convection
*-----
*         g%qsl(solnum,p%n+1)=.5*g%q(p%n+1)*(csln+g%csl(solnum,p%n))
         g%qsl(solnum,p%n+1)=.5*g%q(p%n+1)
     :                    *(wtime1*csln+4D0*wtime*g%csl(solnum,p%n))

      end if
90    continue
      end subroutine
* =====================================================================
      subroutine apswim_thomas(n,a,b,c,rhs,rb,d,v,vb,fail)
* =====================================================================
*     Short description:
*     Thomas algorithm for solving tridiagonal system of eqns
*

      implicit none

*     Global Variables


*     Subroutine Arguments

      integer          N
      double precision a(N)
      double precision b(N)
      double precision c(N)
      double precision d(N)
      logical          fail
      double precision rb
      double precision rhs(N)
      double precision v(N)
      double precision vb(N)

*     Local Variables

      double precision fac
      integer          i
      double precision piv

*     Constant Values
*     none

*
      if(b(1).eq.0.)go to 60
      piv=b(1)
      v(1)=rhs(1)/piv
      do 10 i=2,N
          d(i)=c(i-1)/piv
          piv=b(i)-a(i)*d(i)
          if(piv.eq.0.)go to 60
          v(i)=(rhs(i)-a(i)*v(i-1))/piv
10    continue
      do 20 i=N-1,1,-1
      v(i)=v(i)-d(i+1)*v(i+1)
20    continue
      fail=.FALSE.
      go to 70
60    fail=.true.
70    continue
      end subroutine
* =====================================================================
      subroutine apswim_trans(p,psi,psip,psipp)
* =====================================================================
*     Short description:
*     gets psi and its partial derivitives
*

      implicit none

*     Global Variables


*     Subroutine Arguments
      double precision p
      double precision psi
      double precision psip
      double precision psipp

*     Internal Variables

      double precision ep
      double precision emp
      double precision sinhp
      double precision coshp
      double precision v

* Constants
      double precision psi0
      parameter (psi0=-50d0)

      double precision psi1
      parameter (psi1=psi0/10d0)

*
      if(p.lt.0d0)then
         ep=exp(p)
         emp=1d0/ep
         sinhp=0.5d0*(ep-emp)
         coshp=0.5d0*(ep+emp)
         v=psi1*sinhp
         psi=psi0-v
         psip=-psi1*coshp
         psipp=-v
      else
         psi=psi0-psi1*p
         psip=-psi1
         psipp=0d0
      end if

      end subroutine
*
***   water functions
* =====================================================================
      double precision function apswim_pf(psi)
* =====================================================================
*     Short description:
*     returns transform p
*

      implicit none

*     Global Variables
*     none


*     Subroutine Arguments
      double precision psi

*     Internal Variables
      double precision v

*     Constant Values
      double precision psi0
      parameter (psi0=-50d0)

      double precision psi1
      parameter (psi1=psi0/10d0)

*
      v=-(psi-psi0)/psi1
      if(psi.lt.psi0)then
         apswim_pf=log(v+sqrt(v**2+1d0))
      else
         apswim_pf=v
      end if

      end function
* ===================================================================
      subroutine apswim_uptake(tpsi,thk,tpsip,thkp,tqex,tqexp)
* ===================================================================
*     gets flow rates to roots and total water extraction rates
*
*  Note some variables renamed using g%t prefix because of clash with
*  common variables.
* g%psi->tpsi
* psip->tpsip
* g%hk->thk
* hkp->thkp
* g%qex->tqex
* qexp->tqexp
* g%q->tq
* tr->ttr


      implicit none

*     Subroutine Arguments

      double precision tpsi(0:p%n),thk(0:p%n),tpsip(0:p%n),thkp(0:p%n)
     :                 ,tqex(0:p%n),tqexp(3,0:p%n)


*     Local Variable
      double precision a
      double precision b
      logical          change
      double precision derp
      double precision g_(0:M)
      integer          i
      integer          iveg
      integer          j
      integer          k
      double precision psix
      double precision qhk
      double precision qpsi
      logical          stress
      double precision ttr
      double precision tq

*     set root conductance gr (alter as required)
c      double precision gr  ! cm/g%h
c      parameter (gr=1.4d-7)

*
      do 10 i=0,p%n
         tqex(i)=0.
         g%qexpot(i) = 0.
         do 10 j=1,3
         tqexp(j,i)=0.
cnh
        do 5 k=1,g%nveg
           g%qr(i,k) = 0d0
           g%qrpot(i,k) = 0d0
 5      continue
cnh
10    continue
      do 100 iveg=1,g%nveg
*        find transpiration rates
         g%rt(iveg)=0.
         ttr=g%rtp(iveg)
         if(ttr.gt.0.)then
            psix=g%psimin(iveg)
*           get soil->xylem conductances
            a=0.
            b=0.
            do 20 i=0,p%n
               g_(i)=0.
               if(tpsi(i).gt.psix)then
cnh root conductance is not an input
cnh                  g(i)=1./(g%rc(i,iveg)/thk(i)+1./(gr*g%rld(i,iveg)*p%dx(i)))
                  g_(i)=1./(g%rc(i,iveg)/thk(i)+1./
     :                     (g%root_conductance(i,iveg)*g%rld(i,iveg)
     :                         *p%dx(i)))
               end if
               a=a+g_(i)*tpsi(i)
               b=b+g_(i)
20          continue
            if(b.eq.0.)then
               stress=.TRUE.
            else if((a-ttr)/b.lt.psix)then
               stress=.TRUE.
            else
               stress=.FALSE.
            end if
            if(.not.stress)then
*              get xylem potl
30             continue
                  change=.FALSE.
                  psix=(a-ttr)/b
                  do 40 i=0,p%n
                     if(tpsi(i).lt.psix.and.g_(i).ne.0.)then
                        change=.TRUE.
                        a=a-g_(i)*tpsi(i)
                        b=b-g_(i)
                        g_(i)=0.
                     end if
40                continue
               if(change)go to 30
            end if
            if (g%psix(iveg).gt.psix) then
               g%psix(iveg) = psix
            endif
            do 50 i=0,p%n
               if(g_(i).ne.0.)then
                  tq=g_(i)*(tpsi(i)-psix)
                  tqex(i)=tqex(i)+tq
*                 get partial derivs of tqex at i-1, i, i+1 wrt g%p
                  qpsi=g_(i)
                  qhk=g_(i)*g%rc(i,iveg)*tq/thk(i)**2
                  if(.not.stress)then
                     derp=qpsi*tpsip(i)+qhk*thkp(i)
                     if(i.gt.0)tqexp(3,i-1)=tqexp(3,i-1)-g_(i-1)*derp/b
                     if(i.lt.p%n)tqexp(1,i+1)=tqexp(1,i+1)
     :                                          -g_(i+1)*derp/b
                     qpsi=qpsi*(1d0-g_(i)/b)
                     qhk=qhk*(1d0-g_(i)/b)
                  end if
                  tqexp(2,i)=tqexp(2,i)+qpsi*tpsip(i)+qhk*thkp(i)
                  g%rt(iveg)=g%rt(iveg)+tq
                  g%qr(i,iveg)=tq
               else
                  g%qr(i,iveg)=0.
               end if
               if (ttr.gt.0) then
                  g%qrpot(i,iveg) = g_(i)*(tpsi(i)-g%psimin(iveg))
                  g%qexpot(i)=g%qexpot(i)+g%qrpot(i,iveg)
               else
                  g%qrpot(i,iveg) = 0d0
               endif
50          continue
         end if
100   continue

      end subroutine
* =====================================================================
      subroutine apswim_watvar(ix,tp,tpsi,psip,psipp,tth,thp,thk,hkp)
* =====================================================================
*     Short Description:
*     calculates water variables from transform value g%p at grid point ix
*     using cubic interpolation between given values of water content p%wc,
*     log10 conductivity p%hkl, and their derivatives p%wcd, p%hkld with respect
*     to log10 suction p%sl
*
*     nih - some local variables had the same name as globals so I had
*     to rename them. I added a g%t (for temp) to start of name for
*     g%psi, g%hk, g%p, g%th, p%x, p%dx,g%dc


      implicit none

*     notes

*         dTheta     dTheta       d(log g%psi)
*         ------ = ----------  p%x  ---------
*           dP     d(log g%psi)        d g%p

*                    dTheta        d g%psi           1
*                = ----------  p%x  -------  p%x ------------
*                  d(log g%psi)       d g%p       ln(10).g%psi


*         dHK          dHK       d(log g%psi)
*        ------  = ----------  p%x ----------
*          dP      d(log g%psi)       d g%p

*                   ln(10).g%hk   d(log(g%hk))     dPsi        1
*                =  --------- p%x ----------  p%x ------ p%x ----------
*                        1      d(log(g%psi))     dP     ln(10).g%psi

*                    g%hk       d(log(g%hk))     dPsi
*                =  -----  p%x  ----------  p%x  ----
*                    g%psi      d(log(g%psi))     dP

*     note:- d(log(y)/p%dx) = 1/y . dy/p%dx
*
*     Therefore:-
*
*            d(log10(y))/p%dx = d(ln(y)/ln(10))/p%dx
*                           = 1/ln(10) . d(ln(y))/p%dx
*                           = 1/ln(10) . 1/y . dy/p%dx

*     Global Variables

*     Subroutine Arguments
      integer ix
      double precision tp              ! g%p - transform of g%psi
      double precision tpsi            ! g%psi for the given g%p
      double precision psip            ! dPsi/dP  - 1st derivative
      double precision psipp           ! dPsip/dP - 2nd derivative
      double precision tth             ! water content for given g%psi
      double precision thp             ! dTheta/dP
      double precision thk             ! hydraulic conductivity
      double precision hkp             ! d(thk)/dP

*     Internal Variables
      double precision hklg
      double precision hklgd
      double precision hkv
      double precision phi
      double precision thd
      double precision thsat

*     Constant Values
      double precision al10
      parameter (al10=2.3025850929940457d0)

      double precision vcon1
      parameter (vcon1=7.28d-9)

      double precision vcon2
      parameter (vcon2=7.26d-7)
*

      call apswim_trans(tp,tpsi,psip,psipp)

      call apswim_interp (ix,tpsi,tth,thd,hklg,hklgd)

      thk=exp(al10*hklg)

      if(tpsi.ne.0d0)then
         thp=(thd*psip)/(al10*tpsi)
         hkp=(thk*hklgd*psip)/tpsi
      end if

      thsat = p%sat(ix) ! NOTE: this assumes that the wettest p%wc is
                        ! first in the pairs of log suction vs p%wc
      if (thsat.eq.0.0) thsat = p%sat(ix)

      if(p%ivap)then
*        add vapour conductivity hkv
         phi=thsat/.93-tth
         hkv=vcon1*phi*exp(vcon2*tpsi)
         thk=thk+hkv
         hkp=hkp+hkv*(vcon2*psip-thp/phi)

      end if

      end subroutine


* =====================================================================
      subroutine apswim_scond(ttt,tth,g_,gh)
* =====================================================================
*     Short Description:
*     gets soil surface conductance g and derivative gh
*
*     g%t was renamed to ttt as g%t already exists in common
*     g%h was renamed to tth as g%h already exists in common


      implicit none

*     Global Variables

*     Subroutine Arguments
      double precision ttt
      double precision tth
      double precision g_
      double precision gh

*     Internal Variables
*     none

*     Constant Values
*     none
*
      g_ = g%gsurf
      gh = 0d0

      return
      end subroutine


* =====================================================================
      subroutine apswim_runoff(t,h,roff,roffh)
* =====================================================================
*     Short Description:
*     gets runoff rate


      implicit none

*     Subroutine Arguments
      double precision t
      double precision h
      double precision roff
      double precision roffh

*     Internal Variables
      double precision v

*     Constant Values
*     none

      if(h.gt.g%hmin)then
         v=p%roff0*(h-g%hmin)**(p%roff1-1d0)
         roff=v*(h-g%hmin)
         roffh=p%roff1*v
      else
         roff=0d0
         roffh=0d0
      end if

      return
      end subroutine
*
      subroutine map(n,x,y,M,u,v)
*     maps concentration in y into v so that integral is conserved
*     p%x and u give intervals corresponding to y and v values
      integer N,M
      double precision x(*),y(*),u(*),v(*)
      double precision sx, sy, su, sv,w
      logical again
cnh added following declarations
      integer i,j

      sx=0.
      sy=0.
      j=0
      su=u(1)
      sv=0.
      do 20 i=1,N
         sx=sx+x(i)
         sy=sy+y(i)*x(i)
10       continue
            again=.FALSE.
            if((j.lt.M).and.(sx.ge.su.or.i.eq.N))then
               j=j+1
               w=sy-(sx-su)*y(i)
               v(j)=(w-sv)/u(j)
               if(j.lt.M)then
                  su=su+u(j+1)
                  sv=w
                  again=.TRUE.
               end if
            end if
         if(again)go to 10
20    continue
      end subroutine

* =====================================================================
      subroutine apswim_drain(qdrain,qdrainpsi)
* =====================================================================
*     Short Description:
*     gets flow rate into drain
*     All units are mm and days


      implicit none

*     Subroutine Arguments
      double precision qdrain(0:M)
      double precision qdrainpsi(0:M)

*     Internal Variables
      integer drain_node
      real dlayer(1:M+1)
      double precision q,q2,d,wt_above_drain,wt_above_drain2
      double precision qdrain2(0:M)

*     Constant Values
      double precision dpsi
      parameter (dpsi = 0.01)

*
      qdrain(0:M) = 0d0
      qdrainpsi(0:M) = 0d0
      dlayer(1:M+1) = p%dlayer(0:M)

      if (p%subsurface_drain.eq.'on') then
         drain_node = find_layer_no(real(p%drain_depth)
     :                             ,dlayer(1)
     :                             ,p%n+1)
     :              - 1

         d = p%Imperm_depth - p%drain_depth
         if (g%psi(drain_node).gt.0) then
            wt_above_drain = g%psi(drain_node)*10d0
         else
            wt_above_drain = 0d0
         endif

         q = Hooghoudt(d
     :                ,wt_above_drain
     :                ,p%drain_spacing
     :                ,p%drain_radius
     :                ,p%Klat)

         qdrain(drain_node) = q/10d0/24d0


         if (g%psi(drain_node)+dpsi.gt.0) then
            wt_above_drain2 = (g%psi(drain_node)+dpsi)*10d0
         else
            wt_above_drain2 = 0d0
         endif

         q2 = Hooghoudt(d
     :                ,wt_above_drain2
     :                ,p%drain_spacing
     :                ,p%drain_radius
     :                ,p%Klat)

         qdrain2(drain_node) = q2/10d0/24d0

         qdrainpsi(drain_node) =
     :         (qdrain2(drain_node)-qdrain(drain_node))/dpsi

      endif

      return
      end subroutine

*     ===========================================================
      double precision function Hooghoudt (d,m,L,r,Ke)
*     ===========================================================

      implicit none

*+  Sub-Program Arguments
      double precision       d           ! (input) distance from drain to impermeable layer (mm)
      double precision       m           ! (input) distance from drain to water table       (mm)
      double precision       L           ! (input) distance between drains                  (mm)
      double precision       r           ! (input) drain radius                             (mm)
      double precision       Ke          ! (input) effective lateral saturated conductivity (mm/d)

*+  Purpose
*       Drainage loss to subsurface drain using Hooghoudts drainage equation. (mm/d)


*+  Constant Values
      character  my_name*(*)           ! name of subroutine
      parameter (my_name = 'Hooghoudt')

      double precision C                 ! ratio of flux between drains to flux midway between drains.
      parameter (C = 1.0)    ! value of 1.0 usually used as a simplification.

      double precision pi
      parameter (pi = 3.14159265)

*+  Local Variables
      double precision       q           ! flux into drains (mm/s)
      double precision       de          ! effective d to correct for convergence near the drain. (mm)
      double precision       alpha       ! intermediate variable in de calculation

*- Implementation Section ----------------------------------

      if (d/L .le. 0) then
         de = 0.0
      elseif (d/L .lt. 0.3) then
         alpha = 3.55 - 1.6*(d/L) + 2*(d/L)**2
         de = d/(1.0+d/L*(8.0/pi*log(d/r)-alpha))
      else
         de = L*pi/(8.0*log(L/r)-1.15)
      endif

      q = (8.0*Ke*de*m + 4*Ke*m**2)/(C*L**2)

      Hooghoudt = q

      return
      end function

