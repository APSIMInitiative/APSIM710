'      include 'apswim.inc'

* =
sub  apswim_gsurf(deqrain,surfcon)
* =
*     gets soil surface conductance, surfcon
use infrastructure
'implicit none

*     global variables


*     subroutine arguments
' sub parameter : do not dim ! Dim deqrain as double
' sub parameter : do not dim ! Dim surfcon as double

*     internal variables
Dim decay_fraction as double
Dim ceqrain as double

*     constant values
*     none
'     ! ideally, if timesteps are small we could just use
'     ! dgsurf/deqr = -1/grc x (gsurf - g0)
'     ! but because this is really just a linear approximation of the
'     ! curve for longer timesteps we had better be explicit and
'     ! calculate the difference from the exponential decay curve.

if (p%grc <> 0) then
'        ! first calculate the amount of energy that must have been
'        ! applied to reach the current conductance.

    decay_fraction = ddivide(g%gsurf-p%g0,p%g1-p%g0,0d0)

    if (doubles_are_equal (decay_fraction, 0d0)) then
'           ! seal is totally decayed
        surfcon = p%g0
        else

        ceqrain = -p%grc * log(decay_fraction)

'           ! now add rainfall energy for this timestep
        if (c%cover_effects = "on") then
            ceqrain = ceqrain + deqrain * (1d0 - g%residue_cover)
            else
            ceqrain = ceqrain + deqrain
        end if

'           ! now calculate new surface storage from new energy
        surfcon = p%g0+(p%g1-p%g0)*exp(-ceqrain/p%grc)
    end if
    else
    surfcon = g%gsurf
end if

exit sub
end sub
* =
sub  apswim_solve(itlim,fail)
* =
*     solves for this time step
use infrastructure
'implicit none

*     global variables


*     subroutine arguments
' sub parameter : do not dim ! Dim itlim           '(input) limit for no. of iterations as integer
' sub parameter : do not dim ! Dim fail            '(output)fail flag as boolean

*     internal variables

Dim aerr as double
Dim balerr as double
Dim err as double
Dim i as integer
Dim i1 as integer
Dim i2 as integer
Dim i0 as integer
Dim ibpf as integer
Dim iroots as integer
Dim it as integer
Dim j as integer
Dim neq as integer
Dim solnum as integer
Dim wpold as double

*     constant values
*     none

it=0
wpold=g%wp
iroots=0
*     loop until solved or too many iterations or thomas algorithm fails
L10:
continue
it=it+1
*        get balance eqns
call apswim_baleq(it,iroots,p%isol,p%slos,g%csl,i1,i2,a,b,c_
*        test for convergence to soln
cnh hey - wpf has no arguments '
cnh         g%wp=wpf(p%n,p%dx,g%th)
g%wp = apswim_wpf()

balerr=g%ron-g%roff-g%q(p%n+1)-g%rex-g%res+g%rssf-
L1:
(g%h-g%hold+g%wp-wpold+(g%hbp-g%hbpold)*p%sbp)/g%dt
err=0.
for i=i1 to 2
    aerr=abs(rhs(i))
    if(err < aerr) then rr=aerr
L20:
    continue
next i
*        switch off iteration for root extraction if err small enough
if(err < p%errex*g%rex and iroots = 0) then roots=1
if(abs(balerr) < p%ersoil and err < p%ernode)then
fail= false
else
neq=i2-i1+1
ibpf=p%ibp-i1+1
call apswim_thomas(neq,ibpf,a(i1),b(i1),c_(i1),rhs(i1)
g%work=g%work+neq
cnh            if(fail)goto L90
if(fail) then
    cnh               call warning_error(err_internal,
    goto L90
end if

fail= true
*           limit step size_of for soil nodes
i0=max(i1,0)
for i=i0 to 2
    if(dp(i) > p%dppl) then p(i)=p%dppl
    if(dp(i) < -p%dpnl) then p(i)=-p%dpnl
L30:
    continue
next i
*           update solution
j=i0
for i=i0 to 2
    g%p(j)=g%p(j)+dp(i)
    if(j > 0 and j < p%n-1)then
    if(doubles_are_equal(p%x(j),p%x(j+1)))then
    j=j+1
    g%p(j)=g%p(j-1)
end if
end if
j=j+1
L40:
continue
next i
if(i1 = -1) then %h=max(0d0,g%h+dp(-1))
end if
if(fail and it < itlim) then oto L10
cnh      if(p%isol <> 1 or fail)goto L90
if (fail) then
    goto L90
end if
if(p%isol <> 1) then
    goto L90
end if

*     solve for solute movement
cnh      call getsol(a(0),b(0),c(0),d(0),rhs(0),dp(0),vbp(0),fail)

for solnum=1 to p%num_solutes
    call apswim_getsol
    if (fail) then

        goto L85
    end if
L80:
    continue
next solnum
L85:
continue
cnh

L90:
continue
end sub
* =
sub  apswim_pstat (istat,tresp)
* =
*     gets potl evap. for soil and veg., and root length densities
*     g%resp,p%slupf and g%csl were renamed to tslupf,trep,tcsl as there were
*     already variables with those names in common

use infrastructure
'implicit none

*     global variables
cnh      double precision cevap            ' function


*     subroutine arguments
' sub parameter : do not dim ! Dim istat as integer
' sub parameter : do not dim ! Dim tresp as double

*     internal variables
cnh      double precision frac
Dim i as integer
Dim iveg as integer
Dim j as integer
Dim rep as double
Dim rldi as double
Dim sep          ' soil evaporation demand as double
cnh      double precision sfrac
Dim solnum as integer
cnh      double precision tfrac
Dim start_of_day as double
Dim end_of_day as double
Dim td_eo as double

*     constant values

c      double precision rad    ' set root radius rad (alter as required)
c      parameter (rad=0.1d0)

Dim pi as double
parameter (pi=3
if(istat = 0)then
*        calc. potl evap.
rep=(apswim_cevap(g%t)-apswim_cevap(g%t-g%dt))/g%dt
if (c%cover_effects = "on") then
'           !use soilwat cover effects on evaporation.
    sep = rep*g%dt * apswim_cover_eos_redn()
    else
    sep = rep*g%dt * (1d0 - g%crop_cover)
end if

'        ! note: g%pep is passed to swim as total ep for a plant for the
'        ! entire apsim timestep. so rate will be (cep = cum ep)
'        !   dcep   total daily ep     deo
'        !   ---- = -------------- p%x --------
'        !    g%dt    total daily eo      g%dt

start_of_day = apswim_time (g%year,g%day,
end_of_day = apswim_time (g%year

td_eo = apswim_cevap(end_of_day)-apswim_cevap(start_of_day)

for j=1 to g%nveg
    g%rtp(j) = ddivide(g%pep(j),td_eo, 0d0)*rep
L50:
    continue
next j

'        ! pot soil evap rate is not linked to apsim timestep
tresp = sep/g%dt

for iveg=1 to g%nveg
    for i=0 to p%n
        cnh               g%rld(i,iveg)=g%rld(i,iveg)/p%dx(i)
        if(g%rld(i,iveg) < 1d-20) then %rld(i,iveg)=1d-20
        rldi=g%rld(i,iveg)
        cnh now use root_raidus as in initialisation file
        cnh               g%rc(i,iveg)=-log(pi*rad^2*rldi)/(4.*pi*rldi*p%dx(i))

        g%rc(i,iveg)=-log(pi*g%root_radius(iveg)^2*rldi)

L60:
        continue
    next i
next iveg

elseif(istat = 1)then
*        update cumulative transpiration
for i=1 to g%nveg
    g%ctp(i)=g%ctp(i)+g%rtp(i)*g%dt
    g%ct(i)=g%ct(i)+g%rt(i)*g%dt
    cnh
    for j=0 to p%n
        g%pwuptake(i,j) = g%pwuptake(i,j) + g%qr(j,i)*g%dt*10d0
'                                            ! cm -> mm __/
        g%pwuptakepot(i,j)=g%pwuptakepot(i,j)
'                                       ! cm -> mm __/
L65:
        continue
    next j
L70:
    continue
next i

elseif(istat = 2)then
*        update cumulative solute uptake

for i=1 to g%nveg
    for j=0 to p%n
        for solnum=1 to p%num_solutes
            g%slup(i,solnum)=g%slup(i,solnum)
'                 ! i thought g%qr was a rate ----------------/

            g%psuptake(solnum,i,j) = g%psuptake (solnum,i,j) +
L95:
            continue                                   '   /
        next solnum
'                                                         ! ppm -> kg/ha
        c        this doesn"t make sense....g%csl has already been changed from it
        c        was at the start of the timestep.  need to find a different way
        c        of calculating it.  what about qsl???
        c        or try getting g%csl at start of timestep.
        c        but numbers do add up ok????? does he then update at start of next
        c        timestep??????? ''


L80:
        continue
    next j
next solnum
L90:
continue
next i

end if

exit sub
end sub
* =
sub  apswim_baleq
* =
*     gets coefficient matrix and rhs for newton soln of balance eqns
*     some variables had the same name as some global variables and so
*     these were renamed (by prefixing with g%t - for temp)
*     this '************ to be changed ******** include p%isol, g%csl, p%slos

use infrastructure
'implicit none

*     global variables


*     subroutine argruments
Dim it                '(input) iteration no. as integer
Dim iroots            '(input) root extraction flag as integer
Dim tisol             '(input) solute flag as integer
Dim tslos(nsol)    '(input) osmotic pressure per unit solute as double
Dim ibegin            '(output) position of first equation as integer
Dim iend              '(output) position of last equation as integer
Dim a(-1:p%n) as double
Dim b(-1:p%n) as double
Dim c_(-1:p%n) as double

*     internal variables
Dim accept as double
Dim absgf as double
Dim deltax as double
Dim deltap as double
Dim hkd2 as double
Dim hkdp1 as double
Dim hkdp2 as double
Dim g_ as double
Dim gh as double
Dim gr as double
Dim hkd1 as double
Dim hsoil as double
Dim i as integer
Dim i1 as integer
Dim ifirst as integer
Dim ilast as integer
Dim j as integer
Dim k as integer
Dim q0 as double
Dim qbpp as double
Dim qbps as double
Dim qbpsp as double
Dim respsi as double
Dim roffd as double
Dim skd as double
Dim solnum as integer
Dim v1 as double
Dim value as double
Dim w1 as double
Dim w2 as double
Dim wt as double
Dim xidif as boolean
Dim xipdif as boolean
Dim string as string * 300
Dim gfhkp as double
Dim qdrain(0:m) as double
Dim qdrainpsi(0:m) as double

Dim headdiff ' head difference between water table and as double
'                               ! potential at bottom of profile (cm)

save ifirst,ilast,gr

*     constant values
Dim hcon as double
parameter (hc

Dim hair as double
parameter

cnh - added initialisation to zero to eliminate ndp errors
for i=0 to m
    psip(i)=0.d0
    psipp(i)=0.d0
    thp(i)=0.d0
    hkp(i)=0.d0
    qsp(i)=0.d0
    for j=1 to 3
        qexp(j,i)=0.d0
L2:
        continue
    next j
    qp1(i)=0.d0
    qp2(i)=0.d0
    psios(i)=0.d0

L1:
    continue
next i
qp1(m+1) = 0.d0
qp2(m+1) = 0.d0

cnh - end subroutine

^*   initialise for first iteration
if(it = 1)then
ifirst=0
ilast=p%n
if(p%itbc = 2 and g%hold > 0.) then first=-1
cnh         if(p%ibbc = 0)gr=grad(g%t)
cnh now uses constant gradient from input file
if (p%ibbc = 0) then r = p%constant_gradient

if(p%ibbc = 1)then
cnh            g%psi(p%n)=potl(g%t)
cnh now uses constant potential from input file
g%psi(p%n) = p%constant_potential

g%p(p%n)=apswim_pf(g%psi(p%n))
end if
cnh added to allow seepage to user potential at bbc
ccnh - now deleted as it is pickup up below
c         if(p%ibbc = 3)then
c            g%psi(p%n) = p%constant_potential
c         end if

end if
^*   get soil water variables and their derivatives
for i=0 to p%n
    call apswim_watvar(i,g%p(i),g%psi(i),psip(i),psipp(i),g%th(i)
L8:
    continue
next i
^*   check boundary potls
if(p%itbc = 0 and p%isbc = 0 and g%psi(0) > 0.)then
*        infinite conductance and no ponding allowed
g%psi(0)=0.
g%p(0)=apswim_pf(g%psi(0))
call apswim_watvar(0,g%p(0),v1,psip(0),psipp(0),g%th(0),thp(0),
L1:
g%hk(0),hkp(0))
next i
end if
cnh added to allow seepage to user potential at bbc
cnh      if(p%ibbc = 3 and g%psi(p%n) > 0.)then
if(p%ibbc = 3 and g%psi(p%n) > p%constant_potential)then
*        seepage at bottom boundary
cnh         g%psi(p%n)=0.
g%psi(p%n)=p%constant_potential
g%p(p%n)=apswim_pf(g%psi(p%n))
call apswim_watvar(p%n,g%p(p%n),v1,psip(p%n),psipp(p%n)
end if
^*   get fluxes between nodes
absgf=abs(g%gf)
if( not doubles_are_equal(p%x(i-1),p%x(i)))then
deltax=p%x(i)-p%x(i-1)
deltap=g%p(i)-g%p(i-1)
hkd1=g%hk(i-1)*psip(i-1)
hkd2=g%hk(i)*psip(i)
hkdp1=g%hk(i-1)*psipp(i-1)+hkp(i-1)*psip(i-1)
hkdp2=g%hk(i)*psipp(i)+hkp(i)*psip(i)
skd=hkd1+hkd2
if(p%swt >=.5 and p%swt <=1.)then
*              use fixed space weighting on gravity flow
w1=sign(2.*p%swt,g%gf)
else
*              use central diffs for gravity flow if possible, else use
*                 just enough upstream weighting to avoid instability
*                 user may increase acceptable level for central diffs
*                 by setting p%swt < -1
accept=max(1d0,-p%swt)
wt=0.
c               if(absgf <> 0. and hkp(i) <> 0.)then
gfhkp = g%gf*hkp(i)
if(gfhkp <> 0.)then
if(it = 1)then
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

if ((w1 > 2.0) or (w1 < 0.0)) then
    call warning_error(err_internal
end if

g%q(i)=-0.5*(skd*deltap/deltax-g%gf*(w1*g%hk(i-1)
qp1(i)=-0.5*((hkdp1*deltap-skd)/deltax-g%gf*w1*hkp(i-1))
qp2(i)=-0.5*((hkdp2*deltap+skd)/deltax-g%gf*w2*hkp(i))

g%swf(i)= w1
end if
L10:
continue
^*   get fluxes to storage
for i=0 to p%n
    g%qs(i)=(g%th(i)-g%thold(i))*p%dx(i)/g%dt
    qsp(i)=thp(i)*p%dx(i)/g%dt
L20:
    continue
next i
next i
^*   get uptake fluxes to roots if still in iterations
if(iroots < 2)then
cnh         if(tisol = 1 and tslos <> 0.)then
cnh            do 22 i=0,p%n
cnh22          psios(i)=g%psi(i)-tslos*tcsl(i)
cnh            call uptake(psios,g%hk,psip,hkp,g%qex,qexp)
cnh         else
cnh            call uptake(g%psi,g%hk,psip,hkp,g%qex,qexp)
cnh         end if
cnh replaced with the following
psios(i) = g%psi(i)
for solnum=1 to nsol
    psios(i)=psios(i)-tslos(solnum)*tcsl(solnum,i)
L22:
    continue
next solnum
L23:
continue
call apswim_uptake(psios,g%hk,psip,hkp,g%qex,qexp)
cnh
end if
g%rex=0.
for i=0 to p%n
L25:
    g%rex=g%rex+g%qex(i)
next i

^*   nih  get subsurface fluxes
call apswim_drain(qdrain,qdrainpsi)

g%rssf = 0.
for i=0 to p%n
    g%qssif(i) = g%subsurfaceinflow(i)/10./24d0 ' assumes mm and daily timestep - need something better ''''
    g%qssof(i) = qdrain(i) ' add outflow calc here later
    qssofp(i) = qdrainpsi(i) * psip(i)
    g%rssf = g%rssf + g%qssif(i) - g%qssof(i)
L26:
    continue
next i

^*   get soil surface fluxes, taking account of top boundary condition
if(p%itbc = 0)then
^       infinite conductance
ifirst=0
if(g%psi(0) < 0.)then
hsoil=exp(hcon*g%psi(0))
g%res=g%resp*(hsoil-hair)/(1.-hair)
respsi=g%resp*hcon*hsoil/(1.-hair)
else
g%res=g%resp
respsi=0.
end if

if(p%isbc = 0)then
*           no ponding allowed
g%h=0.
q0=g%ron-g%res+g%hold/g%dt

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
if(g%psi(0) < 0.)then
g%h=0.
g%roff=0.
g%q(0)=g%ron-g%res+g%hold/g%dt
qp2(0)=-respsi*psip(0)
else
g%h=g%psi(0)
g%roff=0.
roffd=0.
if(p%isbc = 2)then
call apswim_runoff (g%t,g%h,g%roff,roffd)
end if
g%q(0)=g%ron-g%roff-g%res-(g%h-g%hold)/g%dt
qp2(0)=(-roffd-respsi-1./g%dt)*psip(0)
end if
end if
end if
if(p%itbc = 1)then
^       const potl
ifirst=1
if(g%psi(0) < 0.)then
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
if(p%itbc = 2)then
^       conductance given by a function
q0=g%ron-g%resp+g%hold/g%dt
if(p%isbc = 0)then
*           no ponding allowed
ifirst=0
g%h=0.
call apswim_scond(g%t,g%h,g_,gh)
if(q0 > -g_*g%psi(0))then
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
if(q0 > -g_*g%psi(0))then
*              initialise g%h if necessary
if(ifirst = 0) then %h=max(g%psi(0),0d0)
ifirst=-1
g%res=g%resp
g%roff=0.
roffd=0.
if(p%isbc = 2 and g%h > 0.)then
call apswim_runoff(g%t,g%h,g%roff,roffd)
end if
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
g%hbp=0.
qbps=0.
qbpsp=0.
if(p%ibp <> 0)then
if(g%psi(p%ibp) > 0.)then
*           allow for change in storage
g%hbp=g%psi(p%ibp)
qbps=(g%hbp-g%hbpold)*p%sbp/g%dt
qbpsp=psip(p%ibp)*p%sbp/g%dt
else
qbps=-g%hbpold*p%sbp/g%dt
end if
*           get bypass flow
if(g%psi(p%ibp) > 0.)then
g%qbp=p%gbp*(g%gf*(p%x(p%ibp)-p%x(0))-g%psi(p%ibp))
qbpp=-p%gbp*psip(p%ibp)
else
g%qbp=p%gbp*g%gf*(p%x(p%ibp)-p%x(0))
end if
if(g%roff < g%qbp)then
g%qbp=g%roff
qbpp=0.
g%qbpd=roffd
end if
g%roff=g%roff-g%qbp
end if
end if
^*   bottom boundary condition
if(p%ibbc = 0)then
^       zero matric potl gradient
g%q(p%n+1)=(g%gf+gr)*g%hk(p%n)
qp1(p%n+1)=(g%gf+gr)*hkp(p%n)
elseif(p%ibbc = 1)then
^       const potl
ilast=p%n-1
g%q(p%n+1)=g%q(p%n)-g%qs(p%n)-g%qex(p%n)+g%qssif(p%n)
if(p%ibp = p%n)then
g%q(p%n+1)=g%q(p%n+1)+g%qbp-qbps
g%qbpd=0.
end if
elseif(p%ibbc = 2)then
^       zero flux
g%q(p%n+1)=0.
qp1(p%n+1)=0.
elseif(p%ibbc = 3)then
^       seepage
cnh added to allow seepage to user potential at bbc
cnh         if(g%psi(p%n) >=0.)then
if(g%psi(p%n) >=p%constant_potential) then
    g%q(p%n+1)=g%q(p%n)-g%qs(p%n)-g%qex(p%n)+g%qssif(p%n)
    if(p%ibp = p%n) then %q(p%n+1)=g%q(p%n+1)+g%qbp
    if(g%q(p%n+1) >=0.)then
    ilast=p%n-1
    g%qbpd=0.
    else
    ilast=p%n
end if
end if
if(ilast = p%n)then
g%q(p%n+1)=0.
qp1(p%n+1)=0.
end if
elseif(p%ibbc = 4)then
^       flux calculated according to head difference from water table
headdiff = g%psi(p%n) - p%x(p%n) + p%water_table_depth/10d0   ' cm
g%q(p%n+1)= headdiff*p%water_table_conductance
qp1(p%n+1)=psip(p%n)*p%water_table_conductance
end if
^*   get newton-raphson equations
i1=max(ifirst,0)
k=i1-1
xidif= true
for i=i1 to last
    *        allow for two nodes at same depth
    xipdif= true
    if(xidif)then
    k=k+1
    j=i+1
loop
if(i > 0 and i < p%n-1)then
if(doubles_are_equal(p%x(i),p%x(i+1)))then
xipdif= false
j=i+2
g%q(i+1)=((p%x(j)-p%x(i))*g%q(i)+(p%x(i)-p%x(i-1))*
end if
end if
rhs(k)=-(g%q(i)-g%q(j))
a(k)=qp1(i)
b(k)=qp2(i)-qp1(j)
c_(k)=-qp2(j)
end if
rhs(k)=rhs(k)+g%qs(i)+g%qex(i)-g%qssif(i)+g%qssof(i)
b(k)=b(k)-qsp(i)-qssofp(i)
*        bypass flow?
if(p%ibp <> 0 and i = p%ibp)then
rhs(k)=rhs(k)-g%qbp+qbps
b(k)=b(k)+qbpp-qbpsp
end if
*        ggg
if(iroots = 0)then
*            a(k)=a(k)-qexp(1,i)
b(k)=b(k)-qexp(2,i)
*            c(k)=c(k)-qexp(3,i)
else
iroots=2
end if
xidif=xipdif
L30:
continue
next i
next i
ibegin=ifirst
iend=k
end sub
* =
function apswim_wpf ()
* =
*     gets water present in profile
use infrastructure
'implicit none

*     global variables

*     subroutine arguments
*     none

*     internal variables
Dim i as integer
Dim wpf as double

*     constant values
*     none

wpf=0.
L10:
wpf=wpf+g%th(i)*p%dx(i)

apswim_wpf = wpf

apswim_wpf=
end function
* =
sub  apswim_getsol(solnum,a,b,c_,d,rhs,c1,c2,fail)
* =
*     get and solve solute balance eqns

use infrastructure
'implicit none

*     global variables


*     subroutine arguments
' sub parameter : do not dim ! Dim solnum as integer
' sub parameter : do not dim ! Dim fail as boolean

*     internal variables
Dim accept as double
Dim aq as double
Dim cp as double
Dim csl0 as double
Dim csln as double
Dim d1 as double
Dim d2 as double
Dim dabs as double
Dim dfac as double
Dim dmax as double
Dim dum1 as double
Dim exco1 as double
Dim fq as double
Dim fqc as double
Dim crop as integer
Dim i as integer
Dim itcnt as integer
Dim j as integer
Dim k as integer
Dim kk as integer
Dim neq as integer
Dim nonlin as boolean
Dim rinf as double
Dim rovr as double
Dim rslout as double
Dim rslovr as double
Dim thav as double
Dim thi as double
Dim w1 as double
Dim w2 as double
Dim wt as double
Dim wtime as double
Dim wtime1 as double
Dim solute_bbc as integer

*     constant values
Dim itmax as integer
parameter

Dim constant_conc as integer
parameter (constant

Dim convection_only as integer
parameter (convection

*     determine type of solute bbc to use
if (p%ibbc = 1) then
'        ! water table boundary condition
    solute_bbc = constant_conc
    elseif (((p%ibbc = 0) or (p%ibbc = 4))
'        ! you have a gradient with flow upward
    solute_bbc = constant_conc
    else
    solute_bbc = convection_only
end if

*     surface solute balance - assume evap. (g%res) comes from x0 store
rovr=g%roff+g%qbp
rinf=g%q(0)+g%res
if(rinf > min(p%ersoil,p%ernode))then
g%cslsur(solnum)=(g%rslon(solnum)+g%hold*g%cslsur(solnum)/g%dt)
g%qsl(solnum,0)=rinf*g%cslsur(solnum)
rslovr=rovr*g%cslsur(solnum)
if(g%slsur(solnum) > 0.)then
if(g%cslsur(solnum) < p%slsci(solnum))then
g%qsl(solnum,0)=rinf*p%slsci(solnum)
g%slsur(solnum)=g%slsur(solnum)
else
g%qsl(solnum,0)=rinf*g%cslsur(solnum)
g%slsur(solnum)=0.
end if
end if
if(g%cslsur(solnum) < p%slscr(solnum))then
rslovr=rovr*p%slscr(solnum)
g%slsur(solnum)=g%slsur(solnum)
else
rslovr=rovr*g%cslsur(solnum)+g%slsur(solnum)/g%dt
g%slsur(solnum)=0.
end if
g%slsur(solnum)=g%slsur(solnum)
g%cslsur(solnum)=p%slscr(solnum)
else
if(g%h > 0) then %cslsur(solnum)=g%cslsur(solnum)
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
c1(i)=g%csl(solnum,i)
thi=g%th(i)
cnh         j=indxsl(solnum,i)
j = i
nonlin= false

*peter"s change 21/10/98 to ensure zero exchange is treated as linear
*         if (p%fip(solnum,j) = 1.) then
*           linear exchange isotherm
c2(i)=1.
exco1=p%ex(solnum,j)
else
*           nonlinear freundlich exchange isotherm
nonlin= true
c2(i)=0.
end sub
