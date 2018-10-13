! $Id$
#ifdef TESTCASES
MODULE module_sf_TestCases

IMPLICIT NONE

PUBLIC :: set_surface_fluxes

PRIVATE :: diag_ustar

PRIVATE ! default scope

CONTAINS

!===============================================================================
SUBROUTINE set_surface_fluxes( th_phy, pi_phy, u_phy, v_phy, z, rho,  &
                               qv_curr, z_at_w, t8w, psfc, dt,        &
                               itimestep, cu_physics, bl_pbl_physics, &
                               ust, hfx, qfx,                         &
                               ids, ide, jds, jde, kds, kde,          &
                               ims, ime, jms, jme, kms, kme,          &
                               its, ite, jts, jte, kts, kte           )

USE module_model_constants, only: &
    r_d,  & ! Gas constant for dry air    [J/(kg K)]
    EP_2, & ! R_d/R_v                     [-]
    cp,   & ! Specific heat of dry air    [J/(kg K)]
    XLV,  & ! Latent heat of vaporization [J/kg]
    g       ! Gravitational acceleration  [m/s^2]

USE module_state_description, only:  &
    BOMEXSCHEME,  &
    ARMSCHEME,    &
    FIRESCHEME,   &
    ATEXSCHEME,   &
    WANGARASCHEME

IMPLICIT NONE

! Input Variables
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

INTEGER, INTENT(IN):: cu_physics,    & ! Option for various test cases.
                      bl_pbl_physics   ! Option for PBL scheme.

INTEGER, INTENT(IN):: itimestep  ! Model time step number

REAL, INTENT(IN):: dt  ! Model time step  [s]

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
  th_phy,  & ! Potential Temperature (thermodynamic levels)     [K]
  pi_phy,  & ! Exner Function (thermodynamic levels)            [-]
  u_phy,   & ! U wind component (thermodynamic levels)          [m/s]
  v_phy,   & ! V wind component (thermodynamic levels)          [m/s]
  z,       & ! Thermodynamic-level altitudes                    [m]
  rho,     & ! Air density (thermodynamic levels)               [kg/m^3]
  qv_curr, & ! Water Vapor Mixing Ratio (thermodynamic levels)  [kg/kg]
  z_at_w,  & ! Momentum-level altitudes                         [m]
  t8w        ! Temperature interp./exten. to momentum levels    [K]

REAL, DIMENSION(ims:ime, jms:jme), INTENT(IN)::  &
  psfc   ! Surface pressure                 [Pa]

! Output Variables
REAL, DIMENSION(ims:ime, jms:jme), INTENT(OUT)::   &
  ust, & ! Surface friction velocity, u*    [m/s]
  hfx, & ! Surface heat flux                [W/m^2]
  qfx    ! Surface moisture flux            [kg/(m^2 s)]

! Local Variables
REAL:: &
  rho_sfc,       & ! Total density at the surface                 [kg/m^3]
  rv_sfc,        & ! Surface total water mixing ratio (lin. ext.) [kg/kg]
  w1,            & ! Weighting factor 1 in linear extension       [-]
  w2,            & ! Weighting factor 2 in linear extension       [-]
  time,          & ! Time elapsed since midnight on start date    [s]
  wind_speed,    & ! Wind speed at lowest thermodynamic level     [m/s]
  heat_flux,     & ! Surface heat flux in K m/s                   [K m/s]
  moisture_flux, & ! Surface moisture flux in (kg/kg) m/s         [(kg/kg) m/s]
  H,             & ! Surface heat flux in W/m^2 (ARM case)        [W/m^2]
  LE,            & ! Surface latent heat flux in W/m^2 (ARM case) [W/m^2]
  z0,            & ! Surface roughness length                     [m]
  C_10,          & ! Bulk exchange coefficient for height z=10 m. [-]
  C_h,           & ! Bulk exchange coefficient for any height     [-]
  bflx,          & ! Surface buoyancy flux                        [m^2/s^3]
  cpm              ! Specific heat of moist air                   [J/(kg K)]

INTEGER:: i_start, i_end, j_start, j_end, i, j

REAL, PARAMETER:: PI = 3.141592654
REAL, PARAMETER:: min_wind = 0.01

!--------------------------------------------------------------------

i_start = its
i_end   = MIN(ite,ide-1)
j_start = jts
j_end   = MIN(jte,jde-1)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1

      wind_speed = MAX( SQRT( u_phy(i,kts,j)**2 + v_phy(i,kts,j)**2 ), &
                        min_wind )

      ! Calculate the value of total density (rho) at the surface.
      ! The method is based on whether CLUBB is calling a PBL scheme.
      IF ( bl_pbl_physics > 0 ) THEN

         ! Calculate rho_sfc at the surface itself (momentum level k = 1), based
         ! on total air pressure (p), water vapor mixing ratio (r_v), and
         ! temperature (T), such that:
         ! rho = p / { [ ((R_d/R_v)+r_v) / ((R_d/R_v)*(1+r_v)) ] * R_d * T };
         ! where R_d is the gas constant for dry air and R_v is the gas constant
         ! for water vapor.  The value of water vapor mixing ratio at the
         ! surface is found through a linear extension of it's values at
         ! thermodynamic levels 1 and 2.
         !
         ! Most surface schemes and PBL schemes use the actual surface value of
         ! rho when outputing or inputing surface heat flux and moisture flux.

         ! Weighting factors for linear extension of water vapor mixing ratio to
         ! the surface.
         w1 = ( z_at_w(i,kts,j) - z(i,kts+1,j) ) / ( z(i,kts,j) - z(i,kts+1,j) )
         w2 = 1.0 - w1

         ! Calculate water vapor mixing ratio at the surface.
         rv_sfc = MAX( w1*qv_curr(i,kts,j) + w2*qv_curr(i,kts+1,j) , 0.0 )

         ! Calculate total density (rho) at the surface.
         rho_sfc  &
            = psfc(i,j)  &
              / ( ((EP_2+rv_sfc)/(EP_2*(1.0+rv_sfc))) * r_d * t8w(i,kts,j) )

         ! Calculate cpm by using the value of water vapor mixing ratio at
         ! the model surface.
         cpm = cp * ( 1.0 + 0.8 * rv_sfc )

      ELSE

         ! Calculate rho_sfc by using the value at thermodynamic level k = 1.
         !
         ! If a PBL scheme is not is use, WRF applies surface fluxes in
         ! subroutine vertical_diffusion_2, which simply uses the value of rho
         ! from the first thermodynamic level, which is above the surface.

         rho_sfc = rho(i,kts,j)

         ! Calculate cpm by using the value of water vapor mixing ratio at
         ! thermodynamic level k = 1.
         cpm = cp * ( 1.0 + 0.8 * qv_curr(i,kts,j) )

      ENDIF


      IF ( cu_physics > 0 ) THEN

         SELECT CASE ( cu_physics )

         CASE (BOMEXSCHEME)

            ! Surface fluxes for GCSS BOMEX case.

            ! Constant u* for the BOMEX case.
            ust(i,j) = 0.28

            ! Constant BOMEX heat fluxes.
            heat_flux = 8.0e-3

            ! The BOMEX specifications give surface moisture flux in terms of
            ! total water specific humidity.
            moisture_flux = 5.2e-5

            ! Convert flux from terms of total water specific humidity to terms
            ! of total water mixing ratio.
            moisture_flux = ( 1.0 + qv_curr(i,kts,j) )**2 * moisture_flux

            ! Convert heat flux, with units of K m/s, to WRF's HFX with units
            ! of W / m^2.
            hfx(i,j) = heat_flux * cpm * rho_sfc

            ! Convert moisture flux, with units of (kg/kg) m/s, to WRF's QFX
            ! with units of kg / (m^2 s).
            qfx(i,j) = moisture_flux * rho_sfc


         CASE (ARMSCHEME)

            ! Surface fluxes for GCSS ARM case.

            ! Find the time (in seconds since midnight on the day of the start
            ! of the model run) to use to determine surface sensible (H) and
            ! latent (LE) heat fluxes.
            time = dt*itimestep + 41400.0

            ! Sensible and latent heat fluxes, both in units of W / m^2.
            IF (time >= 41400.0 .and. time <= 55800.0) THEN
               H  = (( 90.0+30.0)/(55800.0-41400.0))*(time-41400.0) - 30.0
               LE = (( 250.0-5.0)/(55800.0-41400.0))*(time-41400.0) + 5.0
            ELSEIF (time > 55800.0 .and. time <= 64800.0) THEN
               H  = (( 140.0-90.0)/(64800.0-55800.0))*(time-55800.0) + 90.0
               LE = (( 450.0-250.0)/(64800.0-55800.0))*(time-55800.0) + 250.0
            ELSEIF (time > 64800.0 .and. time <= 68400.0) THEN
               H  = (( 140.0-140.0)/(68400.0-64800.0))*(time-64800.0) + 140.0
               LE = (( 500.0-450.0)/(68400.0-64800.0))*(time-64800.0) + 450.0
            ELSEIF (time > 68400.0 .and. time <= 77400.0) THEN
               H  = (( 100.0-140.0)/(77400.0-68400.0))*(time-68400.0) + 140.0
               LE = (( 420.0-500.0)/(77400.0-68400.0))*(time-68400.0) + 500.0
            ELSEIF (time > 77400.0 .and. time <= 86400.0) THEN
               H  = ((-10.0-100.0)/(86400.0-77400.0))*(time-77400.0) + 100.0
               LE = (( 180.0-420.0)/(86400.0-77400.0))*(time-77400.0) + 420.0
            ELSEIF (time > 86400.0 .and. time <= 93600.0) THEN
               H  = ((-10.0+10.0)/(93600.0-86400.0))*(time-86400.0) - 10.0
               LE = (( 0.0-180.0)/(93600.0-86400.0))*(time-86400.0) + 180.0
            ENDIF

            ! Sensible heat flux, H, has the same units as WRF's HFX, which are
            ! W / m^2.
            hfx(i,j) = H

            ! Convert latent heat flux, LE, with units of W / m^2, to WRF's QFX,
            ! with units of kg / (m^2 s).
            qfx(i,j) = LE / XLV

            ! u* for the ARM case.

            ! Surface roughness length.
            z0 = 0.035

            ! The equation for surface buoyancy flux uses heat flux in K m/s.
            heat_flux = hfx(i,j) / ( cpm * rho_sfc )

            ! Surface buoyancy flux in m^2/s^3.
            bflx = ( g / th_phy(i,kts,j) ) * heat_flux

            ! Diagnose u*.
            !
            ! The variable z(i,kts,j) is the height of first thermodynamic level
            ! (above the surface), which is where the winds are located on the
            ! vertical grid.
            ust(i,j) = diag_ustar( z(i,kts,j), bflx, wind_speed, z0 )


         CASE (FIRESCHEME)

            ! Surface fluxes for GCSS FIRE case.

            ! Constant u* for the FIRE case.
            ust(i,j) = 0.3

            ! Surface roughness length.
            z0 = 0.0002

            ! Exchange coefficient (dimensionless) for height z = 10 m.
            C_10 = 0.0013

            ! Bulk exchange coefficient
            !
            ! The variable z(i,kts,j) is the height of first thermodynamic level
            ! (above the surface), which is where potential temperature and
            ! moisture are located on the vertical grid.
            C_h = C_10 * ( LOG(10.0/z0) / LOG(z(i,kts,j)/z0) )**2

            ! The heat and moisture fluxes for the FIRE case are calculated
            ! using a simple bulk aerodynamic formula.
            ! The Sea Surface Temperature is specified to be 288 K, and the
            ! surface water vapor mixing ratio is specified to be 10.67 g/kg.
            heat_flux = - C_h * wind_speed  &
                              * ( th_phy(i,kts,j) - 288.0/pi_phy(i,kts,j) )
            moisture_flux = - C_h * wind_speed  &
                                  * ( qv_curr(i,kts,j) - 0.01067 )

            ! Convert heat flux, with units of K m/s, to WRF's HFX with units
            ! of W / m^2.
            hfx(i,j) = heat_flux * cpm * rho_sfc

            ! Convert moisture flux, with units of (kg/kg) m/s, to WRF's QFX
            ! with units of kg / (m^2 s).
            qfx(i,j) = moisture_flux * rho_sfc


         CASE (ATEXSCHEME)

            ! Surface fluxes for GCSS ATEX case.

            ! Constant u* for the ATEX case.
            ust(i,j) = 0.3

            ! Surface roughness length.
            z0 = 0.00015

            ! Exchange coefficient (dimensionless) for height z = 10 m.
            C_10 = 0.0013

            ! Bulk exchange coefficient
            !
            ! The variable z(i,kts,j) is the height of first thermodynamic level
            ! (above the surface), which is where potential temperature and
            ! moisture are located on the vertical grid.
            C_h = C_10 * ( LOG(10.0/z0) / LOG(z(i,kts,j)/z0) )**2

            ! The heat and moisture fluxes for the ATEX case are calculated
            ! using a simple bulk aerodynamic formula.
            ! The Sea Surface Temperature is specified to be 298 K, and the
            ! surface water vapor mixing ratio is specified to be 19.8293 g/kg.
            heat_flux = - C_h * wind_speed  &
                              * ( th_phy(i,kts,j) - 298.0/pi_phy(i,kts,j) )
            moisture_flux = - C_h * wind_speed  &
                                  * ( qv_curr(i,kts,j) - 0.0198293 )

            ! Convert heat flux, with units of K m/s, to WRF's HFX with units
            ! of W / m^2.
            hfx(i,j) = heat_flux * cpm * rho_sfc

            ! Convert moisture flux, with units of (kg/kg) m/s, to WRF's QFX
            ! with units of kg / (m^2 s).
            qfx(i,j) = moisture_flux * rho_sfc


         CASE (WANGARASCHEME)

            ! Surface fluxes for the Wangara case.

            ! Constant u* for the WANGARA case.
            ust(i,j) = 0.13

            ! Wangara starts at 9:00 a.m. Australia EST (23:00Z).
            ! Therefore, it starts 32,400 seconds into the day.
            ! It runs until 5:00 p.m. Australia EST.
            ! Therefore, it runs until 61,200 seconds into the day.

            time = dt*itimestep + 32400.0

            ! The heat and moisture fluxes for the WANGARA case.
            heat_flux = 0.18 * COS( ( (time-45000.0) / 36000.0 ) * PI )
            moisture_flux = 0.00013 * heat_flux

            ! Convert heat flux, with units of K m/s, to WRF's HFX with units
            ! of W / m^2.
            hfx(i,j) = heat_flux * cpm * rho_sfc

            ! Convert moisture flux, with units of (kg/kg) m/s, to WRF's QFX
            ! with units of kg / (m^2 s).
            qfx(i,j) = moisture_flux * rho_sfc


         END SELECT


      ELSE

         ust(i,j) = 0.0
         hfx(i,j) = 0.0
         qfx(i,j) = 0.0

      ENDIF

   ENDDO
ENDDO

END SUBROUTINE set_surface_fluxes

!===============================================================================
!
! DISCLAIMER : this code appears to be correct but has not been
!              very thouroughly tested. If you do notice any
!              anomalous behaviour then please contact Andy and/or
!              Bjorn
!
! Function diag_ustar:  returns value of ustar using the below
! similarity functions and a specified buoyancy flux (bflx) given in
! kinematic units
!
! phi_m (zeta > 0) =  (1 + am * zeta)
! phi_m (zeta < 0) =  (1 - bm * zeta)^(-1/4)
!
! where zeta = z/lmo and lmo = (theta_rev/g*vonk) * (ustar^2/tstar)
!
! Ref: Businger, 1973, Turbulent Transfer in the Atmospheric Surface
! Layer, in Workshop on Micormeteorology, pages 67-100.
!
! Code writen March, 1999 by Bjorn Stevens
!
real function diag_ustar( z, bflx, wnd, z0 ) 

implicit none

real, parameter      :: am   =  4.8   !   "          "         "
real, parameter      :: bm   = 19.3   !   "          "         "

real, intent (in)    :: z             ! height where u locates
real, intent (in)    :: bflx          ! surface buoyancy flux (m^2/s^3)
real, intent (in)    :: wnd           ! wind speed at z
real, intent (in)    :: z0            ! momentum roughness height

integer :: iterate
real    :: lnz, klnz, c1, x, psi1, zeta, lmo, ustar

! Constant Parameters
real, parameter :: & 
  grav = 9.81,        & ! Gravitational acceleration             [m/s^2]
  pi   = 3.141592654, & ! The ratio of radii to their circumference
  vonk = 0.4            ! Accepted value is 0.40 (+/-) 0.01      [-]

lnz   = log( z / z0 )
klnz  = vonk/lnz
c1    = pi / 2.0 - 3.0*log( 2.0 )

ustar =  wnd*klnz
!if (bflx /= 0.0) then
if (abs(bflx) > 1.e-6) then
!if (abs(bflx) > 1.e-4) then
  do iterate=1,4
!    lmo   = -bflx * vonk/(ustar**3 + eps)
    lmo   = -ustar**3 / ( vonk * bflx )
    zeta  = z/lmo
    if (zeta > 0.) then
      ustar =  vonk*wnd  /(lnz + am*zeta)
    else
      x     = sqrt( sqrt( 1.0 - bm*zeta ) )
      psi1  = 2.*log( 1.0+x ) + log( 1.0+x*x ) - 2.*atan( x ) + c1
      ustar = wnd*vonk/(lnz - psi1)
    end if
  end do
end if

diag_ustar = ustar

return
end function diag_ustar

!===============================================================================

END MODULE module_sf_TestCases
#endif /*TESTCASES*/
