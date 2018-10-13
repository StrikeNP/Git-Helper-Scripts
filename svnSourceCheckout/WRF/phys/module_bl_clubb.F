!-------------------------------------------------------------------------------
! $Id: module_bl_clubb.F 1011 2018-07-20 20:47:04Z bmg2@uwm.edu $
!===============================================================================


#ifdef CLUBB
MODULE module_bl_clubb
! This module contains the following subroutines and functions:
!
! 1) SUBROUTINE CLUBB_bl_scheme
!    This subroutine is called every timestep in order to provide results
!    from the CLUBB parameterization for use in WRF code.
!
! 2) SUBROUTINE output_tendencies
!    This subroutine calculates tendencies for u, v, theta, qvapor, and 
!    qcloud to send back into WRF.
!
! 3) SUBROUTINE mp_CLUBB_pdf_closure
!    (removed in wrf:r560)
!    This subroutine is called from a WRF microphysics scheme.  It calls 
!    CLUBB's pdf_closure in order to account for subgrid variability
!    in cloud water mixing ratio and cloud fraction. 
!
! 4) FUNCTION LIN_EXT
!    This is a simple linear extension computation to use for any variable.
!
! 5) FUNCTION LIN_INT
!    This is a simple linear interpolation computation to use for any variable.
!    It is not currently being referenced in the code.
!
! 6) SUBROUTINE CLUBB_bl_init
!    This is a subroutine that is called once in the initialization part of
!    WRF.  It is important because it provides some essential initialization
!    information for some CLUBB variables (mainly wp2) that need to be
!    initialized before they are first called in parameterization_timestep
!    from SUBROUTINE CLUBB_BL_SCHEME.
!
!-------------------------------------------------------------------------------
!
!!!!  WRF VS. CLUBB vertical grid comparison.
!
!
!          WRF VERTICAL GRID            !          CLUBB VERTICAL GRID
!          #################            !          ###################
!                                       !
!                                       !
!     TOP ======= Momentum (top)        !     TOP ======= Momentum (top)
!                                       !
!                                       !
!         ------- Thermodynamic (top-1) !         ------- Thermodynamic (top)
!                                       !
!                                       !
!         ======= Momentum (top-1)      !         ======= Momentum (top-1)
!            .                          !            .
!            .                          !            .
!            .                          !            .
!            .                          !            .
!            .                          !            .
!         ======= Momentum (k+1)        !         ======= Momentum (k+1)
!                                       !
!                                       !
!                                       !
!         ------- Thermodynamic (k)     !         ------- Thermodynamic (k+1)
!                                       !
!                                       !
!                                       !
!         ======= Momentum (k)          !         ======= Momentum (k)
!            .                          !            .
!            .                          !            .
!            .                          !            .
!            .                          !            .
!            .                          !            .
!         ======= Momentum (2)          !         ======= Momentum(2)
!          *   |                        !          *   |
!          *   | 1/2 delta zw           !          *   | 1/2 delta zw
! delta zw *   |                        ! delta zw *   |
!         -*----- Thermodynamic (1)     !         -*----- Thermodynamic (2)
!          *   |                        !          *   |
!          *   | 1/2 delta zw           !          *   | 1/2 delta zw
!          *   |                        !          *   |
! SURFACE ======= Momentum (1)          ! SURFACE ======= Momentum (1)
!        / / / /                        !        / / / /
!       / / / /                         !       / / / /
!                                       ! 
!                                       !         ------- Thermodynamic (1)
!
!===============================================================================
!
!!!! Note on vertical advection in WRF-CLUBB interface.
!
! The vertical advection equation:  - w * dQ/dz; where Q is some property.
!
! When it is by itself, CLUBB handles this term for all the mean and the
! turbulent quantities.
!
! In the WRF-CLUBB interface, CLUBB handles the advection of all the turbulent
! quantities, for they don't appear in WRF.  However, this is not the case
! for the mean quantities.
!
! w = w_lss + w_WRF
!
! Vertical velocity is equal to the imposed large-scale subsidence (w_lss)
! specified for each individual test case, plus the WRF resolved vertical
! velocity (w_WRF or w_2(i,k,j) in the WRF code).
!
! In its code, WRF already handles the vertical advection of the mean 
! quantities due to its own resolved vertical velocity.  In addition, the
! vertical advection of mean quantities due to specified large-scale
! subsidence is handled by the cumulus physics scheme created for each
! individual test case.
!
! Therefore, for mean quantities, the CLUBB code needs to show:
! 
! w = 0
!
! and for the turbulent quantities, the CLUBB code needs to show:
!
! w = w_lss + w_WRF
!
!===============================================================================


use clubb_api_module, only:  &
    time_precision, & ! Constant(s)
    core_rknd, &
    hydromet_dim  ! Variable(s)

IMPLICIT NONE

PUBLIC :: &
  CLUBB_bl_scheme, &
#ifdef CLUBBSTATS
  l_keep_CLUBB_stats, &
  l_sample_patch, &
#endif /*CLUBBSTATS*/
  CLUBB_bl_init

PRIVATE ! Default scope

!  #############################################################################
!                         SET CLUBB CONSTANTS HERE!!!!!

! Flag for CLUBB being implemented as part of a host model (WRF in this case).
logical, parameter, private:: l_implemented = .true.  ! Always true

logical, parameter, private ::  &
  l_host_applies_sfc_fluxes = .false., &  ! Host model applies surface fluxes
  l_uv_nudge  = .false.                   ! Use u/v nudging (not used)

real, parameter, private :: &
  theta0   = 300.0,   &  ! Reference temperature                     [K]
  ts_nudge = 86400.0     ! Time scale for u/v nudging (not used)     [s]

integer, private ::  &
  sclr_dim     = 0, &  ! Number of scalar variables.
  edsclr_dim   = 0     ! Number of eddy-scalar variables.

! This flag will be reset to .false. once an initial pass has been made through
! the code (in subroutine CLUBB_bl_init) that relates to the call to CLUBB's
! subroutine setup_clubb_core.  This is done so that CLUBB's setup_clubb_core is
! called only ONCE.  Otherwise, the CLUBB model code will attempt to allocate
! variables more than once, and therefore produce an error which stops the
! WRF-CLUBB model run.
logical, private :: &
  l_first_call_to_CLUBB_bl_init = .true.

! The variables grid_type, deltaz, and zm_init are not used in WRF-CLUBB
! interface because l_implemented is set to .true.  Set the three variables
! to arbitrary acceptable values, for they are part of the call list.
integer, parameter, private :: grid_type = 3
real, parameter, private :: deltaz  = 10.0,  &
                            zm_init = 0.0,  &
                            zm_top = 0.0

#ifdef CLUBBSTATS
! Added for CLUBB statistics
! Flag to keep CLUBB to its statistics for a single column
logical, parameter ::  &
  l_keep_CLUBB_stats = .true.

! This flag is set to true for the patch which contains the CLUBB stats column
! for parallel jobs.  It is then referenced in wrf_finalize so that only the
! patch which contains the stats column will call subroutine stats_finalize.
! It will always be true for a serial run, since there is only one patch.
! It is initialized to false here.   -- ldgrant Sept 2010
logical :: l_sample_patch = .false.
!$OMP threadprivate(l_sample_patch)

! This flag will be reset to .false. once an initial pass has been made through
! the code (in subroutine CLUBB_bl_init) that relates to the call to CLUBB's
! subroutine stats_init.  This is done so that CLUBB's stats_init is called only
! ONCE.  Otherwise, the CLUBB model code will attempt to allocate variables more
! than once, and therefore produce an error which stops the WRF-CLUBB model run.
logical, private ::  &
  l_1st_call_to_CLUBB_stats_init = .true.
!$OMP threadprivate(l_1st_call_to_CLUBB_stats_init)

! Declare time_restart here because it is referenced by both subroutines
! CLUBB_bl_init and CLUBB_bl_scheme, but it is not passed between them.
real (kind=time_precision) :: &
  time_restart   ! Initial time of restart run  [s]

! Declare a column (i,j) for CLUBB to keep its statistics.

! First declare a WRF domain for that column.  The WRF main domain is 1.  In a
! scenario where a nested grid is used, there is more than one available domain.
integer, parameter ::  &
  set_sample_domain = 1   ! WRF domain for WRF column for CLUBB stats to sample

! The column within the aforementioned domain are defined in namelist.input as
! the variables clubb_i_sample_col and clubb_j_sample_col.
! If the column declared is outside of the i or j domains, then the column
! selected for CLUBB stats will be the central column, or in a column that
! borders the center of the selected model domain.
! To setup CLUBB statistics to automatically use a central or border-central
! column, set variables set_i_sample_col and set_j_sample_col to 0.
integer ::  &
  set_i_sample_col,    & ! WRF column for CLUBB stats to sample; i-coordinate
  set_j_sample_col       ! WRF column for CLUBB stats to sample; j-coordinate

! The user-selected values for the WRF domain and/or column are sometimes
! invalid (and sometimes by design -- for example, setting both set_i_sample_col
! and set_j_sample_col to 0 in order to automatically select a central or
! border-central column).  These variables hold the values of the WRF domain and
! column that are actively being used for CLUBB statistics.
integer, public ::  &
  sample_domain, & ! WRF domain for WRF column for CLUBB stats to sample
  i_sample_col,  & ! WRF column that CLUBB stats is sampling; i-coordinate
  j_sample_col     ! WRF column that CLUBB stats is sampling; j-coordinate
!$OMP threadprivate(sample_domain, i_sample_col, j_sample_col)
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

#ifdef SILHS
  integer, save :: lh_iter = 1
!$OMP threadprivate(lh_iter)

  ! Flag which indicates if this is the first call to 
  ! generate_silhs_sample_api () or not
  logical, private :: &
    l_first_call_to_clubb_bl_scheme = .true.
!$OMP threadprivate(l_first_call_to_clubb_bl_scheme)


  integer, parameter :: &
    lh_microphys_interactive = 1, &
    lh_microphys_non_interactive = 2, &
    lh_microphys_disabled = 3

  integer :: lh_microphys_type
!$omp threadprivate( lh_microphys_type )

  logical, parameter :: & 
    l_calc_weights_all_levs = .true. ! .false. if all time steps use the same weights at all grid
                                     ! levels

  logical :: &
    l_calc_weights_all_levs_itime, & ! .true. if we calculate sample weights separately at all
                                     ! grid levels at the current time step  
    l_rad_itime ! .true. if we calculate radiation at the current time step 

#endif  /*SILHS*/
!  #############################################################################
!===============================================================================

CONTAINS

SUBROUTINE CLUBB_bl_scheme( DT, DTBL, flag_qc, isfflx,                         &
                            tke_heat_flux, tke_drag_coefficient,               &
                            z_at_w, dz8w, z, u_phy, v_phy, rho_in,             &
                            th_phy, p_phy, pi_phy, alb, t_init,                &
                            qv_curr, qc_curr, w_2, t8w, ph_2, ph_1,            &
                            psfc, hfx, qfx, ust, DX, DY,                       &
                            num_force_layers, scm_force,                       &
                            z_force, w_subs,                                   &
#ifdef CLUBBSTATS
                            ! Added for CLUBB statistics
                            grid_id, ITIMESTEP,                                &
                            start_hour, start_minute, start_second,            &
                            ! CLUBB statistics changes end
#endif /*CLUBBSTATS*/
                            wpthlp_sgs, wprtp_sgs, wp2_sgs,                    &
                            rtp2_sgs, thlp2_sgs, rtpthlp_sgs,                  &
                            up2_sgs, vp2_sgs, upwp_sgs, vpwp_sgs,              &
                            wpthvp_sgs, rtpthvp_sgs, thlpthvp_sgs,             &
                            wprcp_sgs, wpthp_sgs,                              &
                            wp3_sgs, wp2thvp_sgs, rcm_CLUBB, cf_CLUBB,         &
                            rcm_lyr_CLUBB, cldcov_CLUBB,                       &
                            pdf_params_CLUBB, pdf_params_zm_CLUBB,             &
                            l_use_cldcov_rcm_lyr,                              &
                            RUBLTEN, RVBLTEN, RTHBLTEN, RQVBLTEN, RQCBLTEN,    &
                            qr, qnr, qndrop,                                   &
                            qs, qns, qi, qni,                                  &
                            qg, qng,                                           &
                            exner, rho_ds_zt, rho_ds_zm,                       &
#ifdef SILHS
                            th_lh, w_lh, qv_lh, qc_lh, qr_lh, qnr_lh,          &
                            qndrop_lh, qs_lh, qns_lh, qi_lh, qni_lh,           &
                            qg_lh, qng_lh,                                     &
                            th_lh_mc, qv_lh_mc,                                &
                            weights_lh, lh_microphys_calls,                    &
                            lh_seed, lh_seed_len, restart,                     &
#endif /*SILHS*/
                            ids, ide, jds, jde, kds, kde,                      &
                            ims, ime, jms, jme, kms, kme,                      &
                            its, ite, jts, jte, kts, kte                       )

!-------------------------------------------------------------------------------

! Description:
! Calls the CLUBB PBL scheme for every grid column.  Advances the variables that
! CLUBB prognoses (wpthlp, wprtp, wp2, wp3, rtp2, thlp2, rtpthlp, up2, and vp2)
! one PBL time step.  Outputs tendencies back to WRF for theta, qvapor, qcloud,
! u, and v.

! CLUBB modules that need to be referenced by this subroutine.
use clubb_api_module, only:  &
    advance_clubb_core_api, & ! Procedure(s)
    setup_grid_heights_api, & ! Procedures
    zt2zm_api, &

    gr, & ! Variable(s)

    adj_low_res_nu_api  ! Procedure(s)

#ifdef CLUBBSTATS
! Added for CLUBB statistics
use clubb_api_module, only: &
    fstdout, &    ! Constant
    sec_per_hr, &
    sec_per_min
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

use clubb_api_module, only: &
    fstderr, &  ! Constant
    w_tol_sqd, &

    l_use_boussinesq, &

    clubb_no_error, & ! Constant

    pdf_parameter,         & ! Variable type
    pack_pdf_params_api,   & ! Procedure(s)
    unpack_pdf_params_api, &

  iirr, iiNr, iirs, iiri, iirg, & ! Variables
  iiNs, iiNi, iiNg

#ifdef SILHS
use silhs_api_module, only: &
    l_lh_importance_sampling

use clubb_api_module, only: &
    cm3_per_m3,     & ! Constant(s)
    zero_threshold, &

    Lscale, & ! Variable(s)

    corr_array_n_cloud, &
    corr_array_n_below, &

    iiPDF_chi, & ! Variable(s)
    iiPDF_w, &
    iiPDF_rr, &
    iiPDF_rs, &
    iiPDF_ri, &
    iiPDF_rg, &
    iiPDF_Nr, &
    iiPDF_Ns, &
    iiPDF_Ni, &
    iiPDF_Ncn, &
    iiPDF_Ng, &
    pdf_dim, &

    hydromet_pdf_parameter, &  ! Variable(s)

    setup_pdf_parameters_api, &  ! Procedure(s)

    fill_holes_driver_api ! Procedure(s)

use clubb_api_module, only : assignment( = )

use clubb_api_module, only : &
    genrand_state, &
    genrand_srepr, &
    genrand_intg, &
    genrand_init_api


use silhs_api_module, only: &
    generate_silhs_sample_api, & ! Procedure(s)
    stats_accumulate_lh_api, &
    lh_clipped_variables_type, &
    clip_transform_silhs_output_api

#endif /*SILHS*/

#ifdef CLUBBSTATS
! Added for CLUBB statistics
use clubb_api_module, only:  &
    l_stats, &       ! Flags
    l_stats_samp, &
    l_stats_last, &

    stats_begin_timestep_api, & ! Procedures
    stats_end_timestep_api, &

    stats_tsamp, & ! Variable(s)
    stats_tout, &

    stats_accumulate_hydromet_api ! Procedure

! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

! WRF modules that need to be referenced by this subroutine.
USE module_init_utilities, ONLY: interp_0
USE module_model_constants
USE module_state_description, ONLY: &
    num_pdf_params_CLUBB, &
    num_pdf_params_zm_CLUBB, &
    PASSIVEQV

IMPLICIT NONE

#ifdef SILHS
integer, parameter :: lh_sequence_length = 1
#endif

! Indices for the domain, memory, and tile
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte
#ifdef SILHS
INTEGER, INTENT(IN):: lh_microphys_calls

type(genrand_state) :: & 
  mt95_state ! State of the pseudo random number generator (internal representation) 

type(genrand_srepr) :: & 
  mt95_srepr ! State of the pseudo random number generator (for output)

INTEGER, DIMENSION(ims:ime,jms:jme,5000), INTENT(INOUT) :: &
  lh_seed ! Seed for the random number generator of SILHS

INTEGER, INTENT(INOUT) :: &
  lh_seed_len ! Length of the seed (# of characters)

INTEGER :: &
  lo ! Index for loop to convert the seed string to character array

LOGICAL, INTENT(IN):: &
  restart,             & ! Flag For Model Restart Run
  l_use_cldcov_rcm_lyr   ! If true, use rcm_lyr_CLUBB and cldcov_CLUBB 
                         ! instead of rcm_CLUBB and cf_CLUBB 

integer(kind=genrand_intg), PARAMETER :: &
  lh_seed_init = 5489_genrand_intg  ! seed to initialize the random number generators 
                                    ! in the first timestep

integer(kind=genrand_intg) :: &
  lh_seed_tmp                       ! Seed actual used to initialize random numner generator
                                    ! lh_seed_tmp = lh_seed_init + i + j

#endif /*SILHS*/

! Standard WRF variables to be brought into this subroutine.
REAL, INTENT(IN)::  &
      DT   ! Duration of model time step   [s]

! When bldt is set to 0, the PBL scheme is called every time step,
! and DTBL = DT.  Otherwise, DTBL depends on the ratio of 60.0*bldt to DT.
REAL, INTENT(IN)::  &
      DTBL ! Duration of PBL time step   [s]

LOGICAL, INTENT(IN)::  &
      flag_qc  ! Flag for WRF's microphysics scheme containing QC.

! Configuration flags
INTEGER, INTENT(IN)::  &
      isfflx        ! surface flux selection.

! WRF namelist-specified variables.
REAL, INTENT(IN)::  &
      tke_heat_flux,        & ! fixed surface heat flux (isfflx = 0 or 2).
      tke_drag_coefficient    ! constant drag coefficient (isfflx = 0).

! Heights of WRF Grid Points.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      z_at_w, & ! height at full-level of z.
      dz8w,   & ! delta-z between full-levels of z.
      z         ! height at half-level of z.

! Standard WRF variables to be brought into this subroutine.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      u_phy,   & ! u-wind at half-level of x
      v_phy,   & ! v-wind at half-level of y
      rho_in,  & ! density (dry air + vapor)
      th_phy,  & ! theta
      p_phy,   & ! pressure
      pi_phy,  & ! exner function (p/p0)^(Rd/cp)
      alb,     & ! Inverse base-state dry density
      t_init,  & ! Base-state (initial) perturbation theta
      qv_curr, & ! water vapor mixing ratio
      qc_curr, & ! cloud water mixing ratio
      w_2,     & ! w-wind at momentum levels
      t8w,     & ! temperature at momentum levels
      ph_2,    & ! perturbation geopotential
      ph_1       ! perturbation geopotential at previous model timestep

! Mean microphysical variables
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      qr,       & ! Rain droplet number concentration   [kg/kg]
      qnr,      & ! Rain number concentration           [#/kg]
      qndrop,   & ! Cloud droplet number concentration  [#/kg]
      qs,       & ! Snow mixing ratio                   [kg/kg]
      qns,      & ! Snow number concentration           [#/kg]
      qi,       & ! Cloud ice mixing ratio              [kg/kg]
      qni,      & ! Cloud ice number concentration      [#/kg]
      qg,       & ! Cloud graupel mixing ratio          [kg/kg]
      qng         ! Cloud graupel number concentration  [#/kg]

#ifdef SILHS
! Latin hypercube samples of fields
REAL, DIMENSION(ims:ime,kms:kme,jms:jme,lh_microphys_calls), INTENT(OUT)::  &
      th_lh,       & ! Potential temperature               [K]
      w_lh,        & ! Vertical velocity                   [kg/kg]
      qc_lh,       & ! Cloud water mixing ratio            [kg/kg]
      qv_lh,       & ! Vapor water mixing ratio            [kg/kg]
      qr_lh,       & ! Rain droplet number concentration   [kg/kg]
      qnr_lh,      & ! Rain number concentration           [#/kg]
      qndrop_lh,   & ! Cloud droplet number concentration  [#/kg]
      qs_lh,       & ! Snow mixing ratio                   [kg/kg]
      qns_lh,      & ! Snow number concentration           [#/kg]
      qi_lh,       & ! Cloud ice mixing ratio              [kg/kg]
      qni_lh,      & ! Cloud ice number concentration      [#/kg]
      qg_lh,       & ! Cloud graupel mixing ratio          [kg/kg]
      qng_lh         ! Cloud graupel number concentration  [#/kg]

! Temperature and water vapor tendencies from the prior timestep
! (used for hole filling only)
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: &
      th_lh_mc, & ! microphysical tendency of temperature [K]
      qv_lh_mc    ! microphysical tendency of water vapor [kg/kg]

REAL, DIMENSION(ims:ime,kms:kme,jms:jme,lh_microphys_calls), INTENT(OUT)::  &
      weights_lh     ! Cloud-weighted sampling weights     [-]
#endif /*SILHS*/

! Standard WRF variables to be brought into this subroutine.
REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN)::  &
      psfc, & ! Surface pressure
      hfx,  & ! Surface heat flux (wpthlp_sfc)
      qfx,  & ! Surface moisture flux (wprtp_sfc)
      ust     ! U-star

! Standard WRF variables to be brought into this subroutine.
REAL, INTENT(IN)::  &
      DX, & ! West-east horizontal grid spacing
      DY    ! South-north horizontal grid spacing

! Standard WRF variables to be brought into this subroutine.
INTEGER, INTENT(IN):: num_force_layers, scm_force
REAL, DIMENSION(num_force_layers), INTENT(IN):: z_force
REAL, DIMENSION(num_force_layers), INTENT(IN):: w_subs

#ifdef CLUBBSTATS
! Added for CLUBB statistics
! Standard WRF variables to be brought into this subroutine.
INTEGER, INTENT(IN)::  &
      grid_id,      & ! WRF domain number
      ITIMESTEP,    & ! Current time step of model run
      start_hour,   & ! Hour of the model starting time
      start_minute, & ! Minute of the model starting time
      start_second    ! Second of the model starting time
! CLUBB statistics changes end
#else
LOGICAL :: l_stats_samp = .false.
#endif /*CLUBBSTATS*/

! Standard WRF tendency variables to be taken out of this subroutine.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(OUT)::  &
      RUBLTEN,  &  ! du/dt due to BL scheme.  [m/s^2]
      RVBLTEN,  &  ! dv/dt due to BL scheme.  [m/s^2]
      RTHBLTEN, &  ! dth/dt due to BL scheme. [K/s]
      RQVBLTEN, &  ! dqv/dt due to BL scheme. [kg/kg/s]
      RQCBLTEN     ! dqc/dt due to BL scheme. [kg/kg/s]

! CLUBB variables stored for entire 3-D WRF Field.
! Momentum-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT):: &
      wpthlp_sgs,     & ! subgrid-scale <w'thl'> (momentum levels)   [m/s K]
      wprtp_sgs,      & ! subgrid-scale <w'rt'> (momentum levels)    [m/s kg/kg]
      wp2_sgs,        & ! subgrid-scale <w'^2> (momentum levels)     [m^2/s^2]
      rtp2_sgs,       & ! subgrid-scale <rt'^2> (momentum levels)    [kg^2/kg^2]
      thlp2_sgs,      & ! subgrid-scale <thl'^2> (momentum levels)   [K^2]
      rtpthlp_sgs,    & ! subgrid-scale <rt'thl'> (momentum levels)  [kg/kg K]
      up2_sgs,        & ! subgrid-scale <u'^2> (momentum levels)     [m^2/s^2]
      vp2_sgs,        & ! subgrid-scale <v'^2> (momentum levels)     [m^2/s^2]
      upwp_sgs,       & ! subgrid-scale <u'w'> (momentum levels)     [m^2/s^2]
      vpwp_sgs,       & ! subgrid-scale <v'w'> (momentum levels)     [m^2/s^2]
      wpthvp_sgs,     & ! subgrid-scale <w'thv'> (momentum levels)   [m/s K]
      rtpthvp_sgs,    & ! subgrid-scale <rt'thv'> (momentum levels)  [kg/kg K]
      thlpthvp_sgs,   & ! subgrid-scale <thl'thv'> (momentum levels) [K^2]
      wprcp_sgs,      & ! subgrid-scale <w'rc'> (momentum levels)    [m/s kg/kg]
      wpthp_sgs         ! subgrid-scale <w'th'> (momentum levels)    [m/s K]

! CLUBB variables stored for entire 3-D WRF Field.
! Thermodynamic-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT)::  &
      wp3_sgs,       & ! subgrid-scale <w'^3> (thermodynamic levels) [m^3/s^3]
      wp2thvp_sgs,   & ! subgrid-scale <w'^2 thv'> (thermo. levels)  [m/s K]
      rcm_CLUBB,     & ! CLUBB's mean cloud water mixing ratio       [kg/kg]
      cf_CLUBB,      & ! CLUBB's grid-box cloud fraction             [-]
      rcm_lyr_CLUBB, & ! CLUBB rcm within cloud layer                [kg/kg]
      cldcov_CLUBB     ! CLUBB cloud cover                           [-]

! CLUBB PDF parameters stored for entire 3-D WRF Field.
! Thermodynamic-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_pdf_params_CLUBB), &
INTENT(INOUT)::  &
      pdf_params_CLUBB    ! CLUBB's PDF parameters                  [units vary]

! CLUBB PDF parameters stored for entire 3-D WRF Field.
! Momentum-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_pdf_params_zm_CLUBB), &
INTENT(INOUT)::  &
      pdf_params_zm_CLUBB    ! CLUBB's PDF parameters (mom. levels) [units vary]

! Output Variables (needed for CLUBB's hole filling)
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(OUT)::  &
      rho_ds_zm,       & ! Dry, static density on momentum levels    [kg/m^3]
      rho_ds_zt,       & ! Dry, static density on thermo. levels     [kg/m^3]
      exner              ! Exner function                            [-]

! Local variables.

! CLUBB variables on 1-D CLUBB Field.
! Momentum-level variables.
REAL, DIMENSION(kms:kme)::  &
      wpthlp,          & ! subgrid-scale <w'thl'> (momentum levels)   [m/s K]
      wprtp,           & ! subgrid-scale <w'rt'> (momentum levels)    [m/s kg/kg]
      wp2,             & ! subgrid-scale <w'^2> (momentum levels)     [m^2/s^2]
      rtp2,            & ! subgrid-scale <rt'^2> (momentum levels)    [kg^2/kg^2]
      thlp2,           & ! subgrid-scale <thl'^2> (momentum levels)   [K^2]
      rtpthlp,         & ! subgrid-scale <rt'thl'> (momentum levels)  [kg/kg K]
      up2,             & ! subgrid-scale <u'^2> (momentum levels)     [m^2/s^2]
      vp2,             & ! subgrid-scale <v'^2> (momentum levels)     [m^2/s^2]
      upwp,            & ! subgrid-scale <u'w'> (momentum levels)     [m^2/s^2]
      vpwp,            & ! subgrid-scale <v'w'> (momentum levels)     [m^2/s^2]
      wpthvp,          & ! subgrid-scale <w'thv'> (momentum levels)   [m/s K]
      rtpthvp,         & ! subgrid-scale <rt'thv'> (momentum levels)  [kg/kg K]
      thlpthvp,        & ! subgrid-scale <thl'thv'> (momentum levels) [K^2]
      wprcp,           & ! subgrid-scale <w'rc'> (momentum levels)    [m/s kg/kg]
      wm_zm,           & ! Vertical velocity on momentum levels.      [m/s]
      w_lss_zm,        & ! Large-scale subsidence on momentum levels  [m/s]
      momentum_heights   ! CLUBB momentum level altitudes.            [m]

! CLUBB variables on 1-D CLUBB Field.
! Thermodynamic-level variables.
REAL, DIMENSION(kms:kme):: &
      wp3,                 & ! subgrid-scale <w'^3> (thermodynamic levels)       [m^3/s^3]
      wp2thvp,             & ! subgrid-scale <w'^2 thv'> (thermodynamic levels)  [m^2/s^2 K]
      rcm,                 & ! CLUBB's mean cloud water mixing ratio             [kg/kg]
      cloud_frac,          & ! CLUBB's grid-box cloud fraction                   [-]
      ice_supersat_frac,   & ! CLUBB's ice cloud fraction (thermodynamic levels) [-]
      radf,                & ! Buoyancy production at CL top                     [m^2/s^3] 
      rcm_in_layer,        & ! CLUBB rcm within the cloud layer                  [kg/kg]
      cloud_cover,         & ! CLUBB cloud cover                                 [-]
      wm_zt,               & ! Vertical velocity on thermodynamic levels         [m/s]
      w_lss_zt,            & ! Large-scale subsidence on thermodynamic levels    [m/s]
      WRF_w_at_tlev,       & ! WRF's resolved w interpolated to thermo. levels   [m/s]
      thermodynamic_heights  ! CLUBB thermodynamic level altitudes               [m]

! thlp3 and rtp3 are used in CLUBB only if l_use_3D_closure is
! set to true in CLUBB's model_flags.F90. This flag and thlp3 and rtp3 are
! experimental as of 15-Jul 2015 and can only be used when thlp3 and rtp3
! are input to CLUBB using l_input_fields
REAL, DIMENSION(kms:kme):: &
      thlp3_dummy,         & ! SGS <th_l'^3>, CLUBB SCM only                     [K^3]
      rtp3_dummy             ! SGS <rt'^3>, CLUBB SCM only                       [(kg/kg)^3]


! CLUBB variables on 1-D CLUBB Field.
! Momentum-level variables.
REAL, DIMENSION(kms:kme)::  &
      invrs_rho_ds_zm, & ! Inv. dry, static density @ momentum levs. [m^3/kg]
      thv_ds_zm          ! Dry, base-state theta_v on momentum levs. [K]

! Thermodynamic-level variables.
REAL, DIMENSION(kms:kme)::  &
      thlm,            & ! Liquid water potential temperature        [K]
      rtm,             & ! Total water mixing ratio                  [kg/kg]
      p_in_Pa,         & ! Pressure (in Pascals)                     [Pa]
      rho,             & ! Air density (thermodynamic levels)        [kg/m^3]
      rho_zm,          & ! Air density on momentum levels            [kg/m^3]
      invrs_rho_ds_zt, & ! Inv. dry, static density @ thermo. levs.  [m^3/kg]
      thv_ds_zt,       & ! Dry, base-state theta_v on thermo. levs.  [K]
      um,              & ! U wind component                          [m/s]
      vm,              & ! V wind component                          [m/s]
      rfrzm              ! Total frozen water mixing ratio           [kg/kg]

! Variables (on WRF momentum and thermodynamic levels) to account for the change
! in altitude of the WRF grid over the previous timestep.
REAL, DIMENSION(kms:kme)::  &
      grid_vel_WRF_mlev, & ! Velocity of WRF's momentum levels       [m/s]
      grid_vel_WRF_tlev    ! Velocity of WRF's thermodynamic levels  [m/s]

! The pre-updated value of CLUBB's rcm is saved for use later in this subroutine.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme)::  &
      rcm_CLUBB_old,     &  ! CLUBB's rcm before being updated during this timestep.
      rcm_lyr_CLUBB_old     ! CLUBB's rcm_lyr_CLUBB before being updated during this timestep.

#ifdef SILHS
REAL, DIMENSION(kms:kme,lh_microphys_calls):: &
      lh_thl    ! Subcolumns of thetal   [K]

TYPE(lh_clipped_variables_type), DIMENSION(kms:kme,lh_microphys_calls):: &
      lh_clipped_vars

REAL, DIMENSION(kms:kme,lh_microphys_calls):: &
      lh_sample_point_weights ! Weights for cloud-weighted sampling     [-]

REAL(kind=core_rknd), DIMENSION(kms:kme,lh_microphys_calls,pdf_dim):: &
      X_nl_all_levs,     & ! Lognormally distributed SILHS samples  [units vary]
      X_nl_all_levs_raw    ! Raw (unclipped) SILHS sample points    [units vary]

REAL(kind=core_rknd), DIMENSION(pdf_dim, kms:kme):: &
      mu_x_1,    & ! Mean array for SILHS
      mu_x_2,    & ! Mean array for SILHS
      sigma_x_1, & ! Stdev array for SILHS
      sigma_x_2    ! Stdev array for SILHS

REAL(kind=core_rknd), DIMENSION(pdf_dim, pdf_dim, kms:kme):: &
      corr_cholesky_mtx_1, & ! Cholesky decomp of corr_array_1 needed for SILHS
      corr_cholesky_mtx_2, & ! Cholesky decomp of corr_array_2 needed for SILHS
      corr_array_1, & ! Correlation array needed for SILHS
      corr_array_2    ! Correlation array needed for SILHS

type(hydromet_pdf_parameter), dimension(kms:kme) :: &
      hydromet_pdf_params    ! Hydrometeor PDF parameters        [units vary]

real( kind = core_rknd ), dimension(kms:kme) :: &
      Nc_in_cloud    ! Mean cloud droplet concentration (within-cloud)  [num/kg]

INTEGER, DIMENSION(kms:kme,lh_microphys_calls):: &
      X_mixt_comp_all_levs

INTEGER:: icall 

LOGICAL :: l_use_Ncn_to_Nc = .true. ! Whether to use Ncn_to_Nc or not; Ncn_to_Nc caused problems
                                    ! in CAM, when using MG 1.0 microphysics, because the changes
                                    ! of this subroutine where not included in the Nc-tendency.
#endif /*SILHS*/

real( kind = core_rknd ), allocatable, dimension(:,:) :: & 
      hydrometp2    ! Variance of a hydrometeor (overall) (m-levs.)   [units^2]

REAL, DIMENSION(kms:kme,hydromet_dim) :: &
      hydromet,    & ! Mean value of hydrometeor fields            [units vary]
      wphydrometp    ! Covariances between w and the hydrometeors  [units vary]

! Variable for determining total density at the surface.
REAL ::  &
      rv_sfc  ! Water vapor mixing ratio at the surface (lin. ext.)  [kg/kg]

! Interface with WRF Surface Fluxes.
REAL ::  &
      sfc_wind, & ! Surface wind speed                                [m/s]
      cpm,      & ! Specific heat of moist air                        [J/(kg K)]
      cd0         ! WRF constant drag coefficient used if isfflx = 0. []

! Relevant CLUBB variables; surface fluxes.
REAL::  &
      wpthlp_sfc, &  ! Surface heat flux, w'th_l'.    [K m/s]
      wprtp_sfc,  &  ! Surface moisture flux, w'r_t'. [kg/kg m/s]
      upwp_sfc,   &  ! Surface momentum flux, u'w'.   [m^2/s^2]
      vpwp_sfc       ! Surface momentum flux, v'w'.   [m^2/s^2]

! Relevant CLUBB variables.
REAL, DIMENSION(kms:kme)::  &
      thlm_forcing,    &
      rtm_forcing,     &
      um_forcing,      &
      vm_forcing,      &
      wprtp_forcing,   &
      wpthlp_forcing,  &
      rtp2_forcing,    &
      thlp2_forcing,   &
      rtpthlp_forcing

! Other variables needed for CLUBB import.
REAL:: fcor

! Scalar Variables

! Scalars can conceivably be integrated with WRF scalar variables
! (chem options).

!=====>
! Note:  To integrate the scalar code with the WRF model, the following scalar
!        variables need to be saved in a 3-D array from timestep to timestep.

! CLUBB variables on 1-D CLUBB Field.
! Thermodynamic-level variables; scalars.
REAL, DIMENSION(kms:kme,sclr_dim)::  sclrm  ! Mean value of scalar. 

! CLUBB variables on 1-D CLUBB Field.
! Momentum-level variables; scalars.
REAL, DIMENSION(kms:kme,sclr_dim)::  &
      wpsclrp,   & ! w'sclr'.
      sclrp2,    & ! sclr'^2.
      sclrprtp,  & ! sclr'r_t'.
      sclrpthlp, & ! sclr'th_l'.
      sclrpthvp    ! sclr'th_v'.
!=====>

! Relevant CLUBB variables; scalars.
REAL, DIMENSION(kms:kme,sclr_dim)::  sclrm_forcing  ! scalar forcings.

! Relevant CLUBB variables; scalar surface fluxes.
REAL, DIMENSION(sclr_dim)::  wpsclrp_sfc  ! w'sclr' surface flux.

! Eddy-scalar Variables

! Eddy-scalars can conceivably be integrated with WRF scalar variables
! (chem options).

!=====>
! Note:  To integrate the eddy-scalar code with the WRF model, the following
!        eddy-scalar variables need to be saved in a 3-D array from timestep
!        to timestep.

! CLUBB variables on 1-D CLUBB Field.
! Thermodynamic-level variables; scalars.
REAL, DIMENSION(kms:kme,edsclr_dim)::  edsclrm  ! Mean value of eddy-scalar. 
!=====>

! Relevant CLUBB variables; scalars.
REAL, DIMENSION(kms:kme,edsclr_dim)::  edsclrm_forcing  ! eddy-scalar forcings.

! Relevant CLUBB variables; scalar surface fluxes.
REAL, DIMENSION(edsclr_dim)::  wpedsclrp_sfc  ! w'edsclr' surface flux.

! Store values of the PDF parameters that are passed into and out of CLUBB core.
TYPE(pdf_parameter), DIMENSION(kms:kme):: &
      pdf_params,    & ! PDF parameters (thermodynamic levels)    [units vary]
      pdf_params_zm    ! PDF parameters on momentum levels        [units vary]

! Error code that is sent out of CLUBB core.
INTEGER:: err_code

REAL(KIND=core_rknd)::  & 
      dt_clubb, & ! Timestep size          [s]
      dtclosure   ! Closure timestep size  [s]

#ifdef CLUBBSTATS
! Added for CLUBB statistics
REAL(KIND=time_precision)::  & 
      time_initial, & ! Model start time (since midnight on start date)    [s]
      time_current    ! Model current time (since midnight on start date)  [s]
REAL(KIND=core_rknd) :: &
      dtmain          ! Main timestep size                                 [s]
INTEGER :: &
      stats_nsamp, &  ! Stats sampling interval [timestep]
      stats_nout      ! Stats output interval   [timestep]
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

REAL, PARAMETER:: eps = 1.0E-10

INTEGER:: niterlong, iter

INTEGER:: k_start, k_end, i, j, k, sclr_type

#ifdef CLUBBSTATS
! Added for CLUBB statistics
LOGICAL, PARAMETER::  &
      l_stdout = .true.  ! Flag to print output per timestep
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

LOGICAL, PARAMETER :: l_fill_holes_hm = .true.

LOGICAL :: l_stats_samp_tmp

REAL(KIND=core_rknd), dimension(kms:kme,hydromet_dim) :: &
  wp2hmp, &
  rtphmp, &
  thlphmp

!-------------------------------------------------------------------------------

k_start = kts
k_end   = MIN(kde-1, kte)

! Note on the WRF grid indices
!
! Throughout much of the WRF code, the grid index kte stands for the number of
! WRF momentum levels.  This can be found in subroutine phy_prep in
! module_big_step_utilities.F.  This can also be found in subroutine phy_init
! in module_physics_init.F.  The number of CLUBB momentum levels is supposed to
! be the same as the number of WRF momentum levels.  Thus, the number of CLUBB
! momentum levels was set to kte in subroutine CLUBB_init, which in turn was
! called by subroutine bl_init, which in turn was called by subroutine phy_init.
! Thus, the CLUBB grid is set up correctly.  However, in WRF's call to its
! various drivers, such as pbl_driver, the value of kte that is sent in is
! reduced by 1.  Thus, the value of kte sent into this subroutine is 1 less than
! the number of WRF (and CLUBB) momentum levels.  Thus, in any loop that is
! related to the number of momentum levels, the upper index must be kte+1.
! Other PBL schemes also make use of grid index kte+1.
!
! The number of WRF thermodynamic levels is one less than the number of WRF
! momentum levels.  The index of the uppermost WRF thermodynamic level is
! usually kte-1.  However, in this subroutine, the index of the uppermost WRF
! thermodynamic level is kte.  Since CLUBB adds an extra thermodynamic level
! below the model surface, the number of CLUBB thermodynamic levels is equal to
! the number of CLUBB (and WRF) momentum levels, and is one greater than the
! number of WRF thermodynamic levels.  Thus, the uppermost CLUBB thermodynamic
! level corresponds with the uppermost WRF thermodynamic level, but its grid
! index is one greater.  Thus, the index of the uppermost CLUBB thermodynamic
! level is usually kte.  However, in this subroutine, the index of the uppermost
! CLUBB thermodynamic level is kte+1.  Also, in this subroutine, k_end = kte.
! Thus, the index of the uppermost WRF thermodynamic level is k_end, and the
! index of the uppermost CLUBB thermodynamic level is k_end+1.

!###############################################################################

! Initialize error code
err_code = clubb_no_error

! Initialize radf to zero
radf = 0.0

! Allocate the overall variance of a hydrometeor, <hm'^2>. 
allocate( hydrometp2(gr%nz,hydromet_dim) ) 



! CALL THE CLUBB PARAMETERIZATION FOR EVERY COLUMN IN THE 3-D WRF FIELD.
DO i = its, ite, 1
   DO j = jts, jte, 1
      !#########################################################################
      !#########################################################################
      !#########################################################################

      ! ####### CLUBB Grid Heights For Column (i,j) #######

      ! Setup momentum-level grid heights.  This is a one-dimensional array used
      ! for CLUBB based on the altitudes in a given column from the
      ! three-dimensional array used by WRF.
      DO k = k_start, kte+1, 1
         momentum_heights(k) = z_at_w(i,k,j)
      ENDDO

      ! Setup thermodynamic-level grid heights.  This is also a one-dimensional
      ! array used for CLUBB based on the altitudes in a given column from the
      ! three-dimensional array used by WRF.  WRF's thermodynamic levels are
      ! always located exactly halfway between the momentum levels that encase
      ! them.  The indices of WRF's thermodynamics levels and CLUBB's
      ! thermodynamic levels differ by one due to the fact that CLUBB's first
      ! thermodynamic level is below the model surface (see above diagram).
      DO k = k_start+1, kte+1, 1
         thermodynamic_heights(k) = z(i,k-1,j)
      ENDDO
      ! The first thermodynamic level (which is below the model surface) is set
      ! to be the same distance below the first momentum level as the second
      ! thermodynamic level is above the first momentum level.
      thermodynamic_heights(k_start)  &
         = ( momentum_heights(k_start) - thermodynamic_heights(k_start+1) ) &
           + momentum_heights(k_start)

      ! Set up CLUBB grid heights for column (i,j).
      ! Calling subroutine setup_grid_heights needs to be called once for every
      ! column.  Doing so will enter the correct grid heights without
      ! reallocating any variables.  All grid variables are allocated ONCE with
      ! the call from subroutine CLUBB_bl_init (below) to CLUBB's subroutine
      ! setup_clubb_core (which in turn calls subroutine setup_grid).
      call setup_grid_heights_api &
                 ( l_implemented, grid_type,  & 
                   deltaz, zm_init, momentum_heights,  & 
                   thermodynamic_heights )

      ! CLUBB grid height variables (type gr) and grid operators (such as zm2zt
      ! or zt2zm) can now be used for column (i,j).

      ! Since the heights have changed from the call to setup_grid_heights,
      ! also adjust the values of background diffusivity (nu's), which depend
      ! on the average vertical grid spacing.
      ! Here, kte+1 is the number of vertical levels (see note at beginning
      ! of this subroutine).
      ! In this code, grid_type and deltaz are not used but are part of the 
      ! call arguments. 
      ! Note: the nu's are not adjusted during the initialization loop in
      ! CLUBB_bl_init because they do not affect anything until the call
      ! to advance_clubb_core below.  --ldgrant Jul 2010
      call adj_low_res_nu_api &
             ( kte+1, grid_type, deltaz, &  ! Intent(in)
               momentum_heights, thermodynamic_heights ) ! Intent(in)


      ! ####### Variables For CLUBB Input #######

      ! Set up the 1-D CLUBB variables for every grid column.
      ! Momentum-level variables.
      DO k = k_start, kte+1, 1

         wpthlp(k)      = wpthlp_sgs(i,k,j)
         wprtp(k)       = wprtp_sgs(i,k,j)
         wp2(k)         = wp2_sgs(i,k,j)
         rtp2(k)        = rtp2_sgs(i,k,j)
         thlp2(k)       = thlp2_sgs(i,k,j)
         rtpthlp(k)     = rtpthlp_sgs(i,k,j)
         up2(k)         = up2_sgs(i,k,j)
         vp2(k)         = vp2_sgs(i,k,j)
         upwp(k)        = upwp_sgs(i,k,j)
         vpwp(k)        = vpwp_sgs(i,k,j)
         wpthvp(k)      = wpthvp_sgs(i,k,j)
         rtpthvp(k)     = rtpthvp_sgs(i,k,j)
         thlpthvp(k)    = thlpthvp_sgs(i,k,j)
         wprcp(k)       = wprcp_sgs(i,k,j)

         IF ( l_use_boussinesq ) THEN
            invrs_rho_ds_zm(k) = 1.0
            rho_ds_zm(i,k,j)   = 1.0
            thv_ds_zm(k)       = t0
         ELSE
            IF ( k == k_start ) THEN
               ! WRF momentum level 1 is below WRF thermodynamic level 1.  Thus,
               ! interpolation cannot take place.  Set the values at momentum
               ! level 1 to the value at thermodynamic level 1.
               invrs_rho_ds_zm(k_start) = alb(i,k_start,j)
               thv_ds_zm(k_start)       = t_init(i,k_start,j) + t0
            ELSEIF ( k > k_start .AND. k < kte+1 ) THEN
               ! Interpolate the values from WRF thermodynamic levels to WRF
               ! momentum levels.
               invrs_rho_ds_zm(k) &
                  = interp_0( alb(i,:,j), z(i,:,j), z_at_w(i,k,j), kte )
               thv_ds_zm(k)       &
                  = interp_0( t_init(i,:,j), z(i,:,j), z_at_w(i,k,j), kte ) + t0
            ELSEIF ( k == kte+1 ) THEN
               ! The uppermost WRF momentum level is above the uppermost WRF
               ! thermodynamic level.  Thus, interpolation cannot take place.
               ! Set the values at momentum level kte+1 to the value at
               ! thermodynamic level kte.
               invrs_rho_ds_zm(kte+1) = alb(i,kte,j)
               thv_ds_zm(kte+1)       = t_init(i,kte,j) + t0
            ENDIF
            rho_ds_zm(i,k,j) = 1.0/invrs_rho_ds_zm(k)
         ENDIF

         ! Passive scalars.
         ! For now, all scalars will be set to 0 for CLUBB testing purposes.
         DO sclr_type = 1, sclr_dim, 1
            wpsclrp(k,sclr_type)   = 0.0
            sclrp2(k,sclr_type)    = 0.0
            sclrprtp(k,sclr_type)  = 0.0
            sclrpthlp(k,sclr_type) = 0.0
            sclrpthvp(k,sclr_type) = 0.0
         ENDDO

      ENDDO

      ! Thermodynamic-level variables.
      DO k = k_start+1, kte+1, 1

         ! Save rcm_CLUBB as rcm_CLUBB_old
         rcm_CLUBB_old(i,k-1,j) = rcm_CLUBB(i,k-1,j)
         rcm_lyr_CLUBB_old(i,k-1,j) = rcm_lyr_CLUBB(i,k-1,j)

         wp3(k)          = wp3_sgs(i,k-1,j)
         wp2thvp(k)      = wp2thvp_sgs(i,k-1,j)
         rcm(k)          = rcm_CLUBB(i,k-1,j)
         cloud_frac(k)   = cf_CLUBB(i,k-1,j)
         rcm_in_layer(k) = rcm_lyr_CLUBB(i,k-1,j)
         cloud_cover(k)  = cldcov_CLUBB(i,k-1,j)

         ! The following thermodynamic-level variables are passed from WRF
         ! to CLUBB as INPUT ONLY!  They are not directly altered by CLUBB.
         um(k)      = u_phy(i,k-1,j)
         vm(k)      = v_phy(i,k-1,j)
         p_in_Pa(k) = p_phy(i,k-1,j)
         exner(i,k,j) = pi_phy(i,k-1,j)
         rho(k)     = rho_in(i,k-1,j)

         IF ( l_use_boussinesq ) THEN
            rho_ds_zt(i,k,j)   = 1.0
            invrs_rho_ds_zt(k) = 1.0
            thv_ds_zt(k)       = t0
         ELSE
            rho_ds_zt(i,k,j)   = 1.0/alb(i,k-1,j)
            invrs_rho_ds_zt(k) = alb(i,k-1,j)
            thv_ds_zt(k)       = t_init(i,k-1,j) + t0
         ENDIF
         

         IF ( .NOT. flag_qc ) THEN

            ! The selected WRF microphysics scheme does not contain cloud water
            ! mixing ratio.  Currently, the only scheme that does this is WRF's
            ! mp_physics = 0 (PASSIVEQV).
            !
            ! CLUBB's liquid water potential temperature, theta_l, must be
            ! calculated from WRF's potential temperature, theta, and any liquid
            ! water.  Since, with this microphysics setting, WRF does not
            ! contain information on cloud water mixing ratio, any information
            ! must be provided by using the value of CLUBB's internally-computed
            ! cloud water mixing ratio, rcm_CLUBB.  CLUBB's total water mixing
            ! ratio, r_t, is the same as WRF's q_v because WRF does not contain
            ! information on q_c with this microphysics setting.  Thus, WRF's
            ! q_v is actually q_t, total water mixing ratio.

            IF ( l_use_cldcov_rcm_lyr ) THEN
              thlm(k) = th_phy(i,k-1,j)  &
                        - ( XLV / ( cp * pi_phy(i,k-1,j) )  ) * rcm_CLUBB(i,k-1,j)
            ELSE
              thlm(k) = th_phy(i,k-1,j)  &
                        - ( XLV / ( cp * pi_phy(i,k-1,j) )  ) * rcm_lyr_CLUBB(i,k-1,j)
            ENDIF 


            rtm(k)  = qv_curr(i,k-1,j)

         ELSE  ! flag_qc == .true.

            ! The selected WRF microphysics scheme contains cloud water mixing
            ! ratio.
            !
            ! All other WRF microphysics schemes include cloud water mixing
            ! ratio, q_c.  CLUBB's liquid water potential temperature, theta_l,
            ! must be calculated from WRF's potential temperature, theta, and
            ! q_c.  CLUBB's total water mixing ratio, r_t, must be calculated
            ! from WRF's water vapor mixing ratio, q_v, and q_c.

            thlm(k) = th_phy(i,k-1,j)  &
                      - ( XLV / ( cp * pi_phy(i,k-1,j) )  ) * qc_curr(i,k-1,j)

            rtm(k)  = qv_curr(i,k-1,j) + qc_curr(i,k-1,j)

         ENDIF

         ! Passive scalars.
         ! For now, all scalars will be set to 0 for CLUBB testing purposes.
         DO sclr_type = 1, sclr_dim, 1
            sclrm(k,sclr_type)   = 0.0
            edsclrm(k,sclr_type) = 0.0
         ENDDO

      ENDDO  ! k = k_start+1 ... kte+1

      ! Setup PDF parameters for the grid column.
      ! The number of PDF parameters found in pdf_params_CLUBB and
      ! pdf_params_zm_CLUBB is the value of the variable num_pdf_params, which
      ! is found in clubb/pdf_parameter_module.F90.  The variables
      ! num_pdf_params_CLUBB and num_pdf_params_zm_CLUBB both have a value of
      ! num_pdf_params + 1.  This happens because WRF adds an extra element (at
      ! index value 1) to the number of species in its 4-D arrays declared in
      ! the Registry.  The relevant indices in pdf_params_CLUBB and
      ! pdf_params_zm_CLUBB range from 2 to num_pdf_params_CLUBB and 2 to
      ! num_pdf_params_zm_CLUBB, respectively.
      ! Thermodynamic grid levels.
      call unpack_pdf_params_api( &
           pdf_params_CLUBB(i,k_start:kte,j,2:num_pdf_params_CLUBB), & ! In
           kte-k_start+1, &                                            ! In
           pdf_params(k_start+1:kte+1) )                               ! Out
      ! Momentum grid levels.
      call unpack_pdf_params_api( &
           pdf_params_zm_CLUBB(i,k_start:kte+1,j,2:num_pdf_params_zm_CLUBB), & ! In
           kte-k_start+2, &                                                    ! In
           pdf_params_zm(k_start:kte+1) )                                      ! Out

      ! Lowest-level Thermodynamic CLUBB setup (below surface).
      wp3(k_start)          = 0.0
      rcm(k_start)          = 0.0
      cloud_frac(k_start)   = 0.0
      rcm_in_layer(k_start) = 0.0
      cloud_cover(k_start)  = 0.0

      ! CLUBB sets um and vm at the thermodynamic level k = 1 (below surface) to
      ! their respective values at thermodynamic level k = 2.
      um(k_start) = um(k_start+1)
      vm(k_start) = vm(k_start+1)

      ! To stay consistent with CLUBB code, pressure, exner, and density at the
      ! lowest CLUBB thermodynamic level (which is below the surface) are
      ! recorded to be the respective pressure, exner, and density at the
      ! surface itself (which is at the lowest momentum level).
      !
      ! Set surface pressure to the value that WRF uses in the PBL driver.
      p_in_Pa(k_start) = psfc(i,j)
      ! Calculate exner at the surface based on surface pressure.
      exner(i,k_start,j) = (psfc(i,j)/p1000mb)**rcp
      ! Computation of density:
      ! Total density (rho) can be calculated from total air pressure (p), water
      ! vapor mixing ratio (r_v), and temperature (T), such that:
      ! rho = p / { [ ((R_d/R_v)+r_v) / ((R_d/R_v)*(1+r_v)) ] * R_d * T };
      ! where R_d is the gas constant for dry air and R_v is the gas constant
      ! for water vapor.  The value of water vapor mixing ratio at the surface
      ! (momentum level k = 1) is found through a linear extension of its
      ! values at thermodynamic levels 1 and 2.
      rv_sfc = MAX( LIN_EXT( qv_curr(i,2,j), qv_curr(i,1,j),  &
                             z(i,2,j), z(i,1,j), z_at_w(i,1,j) ), 0.0 )
      rho(k_start)  &
         = psfc(i,j)  &
           / ( ((EP_2+rv_sfc)/(EP_2*(1.0+rv_sfc))) * r_d * t8w(i,k_start,j) )

      IF ( l_use_boussinesq ) THEN
         rho_ds_zt(i,k_start,j)   = 1.0
         invrs_rho_ds_zt(k_start) = 1.0
         thv_ds_zt(k_start)       = t0
      ELSE
         ! Simply set the values at CLUBB's thermodynamic level 1, which is
         ! below the surface, to the values at CLUBB's thermodynamic level 2,
         ! which is the first thermodynamic level above the surface, and also
         ! corresponds with WRF's thermodynamic level 1.
         rho_ds_zt(i,k_start,j)   = 1.0/alb(i,1,j)
         invrs_rho_ds_zt(k_start) = alb(i,1,j)
         thv_ds_zt(k_start)       = t_init(i,1,j) + t0
      ENDIF

      ! CLUBB sets thlm and rtm at the thermodynamic level k = 1 (below surface)
      ! to their respective values at thermodynamic level k = 2.
      thlm(k_start) = thlm(k_start+1)
      rtm(k_start)  = rtm(k_start+1)

      ! Passive scalars.
      ! For now, all scalars will be set to 0 for CLUBB testing purposes.
      DO sclr_type = 1, sclr_dim, 1
         sclrm(k_start,sclr_type)   = sclrm(k_start+1,sclr_type)
         edsclrm(k_start,sclr_type) = edsclrm(k_start+1,sclr_type)
      ENDDO

      ! Interpolate rho to momentum levels.
      ! rho is a thermodynamic-level variable and the CLUBB variable rho stores
      ! thermodynamic grid levels.  However, the values of rho needs to be known
      ! at momentum levels.  The CLUBB variable rho_zm stores these values.
      rho_zm(k_start) = rho(k_start)
      DO k = k_start+1, kte+1, 1
         rho_zm(k) = zt2zm_api( rho, k )
      ENDDO


      ! ####### Surface Fluxes #######

      ! Surface heat, moisture, and momentum fluxes are all computed at the
      ! lowest momentum level.

      ! Compute the wind speed near the surface.
      ! This is used in the calculations of upwp and vpwp, regardless of the
      ! value of config_flags%isfflx.
      sfc_wind = MAX( SQRT( u_phy(i,kts,j)**2.0 + v_phy(i,kts,j)**2.0 ), &
                      eps )

      ! Calculate cpm by using the value of water vapor mixing ratio at the
      ! model surface.
      ! Reference for this formula:
      !   "Atmospheric Convection" by Kerry Emanuel (1994)
      ! Eq. 4.2.5:   cpm ~= cpd ( 1 + 0.85*r )     
      !                          where ( cpv/cpd - 1 ) ~= 0.85
      ! WRF routines use values other than 0.85 to compute cpm. Cumulus and
      ! microphysics schemes use the value 0.887, while module_diffusion_em,
      ! BL schemes, and surface schemes use 0.8.  The value of 0.8 is used
      ! below for consistency with other BL schemes.
      cpm = cp * ( 1.0 + 0.8 * rv_sfc )

      SELECT CASE ( isfflx )

      ! WRF adds surface fluxes into the appropriate tendency variables in
      ! subroutine vertical_diffusion_2 (in module_diffusion_em).  However,
      ! subroutine vertical_diffusion_2 is not called if a PBL scheme (such as
      ! CLUBB) is being used.  The values of the surface fluxes are brought into
      ! CLUBB.  Eventually, they are used to set WRF tendencies RTHBLTEN,
      ! RQVBLTEN, RUBLTEN, and RVBLTEN, which affect the value of the
      ! appropriate variables in WRF.  Additionally, CLUBB used the values of
      ! the surface fluxes to set the values of certain CLUBB variables at the
      ! surface, such as wp2, rtp2, thlp2, rtpthlp, etc.

      CASE (0)

         ! Surface heat flux, w'th_l'.
         ! A fixed surface heat flux is specified in namelist.input
         ! (as config_flags%tke_heat_flux).  The units of the specified heat
         ! flux are K m/s.
         wpthlp_sfc = tke_heat_flux

         ! Surface moisture flux, w'r_t'.
         ! No surface moisture flux.
         wprtp_sfc = 0.0

         ! Surface momentum fluxes, u'w' and v'w'.
         ! A constant drag coefficient, cd0, is specified in namelist.input
         ! (as config_flags%tke_drag_coefficient).  The constant drag
         ! coefficient has dimensionless units.
         cd0 = tke_drag_coefficient
         upwp_sfc = - cd0 * u_phy(i,kts,j) * sfc_wind
         vpwp_sfc = - cd0 * v_phy(i,kts,j) * sfc_wind

      CASE (1)

         ! Default value of WRF namelist variable isfflx.

         ! Surface heat flux, w'th_l'.
         ! The surface heat flux is computed or set in a surface routine.
         ! The units of the computed or set heat flux are W/m^2, which are
         ! converted here to K m/s.
         wpthlp_sfc = hfx(i,j) / ( cpm * rho_zm(k_start) )

         ! Surface moisture flux, w'r_t'.
         ! The surface moisture flux is computed or set in surface routine.
         ! The units of the computed or set moisture flux are kg / (m^2 s),
         ! which are converted here to (kg/kg) m/s.
         wprtp_sfc  = qfx(i,j) / rho_zm(k_start)

         ! Surface momentum fluxes, u'w' and v'w'.
         ! The surface friction velocity, ustar, is computed or set in a surface
         ! routine.  The units of ustar are m/s.
         upwp_sfc = - ( ust(i,j)**2.0 ) * ( u_phy(i,kts,j) / sfc_wind )
         vpwp_sfc = - ( ust(i,j)**2.0 ) * ( v_phy(i,kts,j) / sfc_wind )

      CASE (2)

         ! Surface heat flux, w'th_l'.
         ! A fixed surface heat flux is specified in namelist.input
         ! (as config_flags%tke_heat_flux).  The units of the specified heat
         ! flux are K m/s.
         wpthlp_sfc = tke_heat_flux

         ! Surface moisture flux, w'r_t'.
         ! The surface moisture flux is computed or set in surface routine.
         ! The units of the computed or set moisture flux are kg / (m^2 s),
         ! which are converted here to (kg/kg) m/s.
         wprtp_sfc  = qfx(i,j) / rho_zm(k_start)

         ! Surface momentum fluxes, u'w' and v'w'.
         ! The surface friction velocity, ustar, is computed or set in a surface
         ! routine.  The units of ustar are m/s.
         upwp_sfc = - ( ust(i,j)**2.0 ) * ( u_phy(i,kts,j) / sfc_wind )
         vpwp_sfc = - ( ust(i,j)**2.0 ) * ( v_phy(i,kts,j) / sfc_wind )

      CASE DEFAULT

         CALL wrf_error_fatal &
             ( 'The value of isfflx is invalid; valid options are 0, 1, or 2.' )

      END SELECT

      ! Surface Fluxes:  passive scalars.
      ! For now, all scalars will be set to 0 for CLUBB testing purposes.
      DO sclr_type = 1, sclr_dim, 1
         wpsclrp_sfc(sclr_type)   = 0.0
         wpedsclrp_sfc(sclr_type) = 0.0
      ENDDO


      ! ####### Vertical Velocity Profile #######

      ! Vertical velocity at thermodynamic levels, wm_zt.

      ! WRF's resolved vertical velocity (w_2) is located on momentum levels.
      ! It will be interpolated to WRF thermodynamic levels.  WRF's
      ! thermodynamic levels are always located exactly halfway between two
      ! momentum levels.  The interpolation is computed using WRF grid indices.
      DO k = k_start, k_end, 1
         WRF_w_at_tlev(k) = (1.0/2.0) * ( w_2(i,k+1,j) + w_2(i,k,j) )
      ENDDO

      ! Large-scale subsidence.
      ! WRF's large-scale subsidence is input from an external netCDF file.  If
      ! there is large-scale subsidence input, z_force will be an array of
      ! various forcing heights.  Otherwise, it will be an array of zeros.
      !
      ! If scm_force equals 0 (scm forcing turned off), z_force and w_subs have length 1 no matter
      ! what the value of num_force_layers is. z_force and w_subs have length num_force_layers only if
      ! scm_force equals 1. But z_force and w_subs are allways allocated with length num_force_layers.
      ! So in case scm_force equals 0 "ANY( z_force(:) > 0.0 )" would check random memory. Therefore we 
      ! have to exclude this case.
      IF ( scm_force .eq. 1 .AND. ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0  ) THEN
         ! WRF's large-scale subsidence is computed at thermodynamic grid
         ! levels.  CLUBB thermodynamic-level indices are offset from WRF
         ! thermodynamic-level indices by one.  See above diagram.
         DO k = k_start+1, kte+1, 1
            w_lss_zt(k) = interp_0(w_subs,z_force,z(i,k-1,j),num_force_layers)
         ENDDO
         ! The lowest CLUBB thermodynamic level is below ground.  The value of w
         ! is set to 0.
         w_lss_zt(k_start) = 0.0
      ELSE
         w_lss_zt(:) = 0.0
      ENDIF

      ! Large-scale subsidence has been brought in on thermodynamic levels.
      ! It will be added to the WRF resolved vertical velocity that has been
      ! interpolated to thermodynamic levels.  Since wm_zt and w_lss are CLUBB
      ! grid variables and WRF_w_at_tlev is a WRF grid variable, the grid
      ! indices will be offset by one (see above diagram).  This is compensated
      ! for below in setting CLUBB's wm_zt.
      DO k = k_start+1, kte+1, 1
         wm_zt(k) = w_lss_zt(k) + WRF_w_at_tlev(k-1)
      ENDDO
      ! The lowest CLUBB thermodynamic level is below ground.  The value of w
      ! is set to 0.
      wm_zt(k_start) = 0.0

      ! Vertical velocity at momentum levels, wm_zm.

      ! Large-scale subsidence for the CLUBB thermodynamic grid is stored as
      ! w_lss_zt.  This is interpolated to the CLUBB momentum grid as w_lss_zm.
      !
      ! WRF's large-scale subsidence is input from an external netCDF file.  If
      ! there is large-scale subsidence input, z_force will be an array of
      ! various forcing heights.  Otherwise, it will be an array of zeros.
      !
      ! If scm_force equals 0 (scm forcing turned off), z_force and w_subs have length 1 no matter
      ! what the value of num_force_layers is. z_force and w_subs have length num_force_layers only if
      ! scm_force equals 1. But z_force and w_subs are allways allocated with length num_force_layers.
      ! So in case scm_force equals 0 "ANY( z_force(:) > 0.0 )" would check random memory. Therefore we 
      ! have to exclude this case.
      IF ( scm_force .eq. 1 .AND. ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
         DO k = k_start+1, kte+1, 1
            w_lss_zm(k) = interp_0(w_subs,z_force,z_at_w(i,k,j),num_force_layers)
         ENDDO
         ! Large-scale subsidence is set to 0 at the surface.
         w_lss_zm(k_start) = 0.0
      ELSE
         w_lss_zm(:) = 0.0
      ENDIF

      ! WRF and CLUBB's momentum level indicies are identical.  Compute CLUBB's
      ! wm_zm.
      DO k = k_start, kte+1, 1
         wm_zm(k) = w_lss_zm(k) + w_2(i,k,j)
      ENDDO

      ! Adjust the values of wm_zt and wm_zt to account for changes in grid
      ! altitude over time.
      !
      ! Account for the changes in the altitudes of the grid levels over time by
      ! subtracting off the velocities of the grid levels from w.
      ! The altitude of WRF's grid levels is determined by geopotential divided
      ! by gravity (g).  Since WRF prognoses geopotential, it changes over time,
      ! and thus so do WRF's grid level altitudes.
      !
      ! At this point in the model timestep, neither ph_2 nor ph_1 have been
      ! updated.  The variable ph_2 contains the current values of perturbation
      ! geopotential.  Unless this is the first model timestep, ph_1 contains
      ! the values of ph_2 from the previous timestep.  Thus, the current
      ! heights of momentum grid levels are given by:
      !    ( phb(i,k,j) + ph_2(i,k,j) ) / g;
      ! where phb is the value of base-state geopotential (which does not change
      ! over time).  The heights of the momentum grid levels at the previous
      ! timestep are given by:
      !    ( phb(i,k,j) + ph_1(i,k,j) ) / g.
      ! The rates of change of the momentum grid level heights over the most
      ! recent timestep are given by:
      !    [ ( ph_2(i,k,j) - ph_1(i,k,j) ) / g ] / dt;
      ! where dt is the duration of the model timestep.
      ! Since WRF's thermodynamic grid levels are located halfway between its
      ! momentum grid levels, the current heights of thermodynamic grid levels
      ! are given by:
      !    0.5*( phb(i,k,j) + ph_2(i,k,j) + phb(i,k+1,j) + ph_2(i,k+1,j) ) / g;
      ! whereas the heights of the thermodynamic grid levels at the previous
      ! timestep are given by:
      !    0.5*( phb(i,k,j) + ph_1(i,k,j) + phb(i,k+1,j) + ph_1(i,k+1,j) ) / g.
      ! The rates of change of the thermodynamic grid level heights over the
      ! most recent timestep are given by:
      !    [ 0.5*( ph_2(i,k,j) + ph_2(i,k+1,j) 
      !            - ph_1(i,k,j) - ph_1(i,k+1,j) ) / g ] / dt.
      ! In the special case that this is the first model timestep, ph_1 is
      ! exactly the same as ph_2.  However, in this scenario, ph_2 and ph_1 are
      ! still at their initialization values, therefore grid altitudes are still
      ! at their initialization values.  Thus, no adjustment is needed.
      !
      ! This adjustment is necessary because WRF's grid levels change in
      ! altitude over time.  Alternatively, if CLUBB's prognostic variables
      ! (wpthlp, wprtp, wp2, wp3, rtp2, thlp2, rtpthlp, up2, and vp2) were to be
      ! treated as scalars in WRF, they would undergo horizontal and vertical
      ! advection in WRF.  Then, this adjustment would not be needed.
      ! Additionally, the code in CLUBB would have to be adjusted so that these
      ! variables are not vertically advected if CLUBB is part of a host model.

      ! Grid velocity on WRF momentum levels.
      DO k = k_start, kte+1, 1
         grid_vel_WRF_mlev(k) = ( ( ph_2(i,k,j) - ph_1(i,k,j) ) / g ) / DT
      ENDDO
      ! Grid velocity on WRF thermodynamic levels.
      DO k = k_start, k_end, 1
         grid_vel_WRF_tlev(k) &
         = ( 0.5 * ( ph_2(i,k,j) + ph_2(i,k+1,j)  &
                     - ph_1(i,k,j) - ph_1(i,k+1,j) ) / g ) / DT
      ENDDO

      ! Adjust w so that ( w - grid_velocity ) is sent into CLUBB.
      ! Momentum grid levels.
      DO k = k_start, kte+1, 1
         wm_zm(k) = wm_zm(k) - grid_vel_WRF_mlev(k)
      ENDDO
      ! Thermodynamic grid levels.
      ! The variable wm_zt is a CLUBB grid variable, while the variable
      ! grid_vel_WRF_tlev is a WRF grid variable.  As a result, the grid indices
      ! will be offset by one (see above diagram).
      DO k = k_start+1, kte+1, 1
         wm_zt(k) = wm_zt(k) - grid_vel_WRF_tlev(k-1)
      ENDDO


      ! ####### Other Large-scale Forcings #######

      ! All case-specific forcings should be computed in other parts of WRF.
      ! CLUBB's l_implemented flag (which is set to .true. when CLUBB is part
      ! of a host model, such as WRF) will not allow large-scale forcings to
      ! be added when CLUBB is part of a host model.  However, as the various
      ! forcing variables are part of the call to advance_clubb_core, they are
      ! required input and need to be set to 0 in this scenario.

      thlm_forcing(:)    = 0.0
      rtm_forcing(:)     = 0.0
      um_forcing(:)      = 0.0
      vm_forcing(:)      = 0.0
      wprtp_forcing(:)   = 0.0
      wpthlp_forcing(:)  = 0.0
      rtp2_forcing(:)    = 0.0
      thlp2_forcing(:)   = 0.0
      rtpthlp_forcing(:) = 0.0
      ! Passive scalar forcings.
      sclrm_forcing(:,:)   = 0.0
      edsclrm_forcing(:,:) = 0.0

      ! Coriolis force
      ! This is included in the WRF part of the WRF-CLUBB combination.
      ! CLUBB does not use Coriolis information if it is part of a host model.
      ! Therefore, set it to 0.
      fcor = 0.0


      ! ####### Setup CLUBB Core Time-step Information #######

      ! Set up CLUBB core time step (previously closure time step) information.
      dtclosure = DTBL

      niterlong = INT( DTBL / dtclosure )
      dt_clubb = dtclosure

      



#ifdef CLUBBSTATS
! Added for CLUBB statistics
      IF ( l_keep_CLUBB_stats .AND.  &
           grid_id == sample_domain .AND. &
           i == i_sample_col .AND. j == j_sample_col ) THEN

         l_stats = .true.

         ! Calculate time_initial, which is the time of the start of the model
         ! expressed in units of seconds since midnight on the start date.
         ! Subtract time_restart from time_initial: 
         !   If this is not a restart run, time_restart = 0 and has no effect
         !     on time_initial
         !   If this is a restart run, time_initial has changed from the
         !     time_initial of the original run; adjust it based on time_restart. 
         time_initial = sec_per_hr * start_hour  &
                        + sec_per_min * start_minute  &
                        + start_second &
                        - time_restart

         ! Set up main time step information
         ! dtmain is set to the PBL time step duration
         dtmain = DTBL
         stats_nsamp = nint(stats_tsamp/dtmain)
         stats_nout = nint(stats_tout/dtmain)

         ! Pre-CLUBB-loop time
         ! This is the set to be the time of the previous call to the CLUBB PBL
         ! scheme, or the model initial time if this is the first call.
         IF ( ITIMESTEP == 1 ) THEN
            ! First timestep, so pre-CLUBB time_current = time_initial
            time_current = time_initial
         ELSE  ! ITIMESTEP > 1
            ! After the first timestep, so pre-CLUBB time_current is the last
            ! time at which the PBL driver was called.
            time_current = time_initial + ( ITIMESTEP - NINT(DTBL/DT) ) * DT
         ENDIF

         call stats_begin_timestep_api( ITIMESTEP,stats_nsamp,stats_nout)

      ELSE

         ! Turn off all stats logicals for all columns besides
         ! the sampling column.
         l_stats       = .false.
         l_stats_samp  = .false.
         l_stats_last  = .false.

      ENDIF
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

      ! Set hydromet for stats purposes and for use with latin hypercube later
      DO k = k_start+1, kte+1, 1
        hydromet(k,iirr)    = qr(i,k-1,j)
        hydromet(k,iiNr)       = qnr(i,k-1,j)
        Nc_in_cloud(k)          = qndrop(i,k-1,j)
        hydromet(k,iirs)    = qs(i,k-1,j)
        hydromet(k,iiNs)    = qns(i,k-1,j)
        hydromet(k,iiri)     = qi(i,k-1,j)
        hydromet(k,iiNi)       = qni(i,k-1,j)
        hydromet(k,iirg) = qg(i,k-1,j)
        hydromet(k,iiNg) = qng(i,k-1,j)
      END DO
      hydromet(1,:) = 0.0 ! Set the ghost point to 0
      Nc_in_cloud(1) = 0.0 ! Set the ghost point to 0

      ! Note:  we could set wphydrometp in WRF-CLUBB accoring to:
      !        <w'hm'> = - K_r * d<hm>/dz.
      !        We will have to base the calculation off of the values of
      !        hydromet from a single timestep, unlike CLUBB standalone that
      !        has the benefit of using a Crank-Nicholson time-stepping scheme.
      wphydrometp = 0.0

      ! Set total ice phase water
      DO k = k_start+1, kte+1, 1
        rfrzm(k) = qi(i,k-1,j) + qs(i,k-1,j)
      END DO
      rfrzm(1) = 0.0

      ! thlp3 and rtp3 are used in CLUBB only if l_use_3D_closure is
      ! set to true in CLUBB's model_flags.F90. This flag and thlp3 and rtp3 are
      ! experimental as of 15-Jul 2015 and can only be used when thlp3 and rtp3
      ! are input to CLUBB using l_input_fields

      ! Assign dummy values
      thlp3_dummy(:) = 0.0
      rtp3_dummy(:) = 0.0

      ! ####### Call The CLUBB Boundary-Layer Parameterization #######
      ! This can be done once or multiple times.
      DO iter = 1, niterlong, 1

          call advance_clubb_core_api &
               ( l_implemented, dt_clubb, fcor, momentum_heights(1), hydromet_dim, &
                 thlm_forcing, rtm_forcing, um_forcing, vm_forcing, & 
                 sclrm_forcing, edsclrm_forcing, wprtp_forcing, &
                 wpthlp_forcing, rtp2_forcing, thlp2_forcing, &
                 rtpthlp_forcing, wm_zm, wm_zt, &
                 wpthlp_sfc, wprtp_sfc, upwp_sfc, vpwp_sfc, & 
                 wpsclrp_sfc, wpedsclrp_sfc, &
                 p_in_Pa, rho_zm, rho, exner(i,:,j), &
                 rho_ds_zm(i,:,j), rho_ds_zt(i,:,j), invrs_rho_ds_zm, &
                 invrs_rho_ds_zt, thv_ds_zm, thv_ds_zt, hydromet,&
                 rfrzm, radf, &
                 wphydrometp, wp2hmp, rtphmp, thlphmp, &
                 DX, DY, &
                 um, vm, upwp, vpwp, up2, vp2, & 
                 thlm, rtm, wprtp, wpthlp, &
                 wp2, wp3, rtp2, rtp3_dummy, thlp2, thlp3_dummy, rtpthlp, &
                 sclrm, &
                 sclrp2, sclrprtp, sclrpthlp, &
                 wpsclrp, edsclrm, err_code, &
                 rcm, cloud_frac, &
                 wpthvp, wp2thvp, rtpthvp, thlpthvp, &
                 sclrpthvp, &
                 pdf_params, pdf_params_zm, &
                 wprcp, ice_supersat_frac, &
                 rcm_in_layer, cloud_cover )

#ifdef SILHS
    !----------------------------------------------------------------
    ! Compute subcolumns if enabled
    !----------------------------------------------------------------
    IF ( lh_microphys_type /= lh_microphys_disabled ) THEN

       ! Set l_stats_samp temporary to .false. so that in the following second call
       ! to fill_holes_driver_api() does not call stats output which would lead to a
       ! sampling error  
       l_stats_samp_tmp = l_stats_samp
       l_stats_samp = .false.

       ! Apply hole filling
       CALL fill_holes_driver_api( kme, dt_clubb, & ! Intent(in)
                               hydromet_dim, l_fill_holes_hm, & ! Intent(in)
                               rho_ds_zm(i,:,j), rho_ds_zt(i,:,j), & ! Intent(in)
                               exner(i,:,j), & ! Intent(in)
                               th_lh_mc(i,:,j), & ! Intent(inout)
                               qv_lh_mc(i,:,j), & ! Intent(inout)
                               hydromet ) ! Intent(inout)

       l_stats_samp = l_stats_samp_tmp

       !!! Setup the PDF parameters.
       call setup_pdf_parameters_api( kme, pdf_dim, dt_clubb, &               ! Intent(in)
                                      Nc_in_cloud, rcm, cloud_frac, &             ! Intent(in)
                                      ice_supersat_frac, hydromet, wphydrometp, & ! Intent(in)
                                      corr_array_n_cloud, corr_array_n_below, &   ! Intent(in)
                                      pdf_params, l_stats_samp, &                 ! Intent(in)
                                      hydrometp2, &                               ! Intent(out)
                                      mu_x_1, mu_x_2, &                           ! Intent(out)
                                      sigma_x_1, sigma_x_2, &                     ! Intent(out)
                                      corr_array_1, corr_array_2, &               ! Intent(out)
                                      corr_cholesky_mtx_1, corr_cholesky_mtx_2, & ! Intent(out)
                                      hydromet_pdf_params )                       ! Intent(out)


       ! In every except the but for the very first timestep of a simulation initialize 
       ! the random number generator of SILHS with the seed saved in the registry.
       ! For restarted simulation this seed will come from the from the restart file (wrfrst*). 
       ! Every colum has its own random number generator and therefore its own
       ! state. That's why for every column the correct seed has to be loaded.
       ! The seed is saved as an integer array. Therefore the integers have to be converted 
       ! back to characters and subsequently to the internal representation of the generator.
       ! Here we use "achar" so the convertion from integer to character is independent
       ! from the machine.
       !
       ! Load the seed when this is not the first call to CLUBB_bl_scheme
       ! or if we are restarting
       if( (l_first_call_to_clubb_bl_scheme .eqv. .false.) .or. (restart .eqv. .true.) )then 

         ! Convert seed from integer to character variables 
         do lo =1,lh_seed_len 
           mt95_srepr%repr(lo:lo)=achar(lh_seed(i,j,lo)) 
         end do 

         ! Convert seed from external to internal representation 
         ! The operator "=" is overloaded! 
         mt95_state = mt95_srepr 

         ! Set the seed 
         call genrand_init_api( put=mt95_state )

       else
         ! This is the first very first step of the simulation.
         ! Initialize the random number generator of SILHS
         lh_seed_tmp = lh_seed_init  &
                      + int( i, kind = genrand_intg ) &
                      + int( j, kind = genrand_intg )
         call genrand_init_api( put=lh_seed_tmp )
       end if

       ! Calculate radiation only once in a while 
       ! l_rad_itime = (mod( itime, floor(dt_rad/dt_main) ) == 0 .or. itime == 1) 

       ! Calculate sample weights separately at all grid levels when radiation is not called 
       !l_calc_weights_all_levs_itime = l_calc_weights_all_levs .and. .not. l_rad_itime 
       l_calc_weights_all_levs_itime = .true.

       call generate_silhs_sample_api &
            ( lh_iter, pdf_dim, lh_microphys_calls, lh_sequence_length, kme, & ! In
              l_calc_weights_all_levs_itime, & ! In 
              pdf_params, gr%dzt, rcm, Lscale, & ! In
              rho_ds_zt(i,:,j), mu_x_1, mu_x_2, sigma_x_1, sigma_x_2, & ! In
              corr_cholesky_mtx_1, & ! In
              corr_cholesky_mtx_2, & ! In
              hydromet_pdf_params, & ! In
              X_nl_all_levs_raw, X_mixt_comp_all_levs,  & ! Out
              lh_sample_point_weights ) ! Out

        call clip_transform_silhs_output_api( kme, lh_microphys_calls, & ! In
                                              pdf_dim, hydromet_dim, & ! In
                                              X_mixt_comp_all_levs, & ! In
                                              X_nl_all_levs_raw, & ! In
                                              pdf_params, l_use_Ncn_to_Nc, & !In
                                              lh_clipped_vars, & ! Out
                                              X_nl_all_levs ) ! Out

        lh_thl = lh_clipped_vars%thl

        ! After generate_silhs_sample_api() is called (so basically, save the seed of the
        ! random number generator of SILHS in the grid. From there the seed it written 
        ! to the restart files (wrfrst*) if necessary.
        ! The seed is saved as an integer array. So the external represenataion of the
        ! seed (which is a string) has to be converted into an integer array.
        ! Here we use "iachar" so the convertion from character to integer is independent
        ! from the machine.
        
        ! Get the seed from randon number generator
        call genrand_init_api( get=mt95_state )

        ! Convert seed from internal to external representation
        ! The operator "=" is overloaded!
        mt95_srepr = mt95_state

        ! Save the length of the seed array
        ! This is important since the length of the seed is not fixed 
        lh_seed_len = len(mt95_srepr%repr)

        ! Convert seed from character to integer and save in the grid structure 
        do lo =1,lh_seed_len
          lh_seed(i,j,lo) = iachar(mt95_srepr%repr(lo:lo))
        end do


        lh_iter = lh_iter + 1 ! Increment iteration

        DO k = k_start, k_end, 1
          ! Compute liquid water from Mellor's 's' variable (extended saturation)
          qc_lh(i,k,j,:) = lh_clipped_vars(k+1,:)%rc

          ! Convert thlm to theta
          th_lh(i,k,j,:) = lh_thl(k+1,:) &
                         + ( XLV / ( Cp * exner(i,k,j) ) ) * qc_lh(i,k,j,:)

          ! Compute vapor water.  Occasionally the Gaussian distribution 
          ! can produce a negative mixing ratio, which has already been clipped
!          qv_lh(i,k,j,:) = max( lh_rt(k+1,:) - qc_lh(i,k,j,:), zero_threshold )
          qv_lh(i,k,j,:) = lh_clipped_vars(k+1,:)%rv

          ! Mixing ratio of rain, ice and snow
          qr_lh(i,k,j,:) = X_nl_all_levs(k+1,:,iiPDF_rr)
          qi_lh(i,k,j,:) = X_nl_all_levs(k+1,:,iiPDF_ri)
          qs_lh(i,k,j,:) = X_nl_all_levs(k+1,:,iiPDF_rs)
          qg_lh(i,k,j,:) = X_nl_all_levs(k+1,:,iiPDF_rg)

          ! Number concentration of cloud droplets, rain droplets, ice and snow
          qndrop_lh(i,k,j,:) = X_nl_all_levs(k+1,:,iiPDF_Ncn)
          qnr_lh(i,k,j,:)    = X_nl_all_levs(k+1,:,iiPDF_Nr)
          qni_lh(i,k,j,:)    = X_nl_all_levs(k+1,:,iiPDF_Ni)
          qns_lh(i,k,j,:)    = X_nl_all_levs(k+1,:,iiPDF_Ns)
          qng_lh(i,k,j,:)    = X_nl_all_levs(k+1,:,iiPDF_Ng)

        END DO

        weights_lh(i,:,j,:)  = lh_sample_point_weights(:,:)

        ! Interpolate vertical velocity to the staggered vertical grid
        DO icall = 1, lh_microphys_calls
          w_lh(i,:,j,icall) = zt2zm_api( real( X_nl_all_levs(:,icall,iiPDF_w) ) )
        END DO
   END IF ! lh_microphys_enabled
#endif /*SILHS*/

#ifdef CLUBBSTATS
! Added for CLUBB statistics
         IF ( l_keep_CLUBB_stats .AND.  &
              grid_id == sample_domain .AND. &
              i == i_sample_col .AND. j == j_sample_col ) THEN

            CALL stats_accumulate_hydromet_api( hydromet, rho_ds_zt(i,:,j) ) ! In
#ifdef SILHS
         IF ( lh_microphys_type /= lh_microphys_disabled ) THEN
            CALL stats_accumulate_lh_api &
                 ( kme, lh_microphys_calls, pdf_dim, rho_ds_zt, & ! In
                   lh_sample_point_weights, X_nl_all_levs, & ! In
                   lh_clipped_vars ) ! In
         ENDIF
#endif /*SILHS*/

            CALL stats_end_timestep_api( )

            ! SET TIME (during CLUBB-loop)
            ! Advance time here, in order to facilitate use of stats.
            ! A host model, e.g. WRF, would advance time outside
            ! of advance_clubb_core.  Vince Larson 7 Feb 2006
            IF ( iter < niterlong ) THEN
               time_current = time_initial  &
                              + ( ITIMESTEP - NINT(DTBL/DT) ) * DT  &
                              + iter * dt_clubb
            ELSEIF ( iter == niterlong ) THEN
               time_current = time_initial + ITIMESTEP * DT
            ENDIF

            ! This was moved from above to be less confusing to the user,
            ! since before it would appear as though the last timestep
            ! was not executed. -dschanen 19 May 08
            IF ( l_stats_last .AND. l_stdout ) THEN
               WRITE(UNIT=fstdout,FMT='(a,i8,a,f10.1)') 'iteration = ',  & 
                 ITIMESTEP, '; time = ', time_current
            ENDIF

         ELSE

            ! Turn off all stats logicals for all columns besides
            ! the sampling column.
            l_stats       = .false.
            l_stats_samp  = .false.
            l_stats_last  = .false.

         ENDIF
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/


         IF ( err_code /= clubb_no_error ) THEN
            WRITE(fstderr,*) "Error in column:  i = ", i, "; j = ", j
            STOP 'Fatal error output from advance_clubb_core_api'
         ENDIF

      ENDDO 


      !########## SAVE OUTPUT FROM CLUBB PARAMETERIZATION ##########

      ! Overwrite the WRF Storage variables with the 1-D CLUBB results for every
      ! grid column.
      ! NOTE:  All of the momentum-level variables listed below were specially
      !        created 3-D variables in the WRF Registry.  They are being used
      !        to save all of the CLUBB output (from every single column) for
      !        use by CLUBB during the next timestep.  The same is true for most
      !        of the thermodynamic-level variables listed below, namely
      !        wp3_sgs, rcm_CLUBB, and cf_CLUBB.

      ! Momentum-level variables.
      DO k = k_start, kte+1, 1

         wpthlp_sgs(i,k,j)     = wpthlp(k)
         wprtp_sgs(i,k,j)      = wprtp(k)
         wp2_sgs(i,k,j)        = wp2(k)
         rtp2_sgs(i,k,j)       = rtp2(k)
         thlp2_sgs(i,k,j)      = thlp2(k)
         rtpthlp_sgs(i,k,j)    = rtpthlp(k)
         up2_sgs(i,k,j)        = up2(k)
         vp2_sgs(i,k,j)        = vp2(k)
         upwp_sgs(i,k,j)       = upwp(k)
         vpwp_sgs(i,k,j)       = vpwp(k)
         wpthvp_sgs(i,k,j)     = wpthvp(k)
         rtpthvp_sgs(i,k,j)    = rtpthvp(k)
         thlpthvp_sgs(i,k,j)   = thlpthvp(k)
         wprcp_sgs(i,k,j)      = wprcp(k)

         ! Calculate w'th' from w'th_l', w'r_c', and the exner function
         ! (interpolated to momentum levels).
         wpthp_sgs(i,k,j) = wpthlp(k)  &
                            + ( XLV / ( cp * zt2zm_api( exner(i,:,j), k ) ) ) * wprcp(k)

      ENDDO

      ! Thermodynamic-level variables.
      ! Once again, note that WRF's vertical grid indices and CLUBB's vertical
      ! grid indices differ by 1.  See above diagram.
      DO k = k_start, k_end, 1

         wp3_sgs(i,k,j)       = wp3(k+1)
         wp2thvp_sgs(i,k,j)   = wp2thvp(k+1)
         rcm_CLUBB(i,k,j)     = rcm(k+1)
         cf_CLUBB(i,k,j)      = cloud_frac(k+1)
         rcm_lyr_CLUBB(i,k,j) = rcm_in_layer(k+1)  ! ldgrant added rcm_in_layer and
         cldcov_CLUBB(i,k,j)  = cloud_cover(k+1)   ! cloud_cover for input to the
                                                   ! radiation scheme.  -- July 2011

      ENDDO

      ! Save PDF parameters for the grid column.
      ! The number of PDF parameters found in pdf_params_CLUBB and
      ! pdf_params_zm_CLUBB is the value of the variable num_pdf_params, which
      ! is found in clubb/pdf_parameter_module.F90.  The variables
      ! num_pdf_params_CLUBB and num_pdf_params_zm_CLUBB both have a value of
      ! num_pdf_params + 1.  This happens because WRF adds an extra element (at
      ! index value 1) to the number of species in its 4-D arrays declared in
      ! the Registry.  The relevant indices in pdf_params_CLUBB and
      ! pdf_params_zm_CLUBB range from 2 to num_pdf_params_CLUBB and 2 to
      ! num_pdf_params_zm_CLUBB, respectively.
      ! Thermodynamic grid levels.
      call pack_pdf_params_api( &
           pdf_params(k_start+1:k_end+1), &                             ! In
           k_end-k_start+1, &                                           ! In
           pdf_params_CLUBB(i,k_start:k_end,j,2:num_pdf_params_CLUBB) ) ! Out
      ! Momentum grid levels.
      call pack_pdf_params_api( &
           pdf_params_zm(k_start:kte+1), &                                    ! In
           kte-k_start+2, &                                                   ! In
           pdf_params_zm_CLUBB(i,k_start:kte+1,j,2:num_pdf_params_zm_CLUBB) ) ! Out

      ! Note:  CLUBB's results for the first thermodynamic-level are thrown out,
      !        for they are located below the surface level and WRF does not
      !        have a level there.

      !#########################################################################
      !#########################################################################
      !#########################################################################


   ENDDO  ! OUTER j-loop
ENDDO     ! OUTER i-loop

! Determine tendencies to output back to WRF.
CALL output_tendencies( dtbl, flag_qc, dz8w, pi_phy,             &
                        qc_curr, rcm_CLUBB, rcm_CLUBB_old,       &
                        l_use_cldcov_rcm_lyr, rcm_lyr_CLUBB,     &
                        rcm_lyr_CLUBB_old,                           &
                        wpthlp_sgs, wprtp_sgs, wprcp_sgs,        &
                        upwp_sgs, vpwp_sgs, wpthp_sgs, RUBLTEN,  &
                        RVBLTEN, RTHBLTEN, RQVBLTEN, RQCBLTEN,   &
                        ids, ide, jds, jde, kds, kde,            &
                        ims, ime, jms, jme, kms, kme,            &
                        its, ite, jts, jte, kts, kte             )

#ifdef SILHS
    l_first_call_to_clubb_bl_scheme = .false.   
#endif /*SILHS*/


END SUBROUTINE CLUBB_bl_scheme

!===============================================================================

SUBROUTINE output_tendencies( dtbl, flag_qc, dz8w, pi_phy,             &
                              qc_curr, rcm_CLUBB, rcm_CLUBB_old,       &
                              l_use_cldcov_rcm_lyr, rcm_lyr_CLUBB,     &
                              rcm_lyr_CLUBB_old,                       &
                              wpthlp_sgs, wprtp_sgs, wprcp_sgs,        &
                              upwp_sgs, vpwp_sgs, wpthp_sgs, RUBLTEN,  &
                              RVBLTEN, RTHBLTEN, RQVBLTEN, RQCBLTEN,   &
                              ids, ide, jds, jde, kds, kde,            &
                              ims, ime, jms, jme, kms, kme,            &
                              its, ite, jts, jte, kts, kte             )

USE module_model_constants, ONLY: &
    XLV, &
    cp

IMPLICIT NONE

! Input variables

! Standard WRF variables to be brought into this subroutine.
! Indices for the domain, memory, and tile
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

REAL, INTENT(IN)::  &
      dtbl         ! Duration of PBL time step

LOGICAL, INTENT(IN):: &
      flag_qc,             &  ! Flag for WRF's microphysics scheme containing QC.
      l_use_cldcov_rcm_lyr    ! If true, use rcm_lyr_CLUBB and cldcov_CLUBB 
                              ! instead of rcm_CLUBB and cf_CLUBB 

REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      dz8w,         &   ! delta-z between full-levels of z
      pi_phy,       &   ! exner function (p/p0)^(Rd/cp)
      qc_curr,      &   ! WRF mean cloud water mixing ratio
      rcm_CLUBB,    &   ! CLUBB current mean cloud water mixing ratio
      rcm_CLUBB_old,&   ! CLUBB previous mean cloud water mixing ratio
      rcm_lyr_CLUBB,&   ! 
      rcm_lyr_CLUBB_old !

! CLUBB variables stored for entire 3-D WRF Field.
! Momentum-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      wpthlp_sgs, &  ! overbar[w'thl'] on subgrid-scale
      wprtp_sgs,  &  ! overbar[w'rt'] on subgrid-scale
      wprcp_sgs,  &  ! overbar[w'rc'] on subgrid-scale
      upwp_sgs,   &  ! overbar[u'w'] on subgrid-scale
      vpwp_sgs,   &  ! overbar[v'w'] on subgrid-scale
      wpthp_sgs      ! overbar[w'th'] on subgrid-scale

! Output variables

! Standard WRF tendency variables to be taken out of this subroutine.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(OUT)::  &
      RUBLTEN,  &  ! WRF du/dt due to BL scheme
      RVBLTEN,  &  ! WRF dv/dt due to BL scheme
      RTHBLTEN, &  ! WRF dth/dt due to BL scheme
      RQVBLTEN, &  ! WRF dqv/dt due to BL scheme
      RQCBLTEN     ! WRF dqc/dt due to BL scheme

! Local variables

REAL :: dqc_dt  ! Rate of change of qc (rcm_CLUBB) over time

INTEGER:: k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

k_start = kts
k_end   = MIN(kde-1, kte)

! Tendencies to send back into WRF:
!
! (dTH/dt)_BL_CLUBB = - d/dz [w'thl']
! (dQV/dt)_BL_CLUBB = - d/dz [w'rt']
! (dQC/dt)_BL_CLUBB = ( QC_new - QC_old ) / dt = - d/dz [w'rc']
! (dU/dt)_BL_CLUBB  = - d/dz [u'w']
! (dV/dt)_BL_CLUBB  = - d/dz [v'w']

DO i = its, ite, 1
   DO j = jts, jte, 1
      DO k = k_start, k_end, 1

         IF ( flag_qc ) THEN

            ! The selected WRF microphysics scheme contains cloud water mixing
            ! ratio.

            !RQCBLTEN(i,k,j)  &
            != - ( wprcp_sgs(i,k+1,j) - wprcp_sgs(i,k,j) ) / dz8w(i,k,j)
            !RQCBLTEN(i,k,j)  &
            != real( ( rcm_CLUBB(i,k,j) - rcm_CLUBB_old(i,k,j) ) / dtbl )
            IF ( l_use_cldcov_rcm_lyr ) THEN
              RQCBLTEN(i,k,j)  &
              = real( ( rcm_CLUBB(i,k,j) - qc_curr(i,k,j) ) / dtbl )
            ELSE
              RQCBLTEN(i,k,j)  &
              = real( ( rcm_lyr_CLUBB(i,k,j) - qc_curr(i,k,j) ) / dtbl )
            ENDIF 

            !RTHBLTEN(i,k,j)  &
            != - ( wpthp_sgs(i,k+1,j) - wpthp_sgs(i,k,j) ) / dz8w(i,k,j)
            !RTHBLTEN(i,k,j)  &
            != - ( wpthlp_sgs(i,k+1,j) - wpthlp_sgs(i,k,j) ) / dz8w(i,k,j)  &
            !  - ( XLV / ( cp * pi_phy(i,k,j) ) )  &
            !    * ( wprcp_sgs(i,k+1,j) - wprcp_sgs(i,k,j) ) / dz8w(i,k,j)
            RTHBLTEN(i,k,j)  &
            = - ( wpthlp_sgs(i,k+1,j) - wpthlp_sgs(i,k,j) ) / dz8w(i,k,j)  &
              + ( XLV / ( cp * pi_phy(i,k,j) ) ) * RQCBLTEN(i,k,j)

            !RQVBLTEN(i,k,j)  &
            != - ( wprtp_sgs(i,k+1,j) - wprtp_sgs(i,k,j) ) / dz8w(i,k,j)  &
            !  + ( wprcp_sgs(i,k+1,j) - wprcp_sgs(i,k,j) ) / dz8w(i,k,j)
            RQVBLTEN(i,k,j)  &
            = - ( wprtp_sgs(i,k+1,j) - wprtp_sgs(i,k,j) ) / dz8w(i,k,j)  &
              - RQCBLTEN(i,k,j)

         ELSE  ! flag_qc = .false.

            ! The selected WRF microphysics scheme does not contain cloud water
            ! mixing ratio.  Currently, the only scheme that does this is WRF's
            ! mp_physics = 0 (PASSIVEQV).

            IF ( l_use_cldcov_rcm_lyr ) THEN
              dqc_dt = real( ( rcm_CLUBB(i,k,j) - rcm_CLUBB_old(i,k,j) ) / dtbl )
            ELSE
              dqc_dt = real( ( rcm_lyr_CLUBB(i,k,j) - rcm_lyr_CLUBB_old(i,k,j) ) / dtbl )
            ENDIF 

            RTHBLTEN(i,k,j)  &
            = - ( wpthlp_sgs(i,k+1,j) - wpthlp_sgs(i,k,j) ) / dz8w(i,k,j)  &
              + ( XLV / ( cp * pi_phy(i,k,j) ) ) * dqc_dt

            RQVBLTEN(i,k,j)  &
            = - ( wprtp_sgs(i,k+1,j) - wprtp_sgs(i,k,j) ) / dz8w(i,k,j)
            
         ENDIF

         RUBLTEN(i,k,j)  &
         = - ( upwp_sgs(i,k+1,j) - upwp_sgs(i,k,j) ) / dz8w(i,k,j)

         RVBLTEN(i,k,j)  &
         = - ( vpwp_sgs(i,k+1,j) - vpwp_sgs(i,k,j) ) / dz8w(i,k,j)

      ENDDO
   ENDDO
ENDDO


END SUBROUTINE output_tendencies

!===============================================================================


FUNCTION LIN_EXT( var_high, var_low, height_high, height_low, height_ext )

! This function computes a linear extension of the value of variable.
! Given two known values of a variable at two height values, the value
! of that variable at a height outside of those two height levels 
! (rather than a height between those two height levels) is computed.
!
! Here is a diagram:
!
!  -------------------------------- Height to be extended to; linear extension
!
!  ################################ Height high, know variable value
!
!
!
!  ################################ Height low, know variable value
!
!
!
!  -------------------------------- Height to be extended to; linear extension
!
!
! FORMULA:
!
! variable(@ Height extension) =
!
! [ (variable(@ Height high) - variable(@ Height low)) / (Height high - Height low) ]
! * (Height extension - Height high)  +  variable(@ Height high)

IMPLICIT NONE

! Input Variables
REAL, INTENT(IN):: var_high
REAL, INTENT(IN):: var_low
REAL, INTENT(IN):: height_high
REAL, INTENT(IN):: height_low
REAL, INTENT(IN):: height_ext

! Output Variable
REAL:: lin_ext

lin_ext = ( var_high - var_low ) / ( height_high - height_low ) &
         * ( height_ext - height_high ) + var_high


END FUNCTION LIN_EXT

!===============================================================================

FUNCTION LIN_INT( var_high, var_low, height_high, height_low, height_int )

! This function computes a linear interpolation of the value of variable.
! Given two known values of a variable at two height values, the value
! of that variable at a height between those two height levels (rather 
! than a height outside of those two height levels) is computed.
!
! Here is a diagram:
!
!  ################################ Height high, know variable value
!
!
!
!  -------------------------------- Height to be interpolated to; linear interpolation
!
!
!
!
!
!  ################################ Height low, know variable value
!
!
! FORMULA:
!
! variable(@ Height interpolation) =
!
! [ (variable(@ Height high) - variable(@ Height low)) / (Height high - Height low) ]
! * (Height interpolation - Height low)  +  variable(@ Height low)

IMPLICIT NONE

! Input Variables
REAL, INTENT(IN):: var_high
REAL, INTENT(IN):: var_low
REAL, INTENT(IN):: height_high
REAL, INTENT(IN):: height_low
REAL, INTENT(IN):: height_int

! Output Variable
REAL:: lin_int

lin_int = ( var_high - var_low ) / ( height_high - height_low ) &
         * ( height_int - height_low ) + var_low


END FUNCTION LIN_INT

!===============================================================================

SUBROUTINE CLUBB_bl_init ( RUBLTEN, RVBLTEN, RTHBLTEN, RQVBLTEN, RQCBLTEN,   &
                           config_flags, restart, phb, ph_2, tke_2,  &
#ifdef CLUBBSTATS
                           ! Added for CLUBB statistics
                           DT, STEPBL, xlat, xlong, xlat_v, xlong_u,         &
                           ! CLUBB statistics changes end
#endif /*CLUBBSTATS*/
                           wpthlp_sgs, wprtp_sgs, wp2_sgs,                   &
                           rtp2_sgs, thlp2_sgs, rtpthlp_sgs,                 &
                           up2_sgs, vp2_sgs, upwp_sgs, vpwp_sgs,             &
                           wpthvp_sgs, rtpthvp_sgs, thlpthvp_sgs,            &
                           wprcp_sgs, wpthp_sgs,                             &
                           wp3_sgs, wp2thvp_sgs, rcm_CLUBB, cf_CLUBB,        &
                           rcm_lyr_CLUBB, cldcov_CLUBB,                      &
                           pdf_params_CLUBB, pdf_params_zm_CLUBB,            &
                           ids, ide, jds, jde, kds, kde,                     &
                           ims, ime, jms, jme, kms, kme,                     &
                           its, ite, jts, jte, kts, kte                      )

! CLUBB modules that need to be referenced by this subroutine.
use clubb_api_module, only:  &
    setup_clubb_core_api, &  ! Procedure

    setup_grid_heights_api, & ! Procedures
    zt2zm_api, &
    gr, &       ! Variables

    w_tol_sqd, & ! Constants
    rt_tol, &
    thl_tol, &
    em_min, &
    fstderr, &

    l_tke_aniso, & ! Variables
    l_diagnose_correlations, &
    l_calc_w_corr, &
    l_use_precip_frac, &

    l_fix_w_chi_eta_correlations, & ! Variable(s)
    l_const_Nc_in_cloud, &

    nparams, &  ! Number of CLUBB parameters to read in.

    read_parameters_api, &  ! Subroutine to read CLUBB parameters.

    set_clubb_debug_level_api, &   ! Procedure(s)
    clubb_no_error   ! Variable(s)

#ifdef CLUBBSTATS
! Added for CLUBB statistics
use clubb_api_module, only: &
    l_stats, &

    stats_init_api, &  ! Procedure

    fstdout, &    ! Constant(s)
    sec_per_hr, &
    sec_per_min
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

! WRF modules that might need to be referenced by this subroutine.
use module_configure, only:  &
    grid_config_rec_type, & ! Type
    model_config_rec  ! Variable(s); flags and settings for all of WRF's domains

use module_model_constants, only: &
    g          ! Acceleration Due To Gravity        [m/s^2]

use module_state_description, ONLY: &
    KESSLERSCHEME,           & ! Variable(s)
    CLUBBSCHEME,             &
    num_pdf_params_CLUBB,    &
    num_pdf_params_zm_CLUBB

use clubb_api_module, only: &
  iirr, iiNr, iirs, iiri, iirg, & ! Variables
  iiNs, iiNi, iiNg

use module_configure, only : grid_config_rec_type ! Variable

use module_wrf_error

IMPLICIT NONE

TYPE(grid_config_rec_type), INTENT(IN)::  &
      config_flags  ! Configuration flags for current domain

! Indices for the domain, memory, and tile
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

LOGICAL, INTENT(IN):: restart   ! Flag For Model Restart Run

! Standard WRF variables to be brought into this subroutine.
! Momentum-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      phb,  &  ! Base-state Geopotential                       [m^2/s^2]
      ph_2     ! Perturbation Geopotential                     [m^2/s^2]

! Standard WRF variables to be brought into this subroutine.
! Thermodynamic-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN)::  &
      tke_2    ! Turbulence Kinetic Energy                     [m^2/s^2]

#ifdef CLUBBSTATS
! Added for CLUBB statistics
REAL, INTENT(IN)::  &
      DT     ! Model Time Step Duration                    [s]

INTEGER, INTENT(IN)::  &
      STEPBL ! Call PBL driver every STEPBL timesteps      [-]

REAL, DIMENSION(ims:ime,jms:jme), INTENT(IN)::  &
      xlat,    & ! Latitude of center of grid column            [degrees north]
      xlong,   & ! Longititude of center of grid column         [degrees east]
      xlat_v,  & ! Latitude of V-momentum points (column edge)  [degrees north]
      xlong_u    ! Longitude of U-momentum points (column edge) [degrees east]
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

! Standard WRF tendency variables to be initialized in this subroutine.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(OUT)::  &
      RUBLTEN,  &  ! du/dt due to BL scheme.
      RVBLTEN,  &  ! dv/dt due to BL scheme.
      RTHBLTEN, &  ! dth/dt due to BL scheme.
      RQVBLTEN, &  ! dqv/dt due to BL scheme.
      RQCBLTEN     ! dqc/dt due to BL scheme.

! CLUBB variables stored for entire 3-D WRF Field.
! Momentum-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT):: &
      wpthlp_sgs,     & ! subgrid-scale <w'thl'> (momentum levels)   [m/s K]
      wprtp_sgs,      & ! subgrid-scale <w'rt'> (momentum levels)    [m/s kg/kg]
      wp2_sgs,        & ! subgrid-scale <w'^2> (momentum levels)     [m^2/s^2]
      rtp2_sgs,       & ! subgrid-scale <rt'^2> (momentum levels)    [kg^2/kg^2]
      thlp2_sgs,      & ! subgrid-scale <thl'^2> (momentum levels)   [K^2]
      rtpthlp_sgs,    & ! subgrid-scale <rt'thl'> (momentum levels)  [kg/kg K]
      up2_sgs,        & ! subgrid-scale <u'^2> (momentum levels)     [m^2/s^2]
      vp2_sgs,        & ! subgrid-scale <v'^2> (momentum levels)     [m^2/s^2]
      upwp_sgs,       & ! subgrid-scale <u'w'> (momentum levels)     [m^2/s^2]
      vpwp_sgs,       & ! subgrid-scale <v'w'> (momentum levels)     [m^2/s^2]
      wpthvp_sgs,     & ! subgrid-scale <w'thv'> (momentum levels)   [m/s K]
      rtpthvp_sgs,    & ! subgrid-scale <rt'thv'> (momentum levels)  [kg/kg K]
      thlpthvp_sgs,   & ! subgrid-scale <thl'thv'> (momentum levels) [K^2]
      wprcp_sgs,      & ! subgrid-scale <w'rc'> (momentum levels)    [m/s kg/kg]
      wpthp_sgs         ! subgrid-scale <w'th'> (momentum levels)    [m/s K]

! CLUBB variables stored for entire 3-D WRF Field.
! Thermodynamic-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT)::  &
      wp3_sgs,       & ! subgrid-scale <w'^3> (thermodynamic levels) [m^3/s^3]
      wp2thvp_sgs,   & ! subgrid-scale <w'^2 thv'> (thermo. levels)  [m^2/s^2 K]
      rcm_CLUBB,     & ! CLUBB's mean cloud water mixing ratio       [kg/kg]
      cf_CLUBB,      & ! CLUBB's grid-box cloud fraction             [-]
      rcm_lyr_CLUBB, & ! CLUBB rcm within the cloud layer            [kg/kg]
      cldcov_CLUBB     ! CLUBB cloud cover                           [-]

! CLUBB PDF parameters stored for entire 3-D WRF Field.
! Thermodynamic-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_pdf_params_CLUBB), &
INTENT(INOUT)::  &
      pdf_params_CLUBB    ! CLUBB's PDF parameters                  [units vary]

! CLUBB PDF parameters stored for entire 3-D WRF Field.
! Momentum-level variables.
REAL, DIMENSION(ims:ime,kms:kme,jms:jme,num_pdf_params_zm_CLUBB), &
INTENT(INOUT)::  &
      pdf_params_zm_CLUBB    ! CLUBB's PDF parameters (mom. levels) [units vary]

! Local variables.

! CLUBB variables on 1-D CLUBB Field.
! Thermodynamic-level variables.
REAL, DIMENSION(kms:kme)::  &
      tke_CLUBB_zt,         &  ! Subgrid TKE on thermodynamic levels.
      thermodynamic_heights    ! CLUBB thermodynamic level altitudes.

! Momentum-level variables.
REAL, DIMENSION(kms:kme)::  &
      em,              &  ! CLUBB subgrid TKE (momentum levels)
      momentum_heights    ! CLUBB momentum level altitudes.

INTEGER:: nzmax  ! Number of WRF vertical momentum levels.

REAL, DIMENSION(nparams):: clubb_params  ! CLUBB parameters.

CHARACTER(LEN=6) ::  &
      saturation_formula  ! Formula for calculating Saturation Vapor Pressure
                          ! CLUBB can use "flatau" or "bolton"

LOGICAL, PARAMETER :: l_input_fields = .false. ! Always disabled for WRF-CLUBB.

! Error code that is sent out of CLUBB core setup.
INTEGER:: err_code

REAL, DIMENSION(sclr_dim):: sclr_tol  ! Scalar variable tolerances.

LOGICAL:: l_initialize_CLUBB_vars  ! Flag to initialize CLUBB variables

INTEGER:: parent_dom_col_indx  ! namelist.input column index of parent domain

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k, iter

#ifdef CLUBBSTATS
! Added for CLUBB statistics
INTEGER::  &
      iunit, & ! File unit used for I/O
      day,   & ! Day (of the month) of the start of the simulation
      month, & ! Month of the start of the simulation
      year     ! Year of the start of the simulation

INTEGER:: &
      incr     ! index

INTEGER, DIMENSION(config_flags%max_dom):: &
      avail_grid_id  ! Domain ID's of WRF domains that use the CLUBB PBL scheme

INTEGER:: &
      num_domains_with_CLUBB  ! Number of WRF domains that use CLUBB PBL scheme

LOGICAL:: &
      l_sel_stats_dom_valid  ! Flag for if the user-selected WRF domain for the
                             ! CLUBB stats column is a valid WRF domain that
                             ! uses the CLUBB PBL scheme.

CHARACTER(len=10)::  &
      stats_fmt  ! File format for stats; typically GrADS.

CHARACTER(len=100)::  &
      runfile  ! Name of file containing &statsnl

CHARACTER(len=100)::  &
      fname_prefix, & ! Prefix of stats filenames, to be followed by,
                      ! for example, "_zt"
      fdir            ! Output directory

INTEGER::  &
      total_atmos_dim    ! Total vertical levels including extended atmosphere

REAL, DIMENSION(kms:kme)::  &
      complete_alt,      & ! CLUBB altitudes including extended atm.: t-levs
      complete_momentum    ! CLUBB altitudes including extended atm.: m-levs

REAL, DIMENSION(1)::  &
      rlat, & ! Latitude  [Degrees North]
      rlon    ! Longitude [Degrees East]

REAL(kind=core_rknd)::  &
      dtmain,       & ! Main model timestep             [s]
      stats_tsamp,  & ! Stats sampling interval         [s]
      stats_tout      ! Stats output interval           [s]

REAL(kind=time_precision):: &
      time_current    ! Current time of simulation      [s]

logical :: l_silhs_out
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

!------------------------------------------------------------------------------

#ifdef CLUBBSTATS
WRITE(fstdout,*) "in CLUBB_bl_init"
#endif /*CLUBBSTATS*/

! Hardwire some flags that are used in setup_pdf_parameters
l_use_precip_frac = config_flags%l_use_precip_frac
l_diagnose_correlations = .false.
l_calc_w_corr = .false.
l_fix_w_chi_eta_correlations = .true.
l_const_Nc_in_cloud = config_flags%l_const_Nc_in_cloud

! Here taking the minimim is necesarry because it is not done before.
i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

! Initialize the error code
err_code = clubb_no_error

#ifdef CLUBBSTATS
! Define  the column for which CLUBB statictics is outputted. The coordinates
! are defined in namelist.input as the variables clubb_i_sample_col and
! clubb_j_sample_col.
! If the column declared is outside of the i or j domains, then the column
! selected for CLUBB stats will be the central column, or in a column that
! borders the center of the selected model domain.
set_i_sample_col = config_flags%clubb_i_sample_col
set_j_sample_col = config_flags%clubb_j_sample_col
#endif /*CLUBBSTATS*/


! Call CLUBB setup
!
! CLUBB subroutine read_parameters reads in the values of the CLUBB parameters
! that are found in the file "clubbprm".  CLUBB subroutine set_clubb_debug_level
! sets the CLUBB model's debug level.  Valid values are 0, 1, and 2.  Subroutine
! setup_clubb_core sets up the CLUBB code for use.  Due to the fact that many
! variables (both grid variables and diagnostic variables) are allocated when
! setup_clubb_core is called, the subroutine must only be called ONCE.
! Otherwise, the CLUBB model code will attempt to allocate variables more than
! once, and therefore produce an error which stops the WRF-CLUBB model run.
!
! In a scenario where WRF is using a nested grid, this initialization subroutine
! (CLUBB_bl_init) may be called multiple times.  Therefore, only call CLUBB's
! setup_clubb_core the first time that subroutine CLUBB_bl_init is called.  This
! is usually from WRF's main domain.  However, in a nested grid scenario, the
! user has the option to declare different physics options for different
! domains.  Thus, CLUBB_bl_init will be called first from the "most parental
! domain" that uses CLUBB as its PBL scheme.
IF ( l_first_call_to_CLUBB_bl_init ) THEN

   ! Read in the values of CLUBB parameters from a namelist.
   call read_parameters_api( 50, "../clubb_input/tunable_parameters/tunable_parameters.in", clubb_params )

   ! Set the debug level as given in namelist.input.  Level 2 has additional computational expense since it
   ! checks the array variables in CLUBB for invalid values.
   !        0 => Print no debug messages to the screen
   !        1 => Print lightweight debug messages, e.g. print statements
   !        2 => Print debug messages that require extra testing,
   !             e.g. checks for NaNs and spurious negative values.
   IF (config_flags%clubb_debug_level >= 0 .and. config_flags%clubb_debug_level <= 2 ) THEN
       call set_clubb_debug_level_api( config_flags%clubb_debug_level )
   ELSE
       WRITE( wrf_err_message , * ) 'CLUBB_debug_level mustb be 0,1 or 2: clubb_debug_level = ', config_flags%clubb_debug_level
       CALL wrf_error_fatal ( wrf_err_message )
   ENDIF

   ! ####### Setup CLUBB Core #######

   ! Since subroutine setup_clubb_core wants the momentum level and
   ! thermodynamic level altitudes as input, they will be declared here for
   ! column (i=its,j=jts).  The grid heights can be overwritten for every column
   ! later, without new memory allocation, by calling CLUBB's subroutine
   ! setup_grid_heights.

   ! Setup momentum-level grid heights.  This is a one-dimensional array used
   ! for CLUBB based on the altitudes in a given column from the
   ! three-dimensional array used by WRF.
   do k = k_start, kte, 1
      momentum_heights(k) = ( phb(its,k,jts) + ph_2(its,k,jts) ) / g
   enddo

   ! Setup thermodynamic-level grid heights.  This is also a one-dimensional
   ! array used for CLUBB based on the altitudes in a given column from the
   ! three-dimensional array used by WRF.  WRF's thermodynamic levels are always
   ! located exactly halfway between the momentum levels that encase them.  The
   ! indices of WRF's thermodynamics levels and CLUBB's thermodynamic levels
   ! differ by one due to the fact that CLUBB's first thermodynamic level is
   ! below the model surface (see above diagram).
   do k = k_start+1, kte, 1
      thermodynamic_heights(k)  &
         = (1.0/2.0) * ( momentum_heights(k) + momentum_heights(k-1) )
   enddo
   ! The first thermodynamic level (which is below the model surface) is set to
   ! be the same distance below the first momentum level as the second
   ! thermodynamic level is above the first momentum level.
   thermodynamic_heights(k_start)  &
      = ( momentum_heights(k_start) - thermodynamic_heights(k_start+1) ) &
        + momentum_heights(k_start)

   ! Number of vertical levels (the number of vertical levels for both the CLUBB
   ! momentum and thermodynamic grids is the same as the number of vertical
   ! levels for the WRF momentum grid).
   nzmax = kte

   ! Set scalar tolerances.
   do i = 1, sclr_dim, 1
      sclr_tol(i) = 1.0e-2
   enddo

   ! Declare the name of the CLUBB saturation formula.
   SELECT CASE ( config_flags%mp_physics )

      CASE ( KESSLERSCHEME )

         ! WRF's Kessler scheme uses Bolton. 
         saturation_formula = "bolton"  ! Bolton 1980 approx. for SVP

      CASE DEFAULT

         ! CLUBB currently uses Flatau by default.
         saturation_formula = "flatau"  ! Flatau polynomial approx. for SVP

   END SELECT

   ! Set up CLUBB interface.
   call setup_clubb_core_api &
        ( nzmax, theta0, ts_nudge, &                            ! Intent(in)
          hydromet_dim, sclr_dim, &                             ! Intent(in)
          sclr_tol, edsclr_dim, clubb_params, &                 ! Intent(in)
          l_host_applies_sfc_fluxes, &                          ! Intent(in)
          l_uv_nudge, saturation_formula, &                     ! Intent(in)
          l_input_fields, &                                     ! Intent(in)
          l_implemented, grid_type, deltaz, zm_init, zm_top, &  ! Intent(in)
          momentum_heights, thermodynamic_heights, &            ! Intent(in)
          momentum_heights(1), &                                ! Intent(in)
          err_code )                                            ! Intent(out)

   IF ( err_code /= clubb_no_error ) THEN
      STOP 'Fatal error output from setup_clubb_core_api'
   ENDIF

#ifdef SILHS
   CALL CLUBB_lh_init( config_flags )
#endif /*SILHS*/

ENDIF  ! l_first_call_to_CLUBB_bl_init


#ifdef CLUBBSTATS
! Added for CLUBB statistics

! Find which WRF domains use CLUBB as the PBL scheme.
!
! Initialize the counter.
incr = 0
! Loop over all WRF domains.  In a nested grid scenario, there is more than one
! available domain.
DO iter = 1, config_flags%max_dom, 1

   ! For each domain, check if CLUBB is used as the PBL scheme.
   IF ( model_config_rec%bl_pbl_physics(iter) == CLUBBSCHEME ) THEN

      ! If the given WRF domain uses CLUBB as the PBL scheme, increment
      ! the counter.
      incr = incr + 1

      ! List the available WRF domains that use CLUBB as the PBL scheme.
      avail_grid_id(incr) = model_config_rec%grid_id(iter)

   ENDIF

ENDDO

! Store the total number of WRF domains that use CLUBB as the PBL scheme.
num_domains_with_CLUBB = incr

! Determine if the user-selected WRF domain for the CLUBB stats column is valid.
! In other words, does the user-selected domain (stored in set_sample_domain)
! match a WRF domain that uses CLUBB as the PBL scheme?  If it does, then 
! logical variable l_sel_stats_dom_valid is set to .true.  If it does not, then
! the logical is set to .false.
!
! Initialize l_sel_stats_dom_valid to .false.
l_sel_stats_dom_valid = .false.
! Loop over all WRF domains that use CLUBB as the PBL scheme.
DO iter = 1, num_domains_with_CLUBB, 1

   ! If the user-selected WRF domain for CLUBB stats matches a WRF domain id
   ! that uses CLUBB as the PBL scheme, set l_sel_stats_dom_valid to .true. and
   ! exit.
   IF ( set_sample_domain == avail_grid_id(iter) ) THEN
      l_sel_stats_dom_valid = .true.
      EXIT
   ENDIF

ENDDO

! Select a WRF domain for the CLUBB statistical column.
IF ( l_sel_stats_dom_valid ) THEN
   ! The user-selected WRF domain for the CLUBB statistical column (stored in
   ! set_sample_domain) matches a valid WRF domain that uses CLUBB as the PBL
   ! scheme.  Thus, the user-selected domain will be used for the CLUBB
   ! statistical column.
   sample_domain = set_sample_domain
ELSE
   ! The user-selected WRF domain is invalid.  Automatically select a WRF domain
   ! for the CLUBB statistical column.  The automatically-selected domain will
   ! be "the most-parental WRF domain" that uses CLUBB as the PBL scheme.  This
   ! is found in avail_grid_id(1).
   sample_domain = avail_grid_id(1)
ENDIF

! Declare column (within the domain) to use for CLUBB to keep its stats.
IF ( config_flags%grid_id == sample_domain ) THEN

   ! Use domain indices (ids, ide-1, etc.) rather than i_start, i_end, etc.
   ! for choosing the stats column because when running in parallel, 
   ! i_start, i_end, etc. differ for each subdomain and the stats column will 
   ! be different for each subdomain.  Then the column for which stats are 
   ! actually written may not be what the user expects. --ldgrant Jul 2010
   IF (       ( set_i_sample_col >= ids   ) &
        .AND. ( set_i_sample_col <= ide-1 ) &
        .AND. ( set_j_sample_col >= jds   ) &
        .AND. ( set_j_sample_col <= jde-1 ) ) THEN
      ! Use the column that was declared above.
      ! To setup CLUBB stats to automatically use a central column, set
      ! variables set_i_sample_col and set_j_sample_col to 0.
      i_sample_col = set_i_sample_col
      j_sample_col = set_j_sample_col
   ELSE
      ! A column isn't declared or is outside of the model domain.  CLUBB will
      ! keep its stats in the central column, or in a column that borders the
      ! center of the selected model domain.
      i_sample_col = INT( 0.5 * ( ids + ide-1 ) )
      j_sample_col = INT( 0.5 * ( jds + jde-1 ) )
   ENDIF

   ! Check whether the CLUBB stats column is within this patch. --ldgrant Sept 2010
   IF (       ( i_sample_col >= i_start ) &
        .AND. ( i_sample_col <= i_end   ) &
        .AND. ( j_sample_col >= j_start ) &
        .AND. ( j_sample_col <= j_end   ) ) THEN
      ! The CLUBB stats column is within this particular patch, so
      ! set l_sample_patch to true.
      l_sample_patch = .true.
   ELSE
      ! The CLUBB stats column is not within this particular patch,
      ! so l_sample_patch is false.
      l_sample_patch = .false.
   ENDIF

ENDIF
! CLUBB statistics changes end
#endif /*CLUBBSTATS*/


! ####### Initialization #######

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1

      ! In the scenario of a restart, the BL tendencies and the values of the
      ! CLUBB variables are read in from the restart file.  Do not overwrite!
      ! Note:  We need to develop an option to initialize for a nested domain
      !        if this is a restart run, but the nested domain hasn't reached
      !        its starting point yet (thus, there would not be a restart file
      !        for the particular domain).
      IF (.not. restart) THEN

        ! Initialize tendencies
        DO k = k_start, k_end, 1
          RUBLTEN(i,k,j)  = 0.0
          RVBLTEN(i,k,j)  = 0.0
          RTHBLTEN(i,k,j) = 0.0
          RQVBLTEN(i,k,j) = 0.0
          RQCBLTEN(i,k,j) = 0.0
        END DO ! k = k_start, k_end, 1

         ! In a single-domain scenario, the CLUBB variables need to be
         ! initialized here.
         ! In a nested-grid scenario, the CLUBB variables need to be initialized
         ! here if this is the main domain (domain 1, or ultra-parent domain),
         ! if this domain uses CLUBB.  However, the initialization of CLUBB
         ! variables for nested (or child) domains that use CLUBB depend on
         ! whether or not the parent domain uses CLUBB.  If the parent domain
         ! uses CLUBB, the values from the parent domain are interpolated onto
         ! the child domain when the child domain is initialized.  Do not
         ! overwrite these values!  However, if the parent domain does not use
         ! CLUBB, then the initialization must take place here for the child
         ! domain.
         ! Determine the namelist.input column of the parent domain of the
         ! current domain.  The reason this is needed, rather than using
         ! config_flags%parent_id in the later code, is that grid_id numbers can
         ! be changed in namelist.input so they don't match the namelist.input
         ! column number.  If this is the main domain (or ultra-parent) there
         ! will not be a match.
         DO iter = 1, config_flags%max_dom, 1
            IF ( config_flags%parent_id  &
                 == model_config_rec%grid_id(iter) ) THEN
               parent_dom_col_indx = iter
               EXIT
            ENDIF
         ENDDO
         ! Determine the scenario for the current domain.
         ! The 1st column of namelist.input must always be the main domain (or
         ! the ultra-parent).
         IF ( config_flags%grid_id == model_config_rec%grid_id(1) ) THEN
            ! This is the main domain (domain 1 or ultra-parent domain).
            ! Since this subroutine is being called, it obviously uses CLUBB as
            ! the PBL scheme.
            ! Initialize CLUBB variables. 
            l_initialize_CLUBB_vars = .true.
         ELSEIF ( model_config_rec%bl_pbl_physics(parent_dom_col_indx)  &
                  /= CLUBBSCHEME ) THEN
            ! This is a nested domain (or child domain).
            ! Since this subroutine is being called, it obviously uses CLUBB as
            ! the PBL scheme.  However, its parent does not use CLUBB, so the
            ! values of CLUBB variables can't be initialized from the parent
            ! domain.
            ! Initialize CLUBB variables.
            l_initialize_CLUBB_vars = .true.
         ELSE
            ! This is a nested domain (or child domain).
            ! Since this subroutine is being called, it obviously uses CLUBB as
            ! the PBL scheme.  Furthermore, its parent uses CLUBB as the PBL
            ! scheme.  The values of CLUBB's variables will be initialized by
            ! being interpolated onto this domain from the parent domain.
            ! Do not overwrite!  Do not initialize CLUBB variables.
            l_initialize_CLUBB_vars = .false.
         ENDIF

         IF ( l_initialize_CLUBB_vars ) THEN

            ! Initialize WRF 3-D Storage for CLUBB variables.
            DO k = kts, kte, 1

               wpthlp_sgs(i,k,j)            = 0.0
               wprtp_sgs(i,k,j)             = 0.0
               wp2_sgs(i,k,j)               = w_tol_sqd
               rtp2_sgs(i,k,j)              = rt_tol**2
               thlp2_sgs(i,k,j)             = thl_tol**2
               rtpthlp_sgs(i,k,j)           = 0.0
               up2_sgs(i,k,j)               = w_tol_sqd
               vp2_sgs(i,k,j)               = w_tol_sqd
               upwp_sgs(i,k,j)              = 0.0
               vpwp_sgs(i,k,j)              = 0.0
               wpthvp_sgs(i,k,j)            = 0.0
               rtpthvp_sgs(i,k,j)           = 0.0
               thlpthvp_sgs(i,k,j)          = 0.0
               wprcp_sgs(i,k,j)             = 0.0
               wpthp_sgs(i,k,j)             = 0.0

               wp3_sgs(i,k,j)               = 0.0
               wp2thvp_sgs(i,k,j)           = 0.0
               rcm_CLUBB(i,k,j)             = 0.0
               cf_CLUBB(i,k,j)              = 0.0
               rcm_lyr_CLUBB(i,k,j)         = 0.0
               cldcov_CLUBB(i,k,j)          = 0.0

               pdf_params_CLUBB(i,k,j,:)    = 0.0
               pdf_params_zm_CLUBB(i,k,j,:) = 0.0

            ENDDO


            ! ####### CLUBB Grid Heights For Column (i,j) #######

            ! Setup momentum-level grid heights.  This is a one-dimensional
            ! array used for CLUBB based on the altitudes in a given column from
            ! the three-dimensional array used by WRF.
            DO k = k_start, kte, 1
               momentum_heights(k) = ( phb(i,k,j) + ph_2(i,k,j) ) / g
            ENDDO

            ! Setup thermodynamic-level grid heights.  This is also a
            ! one-dimensional array used for CLUBB based on the altitudes in a
            ! given column from the three-dimensional array used by WRF.  WRF's
            ! thermodynamic levels are always located exactly halfway between
            ! the momentum levels that encase them.  The indices of WRF's
            ! thermodynamics levels and CLUBB's thermodynamic levels differ by
            ! one due to the fact that CLUBB's first thermodynamic level is
            ! below the model surface (see above diagram).
            DO k = k_start+1, kte, 1
               thermodynamic_heights(k)  &
                  = (1.0/2.0) * ( momentum_heights(k) + momentum_heights(k-1) )
            ENDDO
            ! The first thermodynamic level (which is below the model surface)
            ! is set to be the same distance below the first momentum level as
            ! the second thermodynamic level is above the first momentum level.
            thermodynamic_heights(k_start)  &
               = ( momentum_heights(k_start) &
                   - thermodynamic_heights(k_start+1) ) &
                 + momentum_heights(k_start)

            ! Set up CLUBB grid heights for column (i,j).
            ! Calling subroutine setup_grid_heights needs to be called once for
            ! every column.  Doing so will enter the correct grid heights
            ! without reallocating any variables.
            call setup_grid_heights_api &
                       ( l_implemented, grid_type,  & 
                         deltaz, zm_init, momentum_heights,  & 
                         thermodynamic_heights )

            ! CLUBB grid height variables (type gr) and grid operators (such as
            ! zm2zt or zt2zm) can now be used for column (i,j).


            ! ####### TKE Initialization #######

            ! Initial subgrid TKE profiles for all cases that are setup in WRF
            ! are located in WRF's module_initialize_quarter_ss.F.  There, the
            ! initial value of subgrid TKE is set as tke_2.  WRF's TKE is
            ! located on thermodynamic levels, while CLUBB's TKE is located on
            ! momentum levels.
            ! Start by using the value of TKE from WRF's thermodynamic levels
            ! for CLUBB's equivalent thermodynamic levels.
            DO k = kts+1, kte, 1
               tke_CLUBB_zt(k) = tke_2(i,k-1,j)
            ENDDO
            ! Find the value of subgrid TKE for CLUBB at the first thermodynamic
            ! level, which is below the model surface, from a linear extension
            ! of the values of TKE at the first two above-ground levels.
            tke_CLUBB_zt(kts)  &
               = LIN_EXT( tke_CLUBB_zt(kts+2), tke_CLUBB_zt(kts+1),  &
                          gr%zt(kts+2), gr%zt(kts+1), gr%zt(kts) )

            ! CLUBB's value of TKE on its own thermodynamic levels now needs to
            ! be interpolated to its momentum levels.
            em = MAX( zt2zm_api( tke_CLUBB_zt ), em_min )

            !!! Initialize w'^2, u'^2, and v'^2 based on initial subgrid TKE !!!

            IF ( l_tke_aniso ) THEN

               ! TKE:  em = (1/2) * ( w'^2 + u'^2 + v'^2 )
               ! Evenly divide TKE into its component
               ! contributions (w'^2, u'^2, and v'^2).

               DO k = kts, kte, 1
                  wp2_sgs(i,k,j) = (2.0/3.0) * em(k)
                  up2_sgs(i,k,j) = (2.0/3.0) * em(k)
                  vp2_sgs(i,k,j) = (2.0/3.0) * em(k)
               ENDDO

            ELSE

               ! TKE:  em = (3/2) * w'^2

               DO k = kts, kte, 1
                  wp2_sgs(i,k,j) = (2.0/3.0) * em(k)
               ENDDO

            ENDIF

         ENDIF  ! l_initialize_CLUBB_vars

      ENDIF ! restart flag

#ifdef CLUBBSTATS
      ! Added for CLUBB statistics
      IF ( l_keep_CLUBB_stats .AND.  &
           config_flags%grid_id == sample_domain .AND.  &
           i == i_sample_col .AND. j == j_sample_col .AND.  &
           l_1st_call_to_CLUBB_stats_init ) THEN

         l_stats = .true.

         ! Set some of the required variables.  This may be done differently later.
         iunit = 50                    ! CLUBB: clubb_driver.F90; sub. run_clubb
         fname_prefix = "clubb"        ! CLUBB: model.in file; &stats_settings
         fdir = "./"                   ! CLUBB: clubb_driver.F90; sub. run_clubb
         stats_fmt = "grads"           ! CLUBB: model.in file; &stats_settings
         runfile = "../clubb_input/stats/standard_stats.in" 
                                       ! CLUBB: clubb_standalone.F90; main
                                       !    stats_init uses the stats namelist,
                                       !    &statsnl

         ! Sample stats every model PBL time step.
         stats_tsamp = DT*STEPBL
         ! Write stats.
         ! Write stats every model PBL time step if the PBL time step is at
         ! least one minute in duration.  If the PBL time step duration is less
         ! than one minute, write stats every minute (although stats will still
         ! be sampled every PBL time step).  However, if the PBL time step is
         ! less than one minute in duration, but the PBL time step duration does
         ! not divide evenly into 60 seconds, then simply record stats every
         ! PBL time step.  In this scenario, the times listed by a GrADS output
         ! file will not match the actual model time.
         IF ( DT*STEPBL >= 60.0 ) THEN
            stats_tout = DT*STEPBL
            IF ( MOD( DT*STEPBL, 60.0 ) /= 0.0 ) THEN
               WRITE(fstdout,*) "CLUBB stats output message:  "
               WRITE(fstdout,*) "CLUBB will sample and record statistics "     &
                                // "every PBL time step.  However, in this "   &
                                // "scenario, the PBL time step duration "     &
                                // "does not divide evenly into a number of "  &
                                // "minutes.  Therefore, the times listed "    &
                                // "the GrADS output file will not match "     &
                                // "the actual model time."
               WRITE(fstdout,*) "CLUBB stats sampled and "  &
                                // "recorded (seconds) = ", stats_tout
            ENDIF
         ELSE
            IF ( MOD( 60.0, DT*STEPBL ) == 0.0 ) THEN
               stats_tout = 60.0
            ELSE
               stats_tout = DT*STEPBL
               WRITE(fstdout,*) "CLUBB stats output message:  "
               WRITE(fstdout,*) "CLUBB samples statistics every PBL "          &
                                // "timestep.  If the PBL time step is at "    &
                                // "least one minute in duration, CLUBB "      &
                                // "will record those stats every PBL time "   &
                                // "step.  If the PBL time step duration is "  &
                                // "less than 60 seconds, CLUBB will record "  &
                                // "those stats every 60 seconds (although "   &
                                // "it will still sample every PBL time "      &
                                // "step).  However, in this case, where "     &
                                // "the PBL time step is less than 60 "        &
                                // "seconds, but does not divide evenly "      &
                                // "into 60 seconds, CLUBB will revert to "    &
                                // "recording statistics every PBL time "      &
                                // "step.  In this scenario, the times "       &
                                // "listed by a GrADS output file will not "   &
                                // "match the actual model time."
               WRITE(fstdout,*) "PBL time step duration (seconds); "  &
                                // "CLUBB stats sampled = ", stats_tsamp
               WRITE(fstdout,*) "CLUBB stats recorded (seconds) = ", stats_tout
            ENDIF
         ENDIF

         ! In CLUBB, total_atmos_dim is a variable that is set to the number of
         ! total grid levels over the extended atmosphere, and is typically used
         ! in conjunction with a radiation scheme.  This formula is:
         !  total_atmos_dim = 
         !     grid_size + lin_int_buffer_size + extend_atmos_range_size.
         ! This is unnecessary for WRF-CLUBB.
         total_atmos_dim = gr%nz

         ! The variables complete_alt (for thermodynamic levels) and
         ! complete_momentum (for momentum levels) are used along the same lines
         ! as total_atmos_dim, and are unnecessary for WRF-CLUBB.
         complete_alt       = gr%zt
         complete_momentum = gr%zm

         year  = config_flags%start_year
         month = config_flags%start_month
         day   = config_flags%start_day

         rlat = xlat(i,j)
         rlon = xlong(i,j)

         ! Print information on the WRF domain and column used for CLUBB
         ! statistics here, rather than in the above code where they are set, so
         ! that these messages print once ONCE.

         ! Print information about the selected WRF domain used for the CLUBB
         ! statistical column.
         IF ( l_sel_stats_dom_valid ) THEN
            ! The user-selected WRF domain for the CLUBB statistical column
            ! (stored in set_sample_domain) matches a valid WRF domain that uses
            ! CLUBB as the PBL scheme.  Thus, the user-selected domain will be
            ! used for the CLUBB statistical column.
            ! Print the index of the user-selected domain.
            WRITE(fstdout,*) "User-selected WRF domain for the CLUBB "  &
                             // "statistical column is valid."
            WRITE(fstdout,*) "WRF domain = ", sample_domain
         ELSE
            ! The user-selected WRF domain is invalid.  Automatically select a
            ! WRF domain for the CLUBB statistical column.  The
            ! automatically-selected domain will be "the most-parental WRF
            ! domain" that uses CLUBB as the PBL scheme.  This is found in
            ! avail_grid_id(1).
            ! Print the index of the automatically-selected domain.
            WRITE(fstdout,*) "A WRF domain is being automatically selected "  &
                             // "for the CLUBB statistical grid column.  "    &
                             // "The user-selected domain is invalid.  The "  &
                             // "automatically-selected domain will be "      &
                             // "'the most-parental WRF domain' that uses "   &
                             // "CLUBB as the PBL scheme."
            WRITE(fstdout,*) "WRF domain = ", sample_domain
         ENDIF

         ! Print information about the selected WRF column used for the CLUBB
         ! statistical grid column.
         IF (       ( set_i_sample_col >= ids   )  &
              .AND. ( set_i_sample_col <= ide-1 )  &
              .AND. ( set_j_sample_col >= jds   )  &
              .AND. ( set_j_sample_col <= jde-1 ) ) THEN
            ! Use the column that was user-declared.
            ! To setup CLUBB stats to automatically use a central column, set
            ! variables set_i_sample_col and set_j_sample_col to 0.
            ! Print the indices of the user-selected grid column.
            WRITE(fstdout,*) "User-selected CLUBB statistical column "  &
                             // "(set_i_sample_col,set_j_sample_col):  "
            WRITE(fstdout,*) "i_sample_col = ", i_sample_col
            WRITE(fstdout,*) "j_sample_col = ", j_sample_col
         ELSE
            ! A column isn't declared or is outside of the model domain.  CLUBB
            ! will keep its stats in the central column, or in a column that
            ! borders the center of the selected model domain.
            ! Print a message to inform the user that a column is being selected
            ! automatically, rather than a column that is user-defined by
            ! set_i_sample_col and set_j_sample_col.
            WRITE(fstdout,*) "A column for CLUBB stats is being "  &
                             // "automatically selected.  This column will "   &
                             // "be the central column or a border-central "   &
                             // "column.  The column that was user-selected "  &
                             // "for CLUBB statistics (by set_i_sample_col "   &
                             // "and set_j_sample_col) is outside the i "      &
                             // "and/or j domain."
            WRITE(fstdout,*) "Selected CLUBB statistical column "  &
                             // "(i_sample_col,j_sample_col):  "
            WRITE(fstdout,*) "i_sample_col = ", i_sample_col
            WRITE(fstdout,*) "j_sample_col = ", j_sample_col
         ENDIF

         ! Print the latitude, longitude values of the selected grid column.
         ! This will only be useful in cases where the latitude and longitude
         ! have been defined in module_initialize.
         WRITE(*,*) "Center of CLUBB statistics grid column: "
         WRITE(*,*) "Latitude: ", xlat(i,j)
         WRITE(*,*) "Longitude: ", xlong(i,j)
         WRITE(*,*) "Edges of CLUBB statistics grid column: "
         WRITE(*,*) "Latitude, south edge: ", xlat_v(i,j)
         WRITE(*,*) "Latitude, north edge: ", xlat_v(i,j+1)
         WRITE(*,*) "Longitude, west edge: ", xlong_u(i,j)
         WRITE(*,*) "Longitude, east edge: ", xlong_u(i+1,j)

         ! Here, time_current is the same as time_initial.  It is the time of
         ! the start of the model expressed in units of seconds since midnight
         ! on the start date.
         time_current = sec_per_hr * real( config_flags%start_hour )  &
                        + sec_per_min * real( config_flags%start_minute )  &
                        + real( config_flags%start_second )

         ! Save the restart time in order to adjust time_initial in subroutine
         ! CLUBB_bl_scheme during a restart run (for restart runs, time_initial 
         ! changes from the original time_initial, but ITIMESTEP does not, which 
         ! previously produced a discrepancy in stats output).
         IF (restart) THEN
            time_restart = time_current
         ELSE
            time_restart = 0.0
         ENDIF

         dtmain = DT*STEPBL

         l_silhs_out = .false.
#ifdef SILHS
         if ( config_flags%lh_microphys_type /= lh_microphys_disabled ) then
           l_silhs_out = .true.
         end if
#endif
         ! Initialize statistics output
         call stats_init_api( iunit, fname_prefix, fdir, l_stats, &
                          stats_fmt, stats_tsamp, stats_tout, trim(runfile), &
                          gr%nz, 1, 1, gr%zt, gr%zm, total_atmos_dim, &
                          complete_alt, total_atmos_dim+1, complete_momentum, &
                          day, month, year, &
                          rlon, rlat, time_current, dtmain, l_silhs_out )

         ! Reset the value of l_1st_call_to_CLUBB_stats_init to .false. so that
         ! CLUBB's subroutine stats_init is not called multiple times.  Some
         ! CLUBB statistical variables are allocated when CLUBB's stats_init is
         ! called.  Thus, it is important to call CLUBB's stats_init only ONCE.
         ! Otherwise, the CLUBB model code will attempt to allocate variables
         ! more than once, and therefore produce an error which stops the
         ! WRF-CLUBB model run.
         l_1st_call_to_CLUBB_stats_init = .false.

      ELSE

         ! Turn off stats for all columns besides the sampling column.
         l_stats = .false.

      ENDIF
      ! CLUBB statistics changes end
#endif /*CLUBBSTATS*/

   ENDDO
ENDDO

! Reset the value of l_first_call_to_CLUBB_bl_init to .false. so that CLUBB's
! subroutine setup_clubb_core is not called multiple times.
l_first_call_to_CLUBB_bl_init = .false.

END SUBROUTINE CLUBB_bl_init

#ifdef SILHS
SUBROUTINE CLUBB_lh_init( config_flags )

  use silhs_api_module, only: &
    l_lh_importance_sampling    ! Perform importance sampling

  use clubb_api_module, only: &
    l_fix_w_chi_eta_correlations, & ! Use a fixed correlation for s and t Mellor

    init_pdf_hydromet_arrays_api, & ! Procedure(s)
    setup_corr_varnce_array_api,  &

    Ncnp2_on_Ncnm2, &
    hmp2_ip_on_hmm2_ip_slope_type, &
    hmp2_ip_on_hmm2_ip_intrcpt_type

  use module_configure, only : grid_config_rec_type ! Variable

  use module_wrf_error

  IMPLICIT NONE

#ifdef _OPENMP
    INTEGER :: omp_get_thread_num ! Function
#endif

  ! Constant Parameter(s)
  CHARACTER(LEN=*), PARAMETER :: &
    lh_file_path_below = "./clubb_corr_array_below.in", &
    lh_file_path_cloud = "./clubb_corr_array_cloud.in"

  ! Input Variables
  
  !  Structure that contains run-time configuration (namelist) data for domain
  TYPE (grid_config_rec_type), INTENT(IN) :: config_flags

  ! Local Variable(s)
  INTEGER :: iunit

  integer :: &
      hydromet_dim, & ! Number of hydrometeor fields.
      iirr,         & ! Hydrometeor array index for rain water mixing ratio, rr
      iirs,         & ! Hydrometeor array index for snow mixing ratio, rs
      iiri,         & ! Hydrometeor array index for ice mixing ratio, ri
      iirg,         & ! Hydrometeor array index for graupel mixing ratio, rg
      iiNr,         & ! Hydrometeor array index for rain drop concentration, Nr
      iiNs,         & ! Hydrometeor array index for snow concentration, Ns
      iiNi,         & ! Hydrometeor array index for ice concentration, Ni
      iiNg            ! Hydrometeor array index for graupel concentration, Ng

  ! Place holder to represent the horizonal grid spacing for clubb
  real( kind = core_rknd ) :: dummy_dx, dummy_dy

  ! Structure containing sigma^2 / mu^2 ratios for SILHS in CLUBB is linearly 
  ! dependent on the horizontal grid spacing spcified by the host model, 
  ! and these are the slopes and intercepts defining that linear function
  type( hmp2_ip_on_hmm2_ip_slope_type ) :: hmp2_ip_on_hmm2_ip_slope     ! [1/m]
  type( hmp2_ip_on_hmm2_ip_intrcpt_type ) :: hmp2_ip_on_hmm2_ip_intrcpt ! [-]

  ! ---- Begin code ----

  ! Hardwire these configuration flags for now.  In the future they could be a
  ! namelist option
  l_fix_w_chi_eta_correlations = .TRUE. ! This is needed for ice phase microphysics
  l_lh_importance_sampling = .TRUE. ! Sample points will be weighted

  ! Set lh_microphys_type as given in namelist.input
  ! See module_parameters_microphys in CLUBB for options!
  ! Current setup:
  !      lh_microphys_interactive     = 1, & ! Feed the samples into the microphysics and allow feedback
  !      lh_microphys_non_interactive = 2, & ! Feed the samples into the microphysics with no feedback
  !      lh_microphys_disabled        = 3
  lh_microphys: SELECT CASE(config_flags%lh_microphys_type)
    CASE (lh_microphys_interactive)
          lh_microphys_type = lh_microphys_interactive

    CASE (lh_microphys_non_interactive)
          lh_microphys_type = lh_microphys_non_interactive

    CASE (lh_microphys_disabled)
          lh_microphys_type = lh_microphys_disabled

    CASE DEFAULT
          WRITE( wrf_err_message , * ) 'The lh_microphys_type option does not exist: lh_microphys_type = ', config_flags%lh_microphys_type
          CALL wrf_error_fatal ( wrf_err_message )

   END SELECT lh_microphys

  
  ! Note:  the value of hydromet_dim and the value of each hydrometeor index
  !        (positive integer or -1) needs to be generalized for different
  !        microphysics schemes, not just Morrison.  Brian.
  hydromet_dim = 8
  iirr = 1
  iirs = 2
  iiri = 3
  iirg = 4
  iiNr = 5
  iiNs = 6
  iiNi = 7
  iiNg = 8

  ! Setting the slopes to 0 causes the intercepts to be the final ratio value
  hmp2_ip_on_hmm2_ip_slope%rr = 0.0
  hmp2_ip_on_hmm2_ip_slope%Nr = 0.0
  hmp2_ip_on_hmm2_ip_slope%rs = 0.0
  hmp2_ip_on_hmm2_ip_slope%Ns = 0.0
  hmp2_ip_on_hmm2_ip_slope%ri = 0.0
  hmp2_ip_on_hmm2_ip_slope%Ni = 0.0
  hmp2_ip_on_hmm2_ip_slope%rg = 0.0
  hmp2_ip_on_hmm2_ip_slope%Ng = 0.0

  ! Variances / Corrlations here are those used with the RICO case
  hmp2_ip_on_hmm2_ip_intrcpt%rr = 1.25
  hmp2_ip_on_hmm2_ip_intrcpt%Nr = 1.25
  hmp2_ip_on_hmm2_ip_intrcpt%rs = 1.0
  hmp2_ip_on_hmm2_ip_intrcpt%Ns = 1.0
  hmp2_ip_on_hmm2_ip_intrcpt%ri = 1.0
  hmp2_ip_on_hmm2_ip_intrcpt%Ni = 1.0
  hmp2_ip_on_hmm2_ip_intrcpt%rg = 1.0
  hmp2_ip_on_hmm2_ip_intrcpt%Ng = 1.0

  Ncnp2_on_Ncnm2 = 0.003

  ! This is a place holder for the horizontal grid spacing, it does not
  ! affect the calculation of the ratios since the slope is 0
  dummy_dx = 1._core_rknd
  dummy_dy = 1._core_rknd

  ! Set up pdf indices, hydromet indicies, hydromet arrays, and hydromet variance ratios
  call init_pdf_hydromet_arrays_api( dummy_dx, dummy_dy, hydromet_dim, &
                                     iirr, iiri, iirs, iirg, & 
                                     iiNr, iiNi, iiNs, iiNg, &
                                     hmp2_ip_on_hmm2_ip_slope, &
                                     hmp2_ip_on_hmm2_ip_intrcpt )

#ifdef _OPENMP 
   iunit = omp_get_thread_num + 10
#else
   iunit = 10
#endif

  ! Setup the arrays and indexes containing the correlations, etc.
  call setup_corr_varnce_array_api( lh_file_path_cloud, lh_file_path_below, iunit )

  RETURN
END SUBROUTINE CLUBB_lh_init
!===============================================================================
#endif /*SILHS*/

END MODULE module_bl_clubb
#endif /*CLUBB*/
