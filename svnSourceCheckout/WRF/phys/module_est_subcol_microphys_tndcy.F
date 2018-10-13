#ifdef SILHS
!-------------------------------------------------------------------------------
! $Id: module_est_subcol_microphys_tndcy.F 932 2017-12-30 00:43:42Z huebler@uwm.edu $
!-------------------------------------------------------------------------------
MODULE module_est_subcol_microphys_tndcy

  IMPLICIT NONE

  PUBLIC :: est_subcol_microphys_tndcy

  PRIVATE :: setup_hydromet_array

  CONTAINS

  !-----------------------------------------------------------------------------
  SUBROUTINE est_subcol_microphys_tndcy &
    ( config_flags, grid, dtm, t8w, rho, dz8w, p8w, p_phy, pi_phy, &
      specified_bdy, channel_bdy, th_mean, &
      qv_mean, qc_mean, qr_mean, qi_mean, qs_mean, qg_mean, & 
      qndrop_mean, qni_mean, qns_mean, qnr_mean, qng_mean, &
      ids,ide, jds,jde, kds,kde, &
      ims,ime, jms,jme, kms,kme, &
      ips,ipe, jps,jpe, kps,kpe, &
      k_start, k_end, &
      f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni,f_qt, &
      f_qns,f_qnr,f_qng,f_qnn,f_qnc, &
      qvolg, f_qvolg, &
      exner, rho_ds_zt, rho_ds_zm)

    ! Description:
    !   This module estimates the total tendencues from a microphysics scheme by
    !   feeding latin hypercube samples from the CLUBB PDF into the microphyics
    !   scheme and then computing an average of the individual tendencies.
    ! References:
    !   None
    !-----------------------------------------------------------------------------
    USE module_domain, ONLY : domain ! Variable

    USE module_configure, ONLY : grid_config_rec_type ! Variable

    USE module_microphysics_driver, ONLY : microphysics_driver ! Procedure

    USE clubb_api_module, ONLY : &
        hydromet_dim, &

        rt_tol, & ! Variable(s)

        time_precision, &
        core_rknd, &

        fill_holes_driver_api, &

        l_stats_samp, &

        iirr, &
        iirs, &
        iiri,  &
        iirg, &
        iiNr, &
        iiNi, &
        iiNs, &
        iiNg

#ifdef CLUBBSTATS
    USE module_bl_clubb, only: &
        i_sample_col,  & ! Variable(s)
        j_sample_col
#endif /*CLUBBSTATS*/

    IMPLICIT NONE

    ! External
    external :: wrf_debug ! Subroutine

    ! Constant parameters
    integer, parameter :: &
      lh_microphys_interactive = 1  ! Interactive SILHS

    ! Input Variable(s)
    TYPE(domain), INTENT(IN OUT) :: grid

    !  Structure that contains run-time configuration (namelist) data for domain
    TYPE (grid_config_rec_type), INTENT(IN) :: config_flags

    REAL, INTENT(IN) :: dtm ! Timestep [s]

    REAL, INTENT(IN), DIMENSION(ims:ime,kms:kme,jms:jme) :: &
      rho, & ! Air density       [kg/m^3]
      dz8w, &
      p8w, &
      pi_phy, & ! Exner function   [-]
      p_phy,  & ! Pressure [hPa]
      t8w    ! Temperature at layer interfaces?

    LOGICAL, INTENT(IN) :: specified_bdy, channel_bdy

    REAL, INTENT(IN OUT), DIMENSION(ims:ime,kms:kme,jms:jme) :: &
      th_mean, &     ! Gridbox mean value of theta                       [K]
      qv_mean, &     ! Gridbox mean value of vapor water mixing ratio    [kg/kg]
      qc_mean, &     ! Gridbox mean value of cloud water mixing ratio    [kg/kg]
      qr_mean, &     ! Gridbox mean value of snow mixing ratio           [kg/kg]
      qi_mean, &     ! Gridbox mean value of snow mixing ratio           [kg/kg]
      qs_mean, &     ! Gridbox mean value of snow mixing ratio           [kg/kg]
      qg_mean, &     ! Gridbox mean value for graupel water mixing ratio [kg/kg]
      qndrop_mean, & ! Gridbox mean value of cloud droplet num. conc.    [#/kg]
      qni_mean, &    ! Gridbox mean value of ice number conc.            [#/kg]
      qns_mean, &    ! Gridbox mean value of snow number conc.           [#/kg]
      qnr_mean, &    ! Gridbox mean value of rain number conc.           [#/kg]
      qng_mean       ! Gridbox mean value for graupel number conc.       [#/kg]

    INTEGER, INTENT(IN) :: ids,ide, jds,jde, kds,kde, &
      ims,ime, jms,jme, kms,kme, &
      ips,ipe, jps,jpe, kps,kpe, &
      k_start, k_end

    LOGICAL, INTENT(IN) :: f_qv,f_qc,f_qr,f_qi,f_qs,f_qg,f_qndrop,f_qni,f_qt, &
                           f_qns,f_qnr,f_qng,f_qnn,f_qnc, f_qvolg

    real( kind = core_rknd ), dimension(ims:ime,kms:kme,jms:jme), intent(in) :: &
      rho_ds_zm, & ! Dry, static density on momentum levels   [kg/m^3]
      rho_ds_zt    ! Dry, static density on thermo. levels    [kg/m^3]

    real( kind = core_rknd ), dimension(ims:ime,kms:kme,jms:jme), intent(in) :: &
      exner  ! Exner function                                       [-]

    REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: &
      qvolg 

    ! Local Variables
    REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: &
      th_samp,       & ! Theta sample point                  [K]
      w_samp,        & ! Vertical velocity sample point      [m/s]
      qv_samp,       & ! Vapor water mixing ratio sample     [kg/kg]
      qc_samp,       & ! Cloud water mixing ratio sample     [kg/kg]
      qr_samp,       & ! Rain droplet number concentration   [kg/kg]
      qnr_samp,      & ! Rain number concentration           [#/kg]
      qndrop_samp,   & ! Cloud droplet number concentration  [#/kg]
      qs_samp,       & ! Snow mixing ratio                   [kg/kg]
      qns_samp,      & ! Snow number concentration           [#/kg]
      qi_samp,       & ! Cloud ice mixing ratio              [kg/kg]
      qni_samp,      & ! Cloud ice number concentration      [#/kg]
      qg_samp,       & ! Graupel water mixing ratio          [kg/kg]
      qng_samp         ! Graupel number concentration        [#/kg]

    REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: &
      qg_prior,       & ! Graupel water mixing ratio          [kg/kg]
      qng_prior         ! Graupel number concentration        [#/kg]

    REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: &
      SR, & ! One timestep mass ratio of snow to total precip
      nsource, & ! ???
      t8w_inout, &
      cldfra_inout, &
      exch_h_inout, &
      z_inout

    REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: &
      cldfra_old_mp, & ! ???
      wsedl3d          ! ???

    ! Note: it appears that in the microphys_driver these are declared with an
    ! incorrect set of dimensions.  To make this code compile properly with
    ! array bound checking I declared with the dimemsion 1 here. 
    ! -dschanen 8 Aug 2011
    REAL, DIMENSION(1:1,1:1,1:1) :: &
      f_ice_phy_inout, &
      f_rain_phy_inout, &
      f_rimef_phy_inout

    REAL, DIMENSION(ims:ime,jms:jme) :: &
      RAINNC, & ! ???
      RAINNCV, &
      SNOWNC,  &
      SNOWNCV, &
      GRAUPELNC, &
      GRAUPELNCV

    !!! Needed For CLUBB's hole filling algorithms
    REAL, DIMENSION(:,:), ALLOCATABLE :: &
      hydromet_hf ! hydromet array to be used in fill_holes_driver
    !!! End needed for hole filling

    INTEGER, DIMENSION(ims:ime,jms:jme) :: &
      LOWLYR ! ???

    REAL :: naer ! Number of aerosols

    INTEGER :: &
      icall, & ! Loop iterator for lh_microphys_calls
      i, j, k ! Loop iterator for i, j, k

    ! Switch for the new HF algorithm (see clubb:ticket:655)
    LOGICAL :: l_fill_holes_hm = .true.

    
    INTEGER :: its, ite, jts, jte, sz
    LOGICAL :: channel


    ! ---- Begin Code ----

    ! Initialize tendencies and local variables

    grid%th_lh_mc = 0.
    grid%qv_lh_mc = 0.
    grid%qc_lh_mc = 0.
    grid%qr_lh_mc = 0.
    grid%qi_lh_mc = 0.
    grid%qs_lh_mc = 0.
    grid%qg_lh_mc = 0.
    grid%qndrop_lh_mc = 0.
    grid%qnr_lh_mc = 0.
    grid%qni_lh_mc = 0.
    grid%qns_lh_mc = 0.
    grid%qng_lh_mc = 0.

    DO icall = 1, config_flags%lh_microphys_calls

      w_samp  = grid%w_lh(ims:ime,kms:kme,jms:jme,icall)
      th_samp = grid%th_lh(ims:ime,kms:kme,jms:jme,icall)
      qv_samp = grid%qv_lh(ims:ime,kms:kme,jms:jme,icall)
      qc_samp = grid%qc_lh(ims:ime,kms:kme,jms:jme,icall)
      qr_samp = grid%qr_lh(ims:ime,kms:kme,jms:jme,icall)
      qnr_samp = grid%qnr_lh(ims:ime,kms:kme,jms:jme,icall)
      qndrop_samp = grid%qndrop_lh(ims:ime,kms:kme,jms:jme,icall)
      qs_samp = grid%qs_lh(ims:ime,kms:kme,jms:jme,icall)
      qns_samp = grid%qns_lh(ims:ime,kms:kme,jms:jme,icall)
      qi_samp = grid%qi_lh(ims:ime,kms:kme,jms:jme,icall)
      qni_samp = grid%qni_lh(ims:ime,kms:kme,jms:jme,icall)
      qg_samp = grid%qg_lh(ims:ime,kms:kme,jms:jme,icall)
      qng_samp = grid%qng_lh(ims:ime,kms:kme,jms:jme,icall)

      ! Set variables declared as in/out.

      ! Most of these quantities don't appear to need to be in/out, but this
      ! will allow the code to compile without an error. -dschanen 8 Aug 2011

      LOWLYR = grid%lowlyr
      RAINNC = grid%rainnc
      RAINNCV = grid%rainncv

      SNOWNC = grid%snownc
      SNOWNCV = grid%snowncv

      GRAUPELNC = grid%graupelnc
      GRAUPELNCV = grid%graupelncv

      t8w_inout = t8w
      cldfra_inout = grid%cldfra
      exch_h_inout = grid%exch_h

      ! See note above about these dimensions -dschanen 8 Aug 2011
      f_ice_phy_inout = grid%f_ice_phy(1:1,1:1,1:1)
      f_rain_phy_inout = grid%f_rain_phy(1:1,1:1,1:1)
      f_rimef_phy_inout = grid%f_rimef_phy(1:1,1:1,1:1)
      z_inout = grid%z

      ! These are now in/out variables -dschanen 4 Aug 2014
      wsedl3d = grid%wsedl3d
      cldfra_old_mp = grid%cldfra_old_mp

      CALL wrf_debug ( 100, 'est_subcol_microphys_tndcy: calling microphys_driver' )

      CALL microphysics_driver(                                            &
        &         DT=dtm             ,DX=grid%dx              ,DY=grid%dy   &
        &        ,DZ8W=dz8w          ,F_ICE_PHY=f_ice_phy_inout              &
        &        ,ITIMESTEP=grid%itimestep                    ,LOWLYR=lowlyr  &
        &        ,P8W=p8w            ,P=p_phy            ,PI_PHY=pi_phy     &
        &        ,RHO=rho            ,SPEC_ZONE=grid%spec_zone              &
        &        ,SR=sr              ,TH=th_samp                            &
        &        ,WARM_RAIN=grid%warm_rain                                  &
        &        ,T8W=t8w_inout                                             &
        &        ,CLDFRA=cldfra_inout, EXCH_H=exch_h_inout                  &
        &        ,NSOURCE=nsource                                           &
#ifdef NOTNOT /*WRF_CHEM*/
        &        ,CLDFRA_OLD=grid%cldfra_old                                &
        &        ,PRECR=grid%precr, PRECI=grid%preci, PRECS=grid%precs, PRECG=grid%precg &
        &        ,CHEM_OPT=config_flags%chem_opt, PROGN=config_flags%progn  &
        ! Variables required for CAMMGMP Scheme when run with WRF_CHEM
        &        ,CHEM=chem                                                 &
        &        ,QME3D=grid%qme3d,PRAIN3D=grid%prain3d                     &
        &        ,NEVAPR3D=grid%nevapr3d                                    &
        &        ,RATE1ORD_CW2PR_ST3D=grid%rate1ord_cw2pr_st3d              &
        &        ,DGNUM4D=grid%dgnum4d,DGNUMWET4D=grid%dgnumwet4d           &
#endif
        &        ,XLAND=grid%xland, SNOWH=grid%SNOW                         &
        &        ,SPECIFIED=specified_bdy, CHANNEL_SWITCH=channel_bdy       &
        &        ,F_RAIN_PHY=f_rain_phy_inout                                &
        &        ,F_RIMEF_PHY=f_rimef_phy_inout                              &
        &        ,MP_PHYSICS=config_flags%mp_physics                        &
        &        ,ID=grid%id                                                &
        &        ,IDS=ids,IDE=ide, JDS=jds,JDE=jde, KDS=kds,KDE=kde         &
        &        ,IMS=ims,IME=ime, JMS=jms,JME=jme, KMS=kms,KME=kme         &
#ifdef RUN_ON_GPU
        &        ,IPS=ips,IPE=ipe, JPS=jps,JPE=jpe, KPS=kps,KPE=kpe         &
#endif
        &        ,I_START=grid%i_start,I_END=min(grid%i_end, ide-1)         &
        &        ,J_START=grid%j_start,J_END=min(grid%j_end, jde-1)         &
        &        ,KTS=k_start, KTE=min(k_end,kde-1)                         &
        &        ,NUM_TILES=grid%num_tiles                                  &
        &        ,NAER=naer                                                 &
!======================
        ! Variables required for CAMMGMP Scheme
        &        ,DLF=grid%dlf,DLF2=grid%dlf2,T_PHY=grid%t_phy,P_HYD=grid%p_hyd  &
        &        ,P8W_HYD=grid%p_hyd_w,TKE_PBL=grid%tke_pbl                 &
        &        ,Z_AT_W=grid%z_at_w,QFX=grid%qfx,RLIQ=grid%rliq            &
        &        ,TURBTYPE3D=grid%turbtype3d,SMAW3D=grid%smaw3d             &
        &        ,WSEDL3D=wsedl3d,CLDFRA_OLD_MP=cldfra_old_mp               &
        &        ,CLDFRA_MP=grid%cldfra_mp,CLDFRA_MP_ALL=grid%cldfra_mp_ALL &
        &        ,CLDFRAI=grid%cldfrai                                      &
        &        ,CLDFRAL=grid%cldfral,CLDFRA_CONV=grid%CLDFRA_CONV         &
        &        ,ALT=grid%alt                                              &
        &        ,ACCUM_MODE=config_flags%accum_mode                        &
        &        ,AITKEN_MODE=config_flags%aitken_mode                      &
        &        ,COARSE_MODE=config_flags%coarse_mode                      &
        &        ,ICWMRSH3D=grid%icwmrsh,ICWMRDP3D=grid%icwmrdp3d           &
        &        ,SHFRC3D=grid%shfrc3d,CMFMC3D=grid%cmfmc                   &
        &        ,CMFMC2_3D=grid%cmfmc2,CONFIG_FLAGS=config_flags           &
        &        ,FNM=grid%fnm,FNP=grid%fnp,RH_OLD_MP=grid%rh_old_mp        &
        &        ,LCD_OLD_MP=grid%lcd_old_mp                                &
!======================
#ifdef NOTNOT /*ESRL*/
        &        , RAINNC=grid%rainnc, RAINNCV=grid%rainncv,opdt=grid%opdt,rainrtk=grid%rainrtk        &
#else
        &        , RAINNC=rainnc, RAINNCV=rainncv                 &
#endif /*ESRL*/
        &        , SNOWNC=snownc, SNOWNCV=snowncv                 &
        &        , GRAUPELNC=graupelnc, GRAUPELNCV=graupelncv     &
        &        , W=w_samp, Z=z_inout, HT=grid%ht                         &
        &        , QV_CURR=qv_samp, F_QV=F_QV          &
        &        , QC_CURR=qc_samp, F_QC=F_QC          &
        &        , QR_CURR=qr_samp, F_QR=F_QR               &
        &        , QI_CURR=qi_samp, F_QI=F_QI               &
        &        , QS_CURR=qs_samp, F_QS=F_QS               &
        &        , QG_CURR=qg_samp, F_QG=F_QG               &
        &        , QNDROP_CURR=qndrop_samp, F_QNDROP=F_QNDROP &
#ifdef NOTNOT /*WRF_CHEM*/
        &        , RAINPROD=grid%rainprod, EVAPPROD=grid%evapprod           &
        &        , QV_B4MP=grid%qv_b4mp,QC_B4MP=grid%qc_b4mp                &
        &        , QI_B4MP=grid%qi_b4mp, QS_B4MP=grid%qs_b4mp               &
#endif /*WRF_CHEM*/
        &        , QNI_CURR=qni_samp, F_QNI=F_QNI          &
        &        , QNS_CURR=qns_samp, F_QNS=F_QNS          &  
        &        , QNR_CURR=qnr_samp, F_QNR=F_QNR          &  
        &        , QNG_CURR=qng_samp, F_QNG=F_QNG          &  
#ifdef NOTNOT /*ESRL*/
        &        , ETA0_CURR=moist(ims,kms,jms,P_ETA0), F_ETA0=F_ETA0      &  ! for Feingold2m
        &        , SSR_CURR=scalar(ims,kms,jms,P_SSR), F_SSR=F_SSR         &
#endif /*ESRL*/
        &        , QVOLG_CURR=qvolg, F_QVOLG=F_QVOLG                        & ! for nssl_2mom
        &        , qrcuten=grid%rqrcuten, qscuten=grid%rqscuten             &  
        &        , qicuten=grid%rqicuten,mu=grid%mut                        &  
        &        , HAIL=config_flags%gsfcgce_hail                           & ! for gsfcgce
        &        , ICE2=config_flags%gsfcgce_2ice                           & ! for gsfcgce
        &        , re_cloud=grid%re_cloud, re_ice=grid%re_ice, re_snow=grid%re_snow & ! G. Thompson
        &        , has_reqc=grid%has_reqc, has_reqi=grid%has_reqi, has_reqs=grid%has_reqs & ! G. Thompson
        )
!     &        , QT_CURR=scalar(ims,kms,jms,P_QT), F_QT=F_QT              &
!     &        , QNN_CURR=scalar(ims,kms,jms,P_QNN), F_QNN=F_QNN          &
!     &        , QNC_CURR=scalar(ims,kms,jms,P_QNC), F_QNC=F_QNC          &

      ! Adapted from module_pbl_driver.F line 1187 wrf:r605
      ! and from from  module_bl_clubb.F line 840 wrf:r605        
      ! This is without parallelization to make things not unnecessay complicated
      ! at the beginning. I use maxval and minval here because I do not know for sure
      ! whether the core with the lowest rank hast the smallest indices and the
      ! core with the largest rank hast the largest indices.
      its = MINVAL(grid%i_start)
      ite = MIN(ide-1, MAXVAL(grid%i_end))
      jts = MINVAL(grid%j_start)
      jte = MIN(jde-1, MAXVAL(grid%j_end))

      ! Add the new tendency to the sum
      DO i = its, ite
        DO j = jts, jte
!#############Adjust the end of the loop based on the routine of vendor WRF
!#############to prevent the seg fault in restarting
!#############C.S Feb 25th
!         DO k = kds, kde
          DO k = k_start, MIN(k_end,kde-1)
!#############C.S Feb 25th
            grid%th_lh_mc(i,k,j) = grid%th_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( th_samp(i,k,j) - grid%th_lh(i,k,j,icall) ) / dtm
            grid%qv_lh_mc(i,k,j) = grid%qv_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qv_samp(i,k,j) - grid%qv_lh(i,k,j,icall) ) / dtm
            grid%qc_lh_mc(i,k,j) = grid%qc_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qc_samp(i,k,j) - grid%qc_lh(i,k,j,icall) ) / dtm
            grid%qr_lh_mc(i,k,j) = grid%qr_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qr_samp(i,k,j) - grid%qr_lh(i,k,j,icall) ) / dtm
            grid%qi_lh_mc(i,k,j) = grid%qi_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qi_samp(i,k,j) - grid%qi_lh(i,k,j,icall) ) / dtm
            grid%qs_lh_mc(i,k,j) = grid%qs_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qs_samp(i,k,j) - grid%qs_lh(i,k,j,icall) ) / dtm
!            grid%qg_lh_mc(i,k,j) = grid%qg_lh_mc(i,k,j) &
!              + grid%weights_lh(i,k,j,icall) * ( qg_samp(i,k,j) - qg_prior(i,k,j) ) / dtm
            grid%qg_lh_mc(i,k,j) = grid%qg_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qg_samp(i,k,j) - grid%qg_lh(i,k,j,icall) ) / dtm

            grid%qndrop_lh_mc(i,k,j) = grid%qndrop_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qndrop_samp(i,k,j) - grid%qndrop_lh(i,k,j,icall) ) / dtm
            grid%qnr_lh_mc(i,k,j) = grid%qnr_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qnr_samp(i,k,j) - grid%qnr_lh(i,k,j,icall) ) / dtm
            grid%qni_lh_mc(i,k,j) = grid%qni_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qni_samp(i,k,j) - grid%qni_lh(i,k,j,icall) ) / dtm
            grid%qns_lh_mc(i,k,j) = grid%qns_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qns_samp(i,k,j) - grid%qns_lh(i,k,j,icall) ) / dtm
!            grid%qng_lh_mc(i,k,j) = grid%qng_lh_mc(i,k,j) &
!              + grid%weights_lh(i,k,j,icall) * ( qng_samp(i,k,j) - qng_prior(i,k,j) ) / dtm
            grid%qng_lh_mc(i,k,j) = grid%qng_lh_mc(i,k,j) &
              + grid%weights_lh(i,k,j,icall) * ( qng_samp(i,k,j) - grid%qng_lh(i,k,j,icall) ) / dtm
          END DO ! k = k_start, k_end, 1
        END DO ! j = j_start, j_end, 1
      END DO ! i = i_start, i_end, 1

    END DO ! 1 ... config_flags%lh_microphys_calls

    ! Compute the average of all subcolumns
    grid%th_lh_mc = grid%th_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qv_lh_mc = grid%qv_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qc_lh_mc = grid%qc_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qr_lh_mc = grid%qr_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qi_lh_mc = grid%qi_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qs_lh_mc = grid%qs_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qg_lh_mc = grid%qg_lh_mc / real( config_flags%lh_microphys_calls )

    grid%qndrop_lh_mc = grid%qndrop_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qnr_lh_mc = grid%qnr_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qni_lh_mc = grid%qni_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qns_lh_mc = grid%qns_lh_mc / real( config_flags%lh_microphys_calls )
    grid%qng_lh_mc = grid%qng_lh_mc / real( config_flags%lh_microphys_calls )

    ! Update mean fields if the code is running in an interactive mode
    IF ( config_flags%lh_microphys_type == lh_microphys_interactive ) THEN

       DO i = its, ite
          DO j = jts, jte

             ! Update the hydrometeor means
!#############Adjust the end of the loop based on the routine of vendor WRF
!#############to prevent the seg fault in restarting
!#############C.S Feb 25th
!             DO k = kds, kde
              DO k = k_start, MIN(k_end,kde-1)
!#############C.S Feb 25th
                IF ( F_QC ) qc_mean(i,k,j)  = qc_mean(i,k,j)  + dtm * grid%qc_lh_mc(i,k,j)
                IF ( F_QR ) qr_mean(i,k,j)  = qr_mean(i,k,j)  + dtm * grid%qr_lh_mc(i,k,j)
                IF ( F_QI ) qi_mean(i,k,j)  = qi_mean(i,k,j)  + dtm * grid%qi_lh_mc(i,k,j)
                IF ( F_QS ) qs_mean(i,k,j)  = qs_mean(i,k,j)  + dtm * grid%qs_lh_mc(i,k,j)
                IF ( F_QG ) qg_mean(i,k,j)  = qg_mean(i,k,j)  + dtm * grid%qg_lh_mc(i,k,j)
                IF ( F_QNDROP ) qndrop_mean(i,k,j)  = qndrop_mean(i,k,j)  + dtm * grid%qndrop_lh_mc(i,k,j) !!!!! <hf>
!                qndrop_mean(i,k,j)  = qndrop_mean(i,k,j)  + dtm * grid%qndrop_lh_mc(i,k,j)
                IF ( F_QNR ) qnr_mean(i,k,j)  = qnr_mean(i,k,j)  + dtm * grid%qnr_lh_mc(i,k,j)
                IF ( F_QNI ) qni_mean(i,k,j)  = qni_mean(i,k,j)  + dtm * grid%qni_lh_mc(i,k,j)
                IF ( F_QNS ) qns_mean(i,k,j)  = qns_mean(i,k,j)  + dtm * grid%qns_lh_mc(i,k,j)
                IF ( F_QNG ) qng_mean(i,k,j)  = qng_mean(i,k,j)  + dtm * grid%qng_lh_mc(i,k,j)
             ENDDO ! k

             ! Call the hole filling algorithm instead of using blunt clipping
             IF ( config_flags%l_use_hf_driver ) then

                ! Setup the hydromet array for fill_holes_driver
                CALL setup_hydromet_array( kme, qr_mean(i,:,j), qi_mean(i,:,j),  & ! Intent(in)
                                           qs_mean(i,:,j), qg_mean(i,:,j),       & ! Intent(in)
                                           qndrop_mean(i,:,j), qnr_mean(i,:,j),  & ! Intent(in)
                                           qni_mean(i,:,j), qns_mean(i,:,j),     & ! Intent(in)
                                           qng_mean(i,:,j),                      & ! Intent(in)
                                           hydromet_hf )                           ! Intent(out)
#ifdef CLUBBSTATS
                ! Otherwise the statistics might be called unintentionally during a parallel run
                IF ( i == i_sample_col .AND. j == j_sample_col ) THEN
                   l_stats_samp = .true.
                ELSE
                   l_stats_samp = .false.
                ENDIF
#endif /*CLUBBSTATS*/

                ! Apply hole filling
                CALL fill_holes_driver_api( kme, dtm, & ! Intent(in)
                                        hydromet_dim, l_fill_holes_hm, & ! Intent(in)
                                        rho_ds_zm(i,:,j), rho_ds_zt(i,:,j), & ! Intent(in)
                                        exner(i,:,j), & ! Intent(in)
                                        grid%th_lh_mc(i,:,j), & ! Intent(inout)
                                        grid%qv_lh_mc(i,:,j), & ! Intent(inout)
                                        hydromet_hf ) ! Intent(inout)

                ! Assign the filled hydromets to their WRF equivalents
!#############Adjust the end of the loop based on the routine of vendor WRF
!#############to prevent the seg fault in restarting
!#############C.S Feb 25th
!                DO k = kds,kde
                 DO k = k_start, MIN(k_end,kde-1)
!#############C.S Feb 25th
                   th_mean(i,k,j)  = th_mean(i,k,j)  + dtm * grid%th_lh_mc(i,k,j)
                   IF ( F_QV ) qv_mean(i,k,j)  = qv_mean(i,k,j)  + dtm * grid%qv_lh_mc(i,k,j)

                   IF ( F_QR ) qr_mean(i,k,j)  = hydromet_hf(k, iirr)
                   IF ( F_QI ) qi_mean(i,k,j)  = hydromet_hf(k, iiri)
                   IF ( F_QS ) qs_mean(i,k,j)  = hydromet_hf(k, iirs)
                   IF ( F_QG ) qg_mean(i,k,j)  = hydromet_hf(k, iirg)

                   IF ( F_QNR ) qnr_mean(i,k,j)  = hydromet_hf(k, iiNr)
                   IF ( F_QNI ) qni_mean(i,k,j)  = hydromet_hf(k, iiNi)
                   IF ( F_QNS ) qns_mean(i,k,j)  = hydromet_hf(k, iiNs)
                   IF ( F_QNG ) qng_mean(i,k,j)  = hydromet_hf(k, iiNg)
                ENDDO

                ! Cleanup the hydromet array
                CALL cleanup_hydromet_array( hydromet_hf ) ! Intent(inout)

             ELSE ! Do hard clipping
!#############Adjust the end of the loop based on the routine of vendor WRF
!#############to prevent the seg fault in restarting
!#############C.S Feb 25th
!                DO k = kds, kde
                 DO k = k_start, MIN(k_end,kde-1)
!#############C.S Feb 25th

                   ! Update th_mean and qv_mean
                   th_mean(i,k,j)  = th_mean(i,k,j)  + dtm * grid%th_lh_mc(i,k,j)
                   IF ( F_QV ) qv_mean(i,k,j)  = qv_mean(i,k,j)  + dtm * grid%qv_lh_mc(i,k,j)

                   ! Hard clipping
                   th_mean(i,k,j)  = max(th_mean(i,k,j), 0.0)
                   IF ( F_QV ) qv_mean(i,k,j)  = max(qv_mean(i,k,j), rt_tol)
                   IF ( F_QC ) qc_mean(i,k,j)  = max(qc_mean(i,k,j), rt_tol)
                   IF ( F_QR ) qr_mean(i,k,j)  = max(qr_mean(i,k,j), 0.0)
                   IF ( F_QI ) qi_mean(i,k,j)  = max(qi_mean(i,k,j), 0.0)
                   IF ( F_QS ) qs_mean(i,k,j)  = max(qs_mean(i,k,j), 0.0)
                   IF ( F_QG ) qg_mean(i,k,j)  = max(qg_mean(i,k,j), 0.0)
                   IF ( F_QNDROP ) qndrop_mean(i,k,j)  = max(qndrop_mean(i,k,j), 0.0) !!!!! <hf>
!                   qndrop_mean(i,k,j)  = max(qndrop_mean(i,k,j), 0.0)
                   IF ( F_QNR ) qnr_mean(i,k,j)  = max(qnr_mean(i,k,j), 0.0)
                   IF ( F_QNI ) qni_mean(i,k,j)  = max(qni_mean(i,k,j), 0.0)
                   IF ( F_QNS ) qns_mean(i,k,j)  = max(qns_mean(i,k,j), 0.0)
                   IF ( F_QNG ) qng_mean(i,k,j)  = max(qng_mean(i,k,j), 0.0)

                ENDDO ! k

             ENDIF ! l_use_hf_driver

          ENDDO ! j
       ENDDO ! i

    ENDIF ! config_flags%lh_microphys_type == lh_microphys_interactive

    RETURN
  END SUBROUTINE est_subcol_microphys_tndcy
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  SUBROUTINE setup_hydromet_array( nz, qr_mean, qi_mean, qs_mean, qg_mean, & ! Intent(in)
                                   qndrop_mean, qnr_mean, qni_mean,        & ! Intent(in)
                                   qns_mean, qng_mean,                     & ! Intent(in)
                                   hydromet )                                ! Intent(out)

    ! Description:
    !   This subroutine creates the hydromet array (as used in CLUBB) from the
    !   respective quantities in WRF, depending on the microphysics scheme.
    !
    ! Side Effects: - Sets iirg = 8 and allocates the following arrays
    !                    > hydromet
    !               - cleanup_hydromet_array to revert
    !
    ! References:
    !   None
    !-----------------------------------------------------------------------------
    USE clubb_api_module, ONLY: &
      core_rknd, & ! Variable(s)

      zero, & ! Variable(s)

      iirr, & ! Variable(s)
      iirs, &
      iiri, &
      iirg, &
      iiNr, &
      iiNs, &
      iiNi, &
      iiNg, &

      hydromet_dim  ! Variable(s)

    IMPLICIT NONE

    ! Input Variables
    INTEGER, INTENT(IN) :: nz ! Number of height levels

    real(kind=core_rknd), dimension(nz), intent(in) :: &
      qr_mean, qi_mean, qs_mean, qg_mean,   &
      qndrop_mean, qnr_mean, qni_mean, qns_mean, qng_mean

    ! Output Variables
    REAL(kind=core_rknd), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: hydromet

    ! ---- Begin Code ----

    allocate( hydromet(nz, hydromet_dim) )
    hydromet = zero

    IF ( iirr > 0 ) THEN
       hydromet(:,iirr) = qr_mean(:)
    ENDIF

    IF ( iiri > 0 ) THEN
       hydromet(:,iiri) = qi_mean(:)
    ENDIF

    IF ( iirs > 0 ) THEN
       hydromet(:,iirs) = qs_mean(:)
    ENDIF

    IF ( iirg > 0 ) THEN
       hydromet(:,iirg) = qg_mean(:)
    ENDIF

    IF ( iiNr > 0 ) THEN
       hydromet(:,iiNr) = qnr_mean(:)
    ENDIF

    IF ( iiNs > 0 ) THEN
       hydromet(:,iiNs) = qns_mean(:)
    ENDIF

    IF ( iiNi > 0 ) THEN
       hydromet(:,iiNi) = qni_mean(:)
    ENDIF

    IF ( iiNg > 0 ) THEN
       hydromet(:,iiNg) = qng_mean(:)
    ENDIF

    RETURN
  END SUBROUTINE setup_hydromet_array

  !-----------------------------------------------------------------------------
  SUBROUTINE cleanup_hydromet_array( hydromet ) ! Intent(inout)

    ! Description:
    !   This subroutine reverts the side effects of setup_hydromet_array.
    !
    ! References:
    !   None
    !-----------------------------------------------------------------------------
    USE clubb_api_module, ONLY: core_rknd ! Variable(s)

    IMPLICIT NONE

    ! Input/Output Variables
    REAL(kind=core_rknd), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: hydromet

    ! ---- Begin Code ----

    deallocate( hydromet )

    RETURN
  END SUBROUTINE cleanup_hydromet_array

END MODULE module_est_subcol_microphys_tndcy
#endif /* SILHS */
