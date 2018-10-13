# $Id: Makefile 1000 2018-07-06 23:10:46Z bmg2@uwm.edu $


LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES = \
	module_bl_clubb.o \
	module_cam_shr_kind_mod.o \
	module_cam_support.o \
	module_cam_shr_const_mod.o \
	module_cam_physconst.o \
	module_cam_cldwat.o \
	module_cam_wv_saturation.o \
	module_cam_esinti.o \
	module_cam_gffgch.o \
	module_cam_error_function.o \
	module_cam_constituents.o \
	module_cam_trb_mtn_stress.o \
	module_cam_molec_diff.o \
	module_cam_upper_bc.o \
	module_cam_bl_diffusion_solver.o \
	module_cam_bl_eddy_diff.o \
	module_cam_mp_microp_aero.o \
	module_cam_mp_cldwat2m_micro.o \
	module_cam_mp_ndrop.o \
	module_cam_mp_modal_aero_initialize_data_phys.o \
	module_cam_mp_radconstants.o \
	module_cam_mp_conv_water.o \
	module_cam_mp_qneg3.o \
	module_data_cam_mam_aero.o \
	module_data_cam_mam_asect.o \
	module_cam_infnan.o \
	module_bl_ysu.o \
	module_bl_mrf.o \
	module_bl_gfs.o \
	module_bl_gfs2011.o \
	module_bl_myjpbl.o \
	module_bl_qnsepbl.o \
	module_bl_qnsepbl09.o \
	module_bl_acm.o \
	module_bl_mynn.o \
        module_bl_fogdes.o \
	module_bl_gwdo.o \
	module_bl_myjurb.o \
	module_bl_boulac.o \
        module_quadratic.o \
        module_cu_TestCases.o \
	module_bl_camuwpbl_driver.o \
	module_bl_mfshconvpbl.o \
	module_shcu_camuwshcu_driver.o \
	module_shcu_camuwshcu.o \
	module_shcu_grims.o \
	module_cu_camzm_driver.o \
	module_cu_camzm.o \
	module_bl_temf.o \
	module_bl_gbmpbl.o \
	module_cu_g3.o \
	module_cu_kf.o  \
	module_cu_bmj.o \
	module_cu_kfeta.o \
	module_cu_tiedtke.o\
	module_cu_gd.o \
	module_cu_gf.o \
	module_cu_nsas.o \
	module_cu_sas.o \
	module_cu_mesosas.o \
	module_cu_osas.o \
	module_mp_radar.o \
	module_mp_kessler.o \
        module_mp_feingold2m.o \
	module_mp_lin.o  \
	module_mp_sbu_ylin.o  \
	module_mp_wsm3.o \
	module_mp_wsm5.o \
	module_mp_wsm6.o \
	module_mp_etanew.o \
        module_mp_etaold.o \
	module_mp_HWRF.o \
	module_mp_thompson.o \
	module_mp_gsfcgce.o \
	module_mp_morr_two_moment.o \
	module_mp_milbrandt2mom.o \
	module_mp_nssl_2mom.o \
	module_mp_wdm5.o \
	module_mp_wdm6.o \
	module_mp_cammgmp_driver.o \
	module_ra_sw.o  \
	module_ra_clWRF_support.o  \
	module_ra_gsfcsw.o \
	module_ra_goddard.o \
	module_ra_rrtm.o  \
	module_ra_rrtmg_lw.o  \
	module_ra_rrtmg_sw.o  \
        module_ra_cam_support.o \
        module_ra_cam.o \
	module_ra_gfdleta.o \
        module_ra_flg.o \
	module_ra_HWRF.o \
	module_ra_hs.o  \
        module_ra_dyclw.o  \
        module_sf_TestCases.o \
	module_sf_sfclay.o \
	module_sf_sfclayrev.o \
	module_sf_gfs.o \
	module_sf_gfdl.o \
	module_sf_slab.o  \
	module_sf_noahdrv.o  \
	module_sf_noahlsm.o  \
	module_sf_clm.o  \
        module_sf_ssib.o  \
	module_sf_noah_seaice.o \
	module_sf_noah_seaice_drv.o \
	module_sf_noahlsm_glacial_only.o \
	module_sf_noahmpdrv.o \
	module_sf_noahmplsm.o \
	module_sf_noahmp_glacier.o \
        module_sf_urban.o  \
        module_sf_bep.o  \
        module_sf_bep_bem.o \
        module_sf_bem.o \
	module_sf_pxlsm.o \
	module_sf_pxlsm_data.o \
	module_sf_ruclsm.o \
	module_sf_sfcdiags.o \
	module_sf_sfcdiags_ruclsm.o \
	module_sf_sstskin.o \
	module_sf_tmnupdate.o \
	module_sf_ocean_driver.o \
	module_sf_oml.o \
	module_sf_3dpwp.o \
	module_sf_myjsfc.o \
	module_sf_qnsesfc.o \
	module_sf_mynn.o \
        module_sf_fogdes.o \
	module_sf_pxsfclay.o \
	module_sf_temfsfclay.o \
	module_sf_idealscmsfclay.o \
	module_sf_scmflux.o \
	module_sf_scmskintemp.o \
	module_physics_addtendc.o \
	module_physics_init.o \
	module_gfs_machine.o \
	module_gfs_funcphys.o \
	module_gfs_physcons.o \
	module_progtm.o \
	module_pbl_driver.o \
	module_data_gocart_dust.o \
	module_cumulus_driver.o \
	module_shallowcu_driver.o \
	module_microphysics_driver.o \
	module_microphysics_zero_out.o \
	module_est_subcol_microphys_tndcy.o \
	module_mixactivate.o \
	module_radiation_driver.o \
	module_surface_driver.o \
	module_diagnostics.o \
	module_lightning_driver.o \
	module_ltng_cpmpr92z.o \
	module_ltng_crmpr92.o \
	module_ltng_iccg.o \
	module_fdda_psufddagd.o \
	module_fdda_spnudging.o \
	module_fddagd_driver.o  \
        module_fddaobs_rtfdda.o \
	module_fddaobs_driver.o \
	module_wind_generic.o \
	module_wind_fitch.o \
	clubb/advance_helper_module.o \
	clubb/advance_windm_edsclrm_module.o \
	clubb/advance_wp2_wp3_module.o \
	clubb/advance_xm_wpxp_module.o \
	clubb/advance_xp2_xpyp_module.o \
	clubb/advance_xp3_module.o \
	clubb/array_index.o \
	clubb/calc_roots.o \
	clubb/calendar.o \
	clubb/clip_explicit.o \
	clubb/clip_semi_implicit.o \
	clubb/advance_clubb_core_module.o \
	clubb/clubb_precision.o \
	clubb/constants_clubb.o \
	clubb/corr_varnce_module.o \
	clubb/csr_matrix_module.o \
	clubb/diffusion.o \
	clubb/endian.o \
	clubb/error_code.o \
	clubb/file_functions.o \
	clubb/fill_holes.o \
	clubb/gmres_cache.o \
	clubb/gmres_wrap.o \
	clubb/grid_class.o \
	clubb/input_names.o \
	clubb/input_reader.o \
	clubb/interpolation.o \
	clubb/lapack_wrap.o \
	clubb/matrix_operations.o \
	clubb/mean_adv.o \
	clubb/mixing_length.o \
	clubb/model_flags.o \
	clubb/mono_flux_limiter.o \
	clubb/numerical_check.o \
	clubb/output_grads.o \
	clubb/output_netcdf.o \
	clubb/parameter_indices.o \
	clubb/parameters_model.o \
	clubb/parameters_tunable.o \
	clubb/pdf_closure_module.o \
	clubb/pdf_parameter_module.o \
	clubb/pos_definite_module.o \
	clubb/saturation.o \
	clubb/sigma_sqd_w_module.o \
	clubb/Skx_module.o \
	clubb/sponge_layer_damping.o \
	clubb/stat_file_module.o \
	clubb/stats_lh_sfc_module.o \
	clubb/stats_lh_zt_module.o \
	clubb/stats_rad_zm_module.o \
	clubb/stats_rad_zt_module.o \
	clubb/stats_sfc_module.o \
	clubb/stats_clubb_utilities.o \
	clubb/stats_type.o \
	clubb/stats_type_utilities.o \
	clubb/stats_variables.o \
	clubb/stats_zm_module.o \
	clubb/stats_zt_module.o \
	clubb/surface_varnce_module.o \
	clubb/T_in_K_module.o \
	clubb/turbulent_adv_pdf.o \
	clubb/variables_diagnostic_module.o \
	clubb/variables_prognostic_module.o \
	clubb/diagnose_correlations_module.o \
	clubb/mt95.o \
	clubb/hydromet_pdf_parameter_module.o \
	clubb/precipitation_fraction.o \
	clubb/setup_clubb_pdf_params.o \
	clubb/pdf_utilities.o \
	clubb/new_pdf.o \
	clubb/new_pdf_main.o \
	clubb/adg1_adg2_3d_luhar_pdf.o \
        clubb/new_tsdadg_pdf.o \
	clubb/LY93_pdf.o \
	clubb/index_mapping.o \
	clubb/Nc_Ncn_eqns.o \
	clubb/clubb_api_module.o \
	silhs/est_kessler_microphys_module.o \
	silhs/transform_to_pdf_module.o \
	silhs/latin_hypercube_arrays.o \
	silhs/latin_hypercube_driver_module.o \
	silhs/math_utilities.o \
	silhs/output_2D_samples_module.o \
	silhs/generate_uniform_sample_module.o \
	silhs/parameters_silhs.o \
	silhs/silhs_api_module.o \
	silhs/silhs_importance_sample_module.o \
	silhs/lh_microphys_var_covar_module.o

FIRE_MODULES = \
        module_fr_fire_driver.o \
        module_fr_fire_driver_wrf.o \
        module_fr_fire_atm.o \
        module_fr_fire_model.o \
        module_fr_fire_core.o \
        module_fr_fire_phys.o \
        module_fr_fire_util.o
  
OBJS    =

NMM_MODULES =  

LIBTARGET    =  physics
TARGETDIR    =  ./

$(LIBTARGET) :
		if [ $(WRF_NMM_CORE) -eq 1 ] ; then \
                  $(MAKE) $(J) nmm_contrib                                      ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(OBJS) $(NMM_OBJS) $(NMM_MODULES) ; \
                else                                                         \
                  $(MAKE) $(J) non_nmm                                          ; \
                  $(AR) $(ARFLAGS) ../main/$(LIBWRFLIB) $(MODULES) $(FIRE_MODULES) $(OBJS)             ; \
                fi

include ../configure.wrf

nmm_contrib : $(NMM_OBJS) $(NMM_MODULES) $(MODULES) $(OBJS)

non_nmm : $(MODULES) $(FIRE_MODULES) $(WIND_MODULES) $(OBJS)

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
# DO NOT DELETE

include ../main/depend.common
