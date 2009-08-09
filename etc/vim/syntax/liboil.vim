" Vim syntax file
" Language: C liboil extension (for version 0.3.16)
" Maintainer: David Nečas (Yeti) <yeti@physics.muni.cz>
" Last Change: 2009-04-02
" URL: http://physics.muni.cz/~yeti/vim/gtk-syntax.tar.gz
" Generated By: vim-syn-gen.py

syn keyword liboilFunction get_cpuinfo_line get_file get_file_int get_tag_value oil_abs_f32_f32 oil_abs_f64_f64 oil_abs_u16_s16 oil_abs_u32_s32 oil_abs_u8_s8 oil_add2_rshift_add_s16 oil_add2_rshift_sub_s16 oil_add_const_rshift_s16 oil_add_f32 oil_add_f64 oil_add_s16 oil_add_s16_u8 oil_addc_rshift_s16 oil_addc_s16 oil_arg_type_name oil_argb_paint_u8 oil_average2_u8 oil_avg2_12xn_u8 oil_avg2_16xn_u8 oil_avg2_32xn_u8 oil_avg2_8xn_u8 oil_ayuv2argb_u8 oil_ayuv2uyvy oil_ayuv2yuyv oil_ayuv2yvyu oil_clamp_f32 oil_clamp_f64 oil_clamp_s16 oil_clamp_s32 oil_clamp_s8 oil_clamp_u16 oil_clamp_u32 oil_clamp_u8 oil_clamphigh_f32 oil_clamphigh_f64 oil_clamphigh_s16 oil_clamphigh_s32 oil_clamphigh_s8 oil_clamphigh_u16 oil_clamphigh_u32 oil_clamphigh_u8 oil_clamplow_f32 oil_clamplow_f64 oil_clamplow_s16 oil_clamplow_s32 oil_clamplow_s8 oil_clamplow_u16 oil_clamplow_u32 oil_clamplow_u8 oil_class_choose_by_name oil_class_get oil_class_get_by_index oil_class_get_n_classes oil_class_optimize oil_class_register_impl oil_class_register_impl_by_name oil_class_register_impl_full oil_clip_f32 oil_clip_f64 oil_clip_s16 oil_clip_s32 oil_clip_s8 oil_clip_u16 oil_clip_u32 oil_clip_u8 oil_clipconv8x8_u8_s16 oil_clipconv_s16_f32 oil_clipconv_s16_f64 oil_clipconv_s16_s32 oil_clipconv_s16_u16 oil_clipconv_s16_u32 oil_clipconv_s32_f32 oil_clipconv_s32_f64 oil_clipconv_s32_u32 oil_clipconv_s8_f32 oil_clipconv_s8_f64 oil_clipconv_s8_s16 oil_clipconv_s8_s32 oil_clipconv_s8_u16 oil_clipconv_s8_u32 oil_clipconv_s8_u8 oil_clipconv_u16_f32 oil_clipconv_u16_f64 oil_clipconv_u16_s16 oil_clipconv_u16_s32 oil_clipconv_u16_u32 oil_clipconv_u32_f32 oil_clipconv_u32_f64 oil_clipconv_u32_s32 oil_clipconv_u8_f32 oil_clipconv_u8_f64 oil_clipconv_u8_s16 oil_clipconv_u8_s32 oil_clipconv_u8_s8 oil_clipconv_u8_u16 oil_clipconv_u8_u32 oil_colorspace_argb oil_colsad8x8_u8 oil_combine2_12xn_u8 oil_combine2_16xn_u8 oil_combine2_8xn_u8 oil_combine4_12xn_u8 oil_combine4_16xn_u8 oil_combine4_32xn_u8 oil_combine4_8xn_u8 oil_compare_u8 oil_composite_add_argb oil_composite_add_argb_const_src oil_composite_add_u8 oil_composite_add_u8_const_src oil_composite_in_argb oil_composite_in_argb_const_mask oil_composite_in_argb_const_src oil_composite_in_over_argb oil_composite_in_over_argb_const_mask oil_composite_in_over_argb_const_src oil_composite_over_argb oil_composite_over_argb_const_src oil_composite_over_u8 oil_conv8x8_f64_s16 oil_conv8x8_s16_f64 oil_conv_f32_f64 oil_conv_f32_s16 oil_conv_f32_s32 oil_conv_f32_s8 oil_conv_f32_u16 oil_conv_f32_u32 oil_conv_f32_u8 oil_conv_f64_f32 oil_conv_f64_s16 oil_conv_f64_s32 oil_conv_f64_s8 oil_conv_f64_u16 oil_conv_f64_u32 oil_conv_f64_u8 oil_conv_s16_f32 oil_conv_s16_f64 oil_conv_s16_s32 oil_conv_s16_s8 oil_conv_s16_u16 oil_conv_s16_u32 oil_conv_s16_u8 oil_conv_s32_f32 oil_conv_s32_f64 oil_conv_s32_s16 oil_conv_s32_s8 oil_conv_s32_u16 oil_conv_s32_u32 oil_conv_s32_u8 oil_conv_s8_f32 oil_conv_s8_f64 oil_conv_s8_s16 oil_conv_s8_s32 oil_conv_s8_u16 oil_conv_s8_u32 oil_conv_s8_u8 oil_conv_u16_f32 oil_conv_u16_f64 oil_conv_u16_s16 oil_conv_u16_s32 oil_conv_u16_s8 oil_conv_u16_u32 oil_conv_u16_u8 oil_conv_u32_f32 oil_conv_u32_f64 oil_conv_u32_s16 oil_conv_u32_s32 oil_conv_u32_s8 oil_conv_u32_u16 oil_conv_u32_u8 oil_conv_u8_f32 oil_conv_u8_f64 oil_conv_u8_s16 oil_conv_u8_s32 oil_conv_u8_s8 oil_conv_u8_u16 oil_conv_u8_u32 oil_convert_s16_f32 oil_convert_s16_f64 oil_convert_s16_s32 oil_convert_s16_s8 oil_convert_s16_u16 oil_convert_s16_u32 oil_convert_s16_u8 oil_convert_s32_f64 oil_convert_s32_s16 oil_convert_s32_s8 oil_convert_s32_u16 oil_convert_s32_u32 oil_convert_s32_u8 oil_convert_s8_f32 oil_convert_s8_f64 oil_convert_s8_s16 oil_convert_s8_s32 oil_convert_s8_u16 oil_convert_s8_u32 oil_convert_s8_u8 oil_convert_u16_f32 oil_convert_u16_f64 oil_convert_u16_s16 oil_convert_u16_s32 oil_convert_u16_u32 oil_convert_u16_u8 oil_convert_u32_f64 oil_convert_u32_s32 oil_convert_u32_u16 oil_convert_u32_u8 oil_convert_u8_f32 oil_convert_u8_f64 oil_convert_u8_s16 oil_convert_u8_s32 oil_convert_u8_s8 oil_convert_u8_u16 oil_convert_u8_u32 oil_copy8x8_u8 oil_copy_u8 oil_cpu_get_flags oil_cpu_get_frequency oil_cpu_get_ticks_per_second oil_dct36_f32 oil_debug_get_level oil_debug_print oil_debug_set_level oil_debug_set_print_function oil_deinterleave oil_deinterleave2_s16 oil_dequantize8x8_s16 oil_diff8x8_average_s16_u8 oil_diff8x8_const128_s16_u8 oil_diff8x8_s16_u8 oil_diffsquaresum_f32 oil_diffsquaresum_f64 oil_divide_f32 oil_divide_f64 oil_err_inter8x8_u8 oil_err_inter8x8_u8_avg oil_err_intra8x8_u8 oil_fault_check_disable oil_fault_check_enable oil_fault_check_try oil_fdct8_f64 oil_fdct8x8_f64 oil_fdct8x8s_s16 oil_fdct8x8theora oil_floor_f32 oil_idct8_f64 oil_idct8theora_s16 oil_idct8x8_f64 oil_idct8x8_s16 oil_idct8x8lim10_f64 oil_idct8x8lim10_s16 oil_idct8x8theora_s16 oil_imdct12_f64 oil_imdct32_f32 oil_imdct36_f64 oil_impl_get_by_index oil_impl_is_runnable oil_impl_is_usable oil_init oil_init_no_optimize oil_interleave oil_interleave2_s16 oil_inverse_f32 oil_lift_add_135 oil_lift_add_mult_shift12 oil_lift_add_shift1 oil_lift_add_shift2 oil_lift_sub_135 oil_lift_sub_mult_shift12 oil_lift_sub_shift1 oil_lift_sub_shift2 oil_lshift_s16 oil_mas10_u8 oil_mas10_u8_l15 oil_mas10_u8_sym_l15 oil_mas12_addc_rshift_decim2_u8 oil_mas2_across_add_s16 oil_mas2_add_s16 oil_mas4_across_add_s16 oil_mas4_add_s16 oil_mas8_across_add_s16 oil_mas8_across_u8 oil_mas8_add_s16 oil_mas8_addc_rshift_decim2_u8 oil_mas8_u8 oil_mas8_u8_l15 oil_mas8_u8_sym_l15 oil_maximum_f32 oil_maximum_f64 oil_md5 oil_mdct12_f64 oil_mdct36_f64 oil_merge_linear_argb oil_merge_linear_u8 oil_minimum_f32 oil_minimum_f64 oil_mix_u8 oil_mt19937 oil_mult8x8_s16 oil_multiply_and_acc_12xn_s16_u8 oil_multiply_and_acc_16xn_s16_u8 oil_multiply_and_acc_24xn_s16_u8 oil_multiply_and_acc_6xn_s16_u8 oil_multiply_and_acc_8xn_s16_u8 oil_multiply_and_add_s16 oil_multiply_and_add_s16_u8 oil_multiply_f32 oil_multiply_f64 oil_multsum_f32 oil_multsum_f64 oil_negative_f32 oil_null oil_optimize oil_optimize_all oil_packyuyv oil_param_from_string oil_param_get_source_data oil_permute_f32 oil_permute_f64 oil_permute_s16 oil_permute_s32 oil_permute_s8 oil_permute_u16 oil_permute_u32 oil_permute_u8 oil_profile_get_ave_std oil_profile_init oil_profile_stamp oil_profile_stop_handle oil_prototype_append_param oil_prototype_free oil_prototype_from_string oil_prototype_to_arg_string oil_prototype_to_string oil_random_alpha oil_random_argb oil_random_f32 oil_random_f64 oil_random_s16 oil_random_s32 oil_random_s64 oil_random_s8 oil_random_u16 oil_random_u32 oil_random_u64 oil_random_u8 oil_recon8x8_inter oil_recon8x8_inter2 oil_recon8x8_intra oil_resample_linear_argb oil_resample_linear_u8 oil_rgb2bgr oil_rgb2rgba oil_rgb565_to_argb oil_rowsad8x8_u8 oil_sad12x12_12xn_u8 oil_sad12x12_u8 oil_sad16x16_16xn_u8 oil_sad16x16_u8 oil_sad8x8_8xn_u8 oil_sad8x8_f64 oil_sad8x8_f64_2 oil_sad8x8_s16 oil_sad8x8_s16_2 oil_sad8x8_u8 oil_sad8x8_u8_avg oil_scalaradd_f32 oil_scalaradd_f32_ns oil_scalaradd_f64 oil_scalaradd_s16 oil_scalaradd_s32 oil_scalaradd_s8 oil_scalaradd_u16 oil_scalaradd_u32 oil_scalaradd_u8 oil_scalarmult_f32 oil_scalarmult_f64 oil_scalarmult_s16 oil_scalarmult_s32 oil_scalarmult_s8 oil_scalarmult_u16 oil_scalarmult_u32 oil_scalarmult_u8 oil_scalarmultiply_f32_ns oil_scalarmultiply_f64_ns oil_scaleconv_f32_s16 oil_scaleconv_f32_s32 oil_scaleconv_f32_s8 oil_scaleconv_f32_u16 oil_scaleconv_f32_u32 oil_scaleconv_f32_u8 oil_scaleconv_f64_s16 oil_scaleconv_f64_s32 oil_scaleconv_f64_s8 oil_scaleconv_f64_u16 oil_scaleconv_f64_u32 oil_scaleconv_f64_u8 oil_scaleconv_s16_f32 oil_scaleconv_s16_f64 oil_scaleconv_s32_f32 oil_scaleconv_s32_f64 oil_scaleconv_s8_f32 oil_scaleconv_s8_f64 oil_scaleconv_u16_f32 oil_scaleconv_u16_f64 oil_scaleconv_u32_f32 oil_scaleconv_u32_f64 oil_scaleconv_u8_f32 oil_scaleconv_u8_f64 oil_scanlinescale2_u8 oil_sign_f32 oil_sincos_f64 oil_splat_u16_ns oil_splat_u32 oil_splat_u32_ns oil_splat_u8 oil_splat_u8_ns oil_split_135 oil_split_53 oil_split_approx97 oil_split_daub97 oil_squaresum_f32 oil_squaresum_f64 oil_squaresum_shifted_s16 oil_subtract_f32 oil_subtract_f64 oil_subtract_s16 oil_subtract_s16_u8 oil_sum_f64 oil_sum_s16 oil_swab_u16 oil_swab_u32 oil_synth_135 oil_synth_53 oil_synth_approx97 oil_synth_daub97 oil_tablelookup_u8 oil_test_check_impl oil_test_check_ref oil_test_cleanup oil_test_free oil_test_get_arg_post_n oil_test_get_arg_pre_n oil_test_get_arg_stride oil_test_get_source_data oil_test_get_value oil_test_init oil_test_new oil_test_set_iterations oil_test_set_test_footer oil_test_set_test_header oil_testzero_u8 oil_trans8x8_f64 oil_trans8x8_u16 oil_trans8x8_u32 oil_trans8x8_u8 oil_type_name oil_type_sizeof oil_unpackyuyv oil_unzigzag8x8_s16 oil_utf8_validate oil_uyvy2ayuv oil_vectoradd_f32 oil_vectoradd_f64 oil_vectoradd_s16 oil_vectoradd_s32 oil_vectoradd_s8 oil_vectoradd_s_f32 oil_vectoradd_s_f64 oil_vectoradd_s_s16 oil_vectoradd_s_s8 oil_vectoradd_s_u16 oil_vectoradd_s_u8 oil_vectoradd_u16 oil_vectoradd_u32 oil_vectoradd_u8 oil_yuv2rgbx_sub2_u8 oil_yuv2rgbx_sub4_u8 oil_yuv2rgbx_u8 oil_yuyv2ayuv oil_yvyu2ayuv oil_zigzag8x8_s16 strsplit
syn keyword liboilTypedef oil_bool
syn keyword liboilConstant OIL_ARG_DEST1 OIL_ARG_DEST2 OIL_ARG_DEST3 OIL_ARG_DSTR1 OIL_ARG_DSTR2 OIL_ARG_DSTR3 OIL_ARG_INPLACE1 OIL_ARG_INPLACE2 OIL_ARG_ISTR1 OIL_ARG_ISTR2 OIL_ARG_LAST OIL_ARG_M OIL_ARG_N OIL_ARG_SRC1 OIL_ARG_SRC2 OIL_ARG_SRC3 OIL_ARG_SRC4 OIL_ARG_SRC5 OIL_ARG_SSTR1 OIL_ARG_SSTR2 OIL_ARG_SSTR3 OIL_ARG_SSTR4 OIL_ARG_SSTR5 OIL_ARG_UNKNOWN OIL_DEBUG_DEBUG OIL_DEBUG_ERROR OIL_DEBUG_INFO OIL_DEBUG_LOG OIL_DEBUG_NONE OIL_DEBUG_WARNING OIL_IMPL_FLAG_3DNOW OIL_IMPL_FLAG_3DNOWEXT OIL_IMPL_FLAG_ALTIVEC OIL_IMPL_FLAG_ARM6 OIL_IMPL_FLAG_ASM OIL_IMPL_FLAG_CMOV OIL_IMPL_FLAG_DISABLED OIL_IMPL_FLAG_EDSP OIL_IMPL_FLAG_MMX OIL_IMPL_FLAG_MMXEXT OIL_IMPL_FLAG_OPT OIL_IMPL_FLAG_REF OIL_IMPL_FLAG_SSE OIL_IMPL_FLAG_SSE2 OIL_IMPL_FLAG_SSE3 OIL_IMPL_FLAG_SSSE3 OIL_IMPL_FLAG_VFP OIL_TYPE_INT OIL_TYPE_UNKNOWN
syn keyword liboilStruct OilFunctionClass OilFunctionImpl OilParameter OilProfile OilPrototype OilTest
syn keyword liboilMacro ARRAY_SIZE CLAMP DIVIDE_ROUND_UP MAX MIN OIL_CHECK_PROTOTYPE OIL_DEBUG OIL_DEBUG_PRINT OIL_DECLARE_CLASS OIL_DEFINE_CLASS OIL_DEFINE_CLASS_FULL OIL_DEFINE_IMPL OIL_DEFINE_IMPL_ASM OIL_DEFINE_IMPL_DEPENDS OIL_DEFINE_IMPL_FULL OIL_DEFINE_IMPL_FULL_WRAPPER OIL_DEFINE_IMPL_REF OIL_ERROR OIL_GET OIL_GNUC_PREREQ OIL_INCREMENT OIL_INFO OIL_LOG OIL_OFFSET OIL_OPT_FLAG_MANGLE OIL_OPT_MANGLE OIL_SSE_WRAPPER OIL_WARNING ROUND_SHIFT ROUND_UP_2 ROUND_UP_4 ROUND_UP_8 ROUND_UP_POW2 ROUND_UP_SHIFT oil_argb oil_argb_A oil_argb_B oil_argb_G oil_argb_R oil_argb_noclamp oil_clamp_255 oil_divide_255 oil_max oil_memcpy oil_min oil_muldiv_255 oil_profile_start oil_profile_stop oil_rand_f32 oil_rand_f64 oil_rand_s16 oil_rand_s32 oil_rand_s64 oil_rand_s8 oil_rand_u16 oil_rand_u32 oil_rand_u64 oil_rand_u8 oil_trans8x8_s16 oil_type_is_floating_point
syn keyword liboilEnum OilArgType OilDebugLevel OilImplFlag OilType
syn keyword liboilUserFunction OilDebugPrintFunc OilTestFunction
syn keyword liboilDefine M_PI NULL OIL_BEGIN_DECLS OIL_CPU_FLAG_MASK OIL_END_DECLS OIL_EXPORT OIL_FUNCTION OIL_INTERNAL OIL_NO_CLASSES OIL_OPT_SUFFIX OIL_PROFILE_HIST_LENGTH OIL_TEST_FOOTER OIL_TEST_HEADER oil_type_f32 oil_type_f64 oil_type_max_s16 oil_type_max_s32 oil_type_max_s8 oil_type_max_u16 oil_type_max_u32 oil_type_max_u8 oil_type_min_s16 oil_type_min_s32 oil_type_min_s8 oil_type_min_u16 oil_type_min_u32 oil_type_min_u8 oil_type_s16 oil_type_s32 oil_type_s64 oil_type_s8 oil_type_u16 oil_type_u32 oil_type_u64 oil_type_u8

" Default highlighting
if version >= 508 || !exists("did_liboil_syntax_inits")
  if version < 508
    let did_liboil_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink liboilFunction Function
  HiLink liboilTypedef Type
  HiLink liboilConstant Constant
  HiLink liboilStruct Type
  HiLink liboilMacro Macro
  HiLink liboilEnum Type
  HiLink liboilUserFunction Type
  HiLink liboilDefine Constant

  delcommand HiLink
endif

