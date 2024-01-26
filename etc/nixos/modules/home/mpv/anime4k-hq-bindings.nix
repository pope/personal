{ anime4k, setShader }:

# Optimized shaders for high end GPUs to maximize quality:
# A for originally blurry lines, B for pretty sharp ones, C for potato resolution
{
  "CTRL+1" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_VL.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
    message = "Anime4K: Mode A (HQ)";
  };
  "CTRL+2" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_Soft_VL.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
    message = "Anime4K: Mode B (HQ)";
  };
  "CTRL+3" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
    message = "Anime4K: Mode C (HQ)";
  };
  "CTRL+4" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_VL.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
    message = "Anime4K: Mode A+A (HQ)";
  };
  "CTRL+5" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_Soft_VL.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_VL.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
    message = "Anime4K: Mode B+B (HQ)";
  };
  "CTRL+6" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Upscale_Denoise_CNN_x2_VL.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
    ];
    message = "Anime4K: Mode C+A (HQ)";
  };
}
