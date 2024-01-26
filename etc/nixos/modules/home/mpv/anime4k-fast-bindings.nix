{ anime4k, setShader }:

# Optimized shaders for lower-end GPU:
# A for originally blurry lines, B for pretty sharp ones, C for potato resolution
{
  "CTRL+1" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
    message = "Anime4K: Mode A (Fast)";
  };
  "CTRL+2" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
    message = "Anime4K: Mode B (Fast)";
  };
  "CTRL+3" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Upscale_Denoise_CNN_x2_M.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
    message = "Anime4K: Mode C (Fast)";
  };
  "CTRL+4" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_M.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${anime4k}/Anime4K_Restore_CNN_S.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
    message = "Anime4K: Mode A+A (Fast)";
  };
  "CTRL+5" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Restore_CNN_Soft_M.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_M.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Restore_CNN_Soft_S.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
    message = "Anime4K: Mode B+B (Fast)";
  };
  "CTRL+6" = setShader {
    files = [
      "${anime4k}/Anime4K_Clamp_Highlights.glsl"
      "${anime4k}/Anime4K_Upscale_Denoise_CNN_x2_M.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x2.glsl"
      "${anime4k}/Anime4K_AutoDownscalePre_x4.glsl"
      "${anime4k}/Anime4K_Restore_CNN_S.glsl"
      "${anime4k}/Anime4K_Upscale_CNN_x2_S.glsl"
    ];
    message = "Anime4K: Mode C+A (Fast)";
  };
}
