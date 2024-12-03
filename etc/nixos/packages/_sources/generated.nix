# This file was generated by nvfetcher, please do not modify it manually.
{ fetchgit, fetchurl, fetchFromGitHub, dockerTools }:
{
  fish-catppuccin = {
    pname = "fish-catppuccin";
    version = "cc8e4d8fffbdaab07b3979131030b234596f18da";
    src = fetchgit {
      url = "https://github.com/catppuccin/fish";
      rev = "cc8e4d8fffbdaab07b3979131030b234596f18da";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-udiU2TOh0lYL7K7ylbt+BGlSDgCjMpy75vQ98C1kFcc=";
    };
    date = "2024-08-31";
  };
  fish-rose-pine = {
    pname = "fish-rose-pine";
    version = "38aab5baabefea1bc7e560ba3fbdb53cb91a6186";
    src = fetchgit {
      url = "https://github.com/rose-pine/fish";
      rev = "38aab5baabefea1bc7e560ba3fbdb53cb91a6186";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-bSGGksL/jBNqVV0cHZ8eJ03/8j3HfD9HXpDa8G/Cmi8=";
    };
    date = "2022-09-16";
  };
  fish-tokyonight = {
    pname = "fish-tokyonight";
    version = "04fc51e1f53afe3be14f9169bf800337a430d253";
    src = fetchgit {
      url = "https://github.com/vitallium/tokyonight-fish";
      rev = "04fc51e1f53afe3be14f9169bf800337a430d253";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-JI1kTez4CeMpSKcSikFUee15N48zkJJOvLHCi0H2PUc=";
    };
    date = "2024-07-17";
  };
  fsrcnnx-lineart = {
    pname = "fsrcnnx-lineart";
    version = "1.1";
    src = fetchurl {
      url = "https://github.com/igv/FSRCNN-TensorFlow/releases/download/1.1/checkpoints_params.7z";
      sha256 = "sha256-h5B7DU0W5B39rGaqC9pEqgTTza5dKvUHTFlEZM1mfqo=";
    };
  };
  fsrcnnx16 = {
    pname = "fsrcnnx16";
    version = "1.1";
    src = fetchurl {
      url = "https://github.com/igv/FSRCNN-TensorFlow/releases/download/1.1/FSRCNNX_x2_16-0-4-1.glsl";
      sha256 = "sha256-1aJKJx5dmj9/egU7FQxGCkTCWzz393CFfVfMOi4cmWU=";
    };
  };
  fsrcnnx8 = {
    pname = "fsrcnnx8";
    version = "1.1";
    src = fetchurl {
      url = "https://github.com/igv/FSRCNN-TensorFlow/releases/download/1.1/FSRCNNX_x2_8-0-4-1.glsl";
      sha256 = "sha256-6ADbxcHJUYXMgiFsWXckUz/18ogBefJW7vYA8D6Nwq4=";
    };
  };
  hatsune-miku-cursor = {
    pname = "hatsune-miku-cursor";
    version = "814a5d731e121ccdeb374f29ea1464fbf56bc36d";
    src = fetchgit {
      url = "https://github.com/supermariofps/hatsune-miku-windows-linux-cursors";
      rev = "814a5d731e121ccdeb374f29ea1464fbf56bc36d";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-OQjjOc9VnxJ7tWNmpHIMzNWX6WsavAOkgPwK1XAMwtE=";
    };
    date = "gpg: Signature made Thu 28 Nov 2024 08:42:52 AM PST\ngpg:                using RSA key B5690EEEBB952194\ngpg: Can't check signature: No public key\n2024-11-28";
  };
  iqm = {
    pname = "iqm";
    version = "e5004bb3f46eafece81376ecafccc7f3ae747b59";
    src = fetchgit {
      url = "https://github.com/lsalzman/iqm";
      rev = "e5004bb3f46eafece81376ecafccc7f3ae747b59";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-M7cn/TEG7hP6U9JpaAVp2vLAQxHNxnBvAU39vFnRiEM=";
    };
    date = "2024-06-23";
  };
  krigBilateral = {
    pname = "krigBilateral";
    version = "038064821c5f768dfc6c00261535018d5932cdd5";
    src = fetchurl {
      url = "https://gist.githubusercontent.com/igv/a015fc885d5c22e6891820ad89555637/raw/038064821c5f768dfc6c00261535018d5932cdd5/KrigBilateral.glsl";
      sha256 = "sha256-ikeYq7d7g2Rvzg1xmF3f0UyYBuO+SG6Px/WlqL2UDLA=";
    };
  };
  modernx = {
    pname = "modernx";
    version = "0.6.1";
    src = fetchFromGitHub {
      owner = "cyl0";
      repo = "ModernX";
      rev = "0.6.1";
      fetchSubmodules = false;
      sha256 = "sha256-q7DwyfmOIM7K1L7vvCpq1EM0RVpt9E/drhAa9rLYb1k=";
    };
  };
  mpv-prescalers = {
    pname = "mpv-prescalers";
    version = "b3f0a59d68f33b7162051ea5970a5169558f0ea2";
    src = fetchgit {
      url = "https://github.com/bjin/mpv-prescalers";
      rev = "b3f0a59d68f33b7162051ea5970a5169558f0ea2";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-KfCFU3fa8Fr5G5zVqKS35CJBzTYMY72kep8+Kd0YIu4=";
    };
    date = "2024-01-11";
  };
  p5r-grub = {
    pname = "p5r-grub";
    version = "v1.0";
    src = fetchFromGitHub {
      owner = "SiriusAhu";
      repo = "Persona_5_Royal_Grub_Themes";
      rev = "v1.0";
      fetchSubmodules = false;
      sha256 = "sha256-YnTBgUWWsR0W8eTtg3oa2MIXSqj7HW555xSEhV7/74w=";
    };
  };
  plow = {
    pname = "plow";
    version = "v1.3.1";
    src = fetchFromGitHub {
      owner = "six-ddc";
      repo = "plow";
      rev = "v1.3.1";
      fetchSubmodules = false;
      sha256 = "sha256-TynFq7e4MtZlA5SmGMybhmCVw67yHYgZWffQjuyhTDA=";
    };
  };
  rbutil = {
    pname = "rbutil";
    version = "1.5.1";
    src = fetchurl {
      url = "https://git.rockbox.org/cgit/rockbox.git/snapshot/rockbox-rbutil_1.5.1.tar.gz";
      sha256 = "sha256-XtiCXrROtJfNCcXDxwvtyxKzrSk0GGY/bDtimr3Iu50=";
    };
  };
  ssimDownscaler = {
    pname = "ssimDownscaler";
    version = "575d13567bbe3caa778310bd3b2a4c516c445039";
    src = fetchurl {
      url = "https://gist.githubusercontent.com/igv/36508af3ffc84410fe39761d6969be10/raw/575d13567bbe3caa778310bd3b2a4c516c445039/SSimDownscaler.glsl";
      sha256 = "sha256-AEq2wv/Nxo9g6Y5e4I9aIin0plTcMqBG43FuOxbnR1w=";
    };
  };
  ssimSuperRes = {
    pname = "ssimSuperRes";
    version = "15d93440d0a24fc4b8770070be6a9fa2af6f200b";
    src = fetchurl {
      url = "https://gist.githubusercontent.com/igv/2364ffa6e81540f29cb7ab4c9bc05b6b/raw/15d93440d0a24fc4b8770070be6a9fa2af6f200b/SSimSuperRes.glsl";
      sha256 = "sha256-qLJxFYQMYARSUEEbN14BiAACFyWK13butRckyXgVRg8=";
    };
  };
  yazi-plugins = {
    pname = "yazi-plugins";
    version = "a53d5440481f0f9a2160ded47d343bd22ffbc1fb";
    src = fetchgit {
      url = "https://github.com/yazi-rs/plugins";
      rev = "a53d5440481f0f9a2160ded47d343bd22ffbc1fb";
      fetchSubmodules = false;
      deepClone = false;
      leaveDotGit = false;
      sha256 = "sha256-I9u1d3l0AyNW/t1A7MAxfA6Wu1/L3XKUoWPZ9L85WTM=";
    };
    date = "Good \"git\" signature with ED25519 key SHA256:UpS/s3AKXiouO9D7joFqpoEMveeZ8A6ZyE48w2WkJ0U\nNo principal matched.\n2024-11-29";
  };
}
