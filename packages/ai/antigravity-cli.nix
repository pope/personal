{
  lib,
  stdenvNoCC,
  fetchurl,
  autoPatchelfHook,
  makeBinaryWrapper,
  versionCheckHook,
}:
let
  wholeVersion = "1.0.11-5686572788613120"; # unfortunately this has dumb versioning
  version = builtins.head (lib.splitString "-" wholeVersion);

  throwSystem = throw "Unsupported system: ${stdenvNoCC.hostPlatform.system}";

  sourceData = {
    x86_64-linux = fetchurl {
      url = "https://storage.googleapis.com/antigravity-public/antigravity-cli/${wholeVersion}/linux-x64/cli_linux_x64.tar.gz";
      hash = "sha256-jDmnONo4dFyZ718pfghJPpQ5gHUnuqLTCNZBSSEJSuE=";
    };
    aarch64-linux = fetchurl {
      url = "https://storage.googleapis.com/antigravity-public/antigravity-cli/${wholeVersion}/linux-arm/cli_linux_arm64.tar.gz";
      hash = "sha256-YTCRdby34+Y3cmHPV1g9FvY3eHLw3F2tNqbXemw2NYk=";
    };
    aarch64-darwin = fetchurl {
      url = "https://storage.googleapis.com/antigravity-public/antigravity-cli/${wholeVersion}/darwin-arm/cli_mac_arm64.tar.gz";
      hash = "sha256-hfD1dHKaJhOdFlM0IYoEoxsgKi+qLXNjAVmxAZc2MpA=";
    };
    x86_64-darwin = fetchurl {
      url = "https://storage.googleapis.com/antigravity-public/antigravity-cli/${wholeVersion}/darwin-x64/cli_mac_x64.tar.gz";
      hash = "sha256-iDVwlKyee4pU/egpeYB39DjSqCjQCBRFkFRtlziOj5Y=";
    };
  };
in
stdenvNoCC.mkDerivation (_finalAttrs: {
  pname = "antigravity-cli";
  inherit version;

  strictDeps = true;
  __structuredAttrs = true;

  src = sourceData.${stdenvNoCC.hostPlatform.system} or throwSystem;

  sourceRoot = ".";

  nativeBuildInputs = lib.optionals stdenvNoCC.isLinux [
    autoPatchelfHook
    makeBinaryWrapper
  ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 antigravity $out/bin/agy

    runHook postInstall
  '';

  # agy runs agent commands inside its own nsjail, which bind-mounts only the
  # host's /lib, /lib64 and every directory on $PATH. On NixOS the binary's
  # glibc interpreter lives in /nix/store (on none of those), so the jail cannot
  # exec it and command execution fails; agy then falls back to running
  # unsandboxed on the host. Putting /nix/store on $PATH makes nsjail bind-mount
  # the store into the jail so the interpreter and every tool/library resolve.
  postFixup = lib.optionalString stdenvNoCC.isLinux ''
    wrapProgram $out/bin/agy --suffix PATH : /nix/store
  '';

  nativeInstallCheckInputs = [ versionCheckHook ];
  doInstallCheck = true;

  passthru = {
    inherit wholeVersion; # for the updateScript
    # updateScript = ./update.sh;
  };

  meta = {
    description = "Google's Go-based terminal user interface (TUI) agent client";
    homepage = "https://antigravity.google";
    changelog = "https://antigravity.google/changelog";
    license = lib.licenses.unfree;
    maintainers = with lib.maintainers; [
      adrielvelazquez
      u3kkasha
    ];
    platforms = lib.attrNames sourceData;
    mainProgram = "agy";
    sourceProvenance = with lib.sourceTypes; [ binaryNativeCode ];
  };
})
