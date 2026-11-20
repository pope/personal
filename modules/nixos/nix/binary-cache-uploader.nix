{
  config,
  lib,
  pkgs,
  ...
}:

let
  nixCfg = config.my.nixos.nix;
  cfg = nixCfg.binaryCacheUploader;
  tailscaleCacheHost = "skrapnel.gumiho-matrix.ts.net";
  localCacheHost = "skrapnel.lan";
  queueDir = "/run/nix-cache-upload";

  uploadHookScript = pkgs.writeShellApplication {
    name = "nix-cache-upload-hook";
    runtimeInputs = [
      pkgs.coreutils
      pkgs.systemd
      pkgs.util-linux
    ];
    text = ''
      if [ -z "''${OUT_PATHS:-}" ]; then
        exit 0
      fi

      IFS=' ' read -ra out_paths <<< "$OUT_PATHS"

      mkdir -p "${queueDir}"
      (
        flock 9
        printf '%s\n' "''${out_paths[@]}" >> "${queueDir}/queue"
      ) 9>"${queueDir}/lock"

      systemctl start --no-block nix-cache-upload.service || true
    '';
  };

  uploadWorkerScript = pkgs.writeShellApplication {
    name = "nix-cache-upload-worker";
    runtimeInputs = [
      config.nix.package
      pkgs.coreutils
      pkgs.openssh
      pkgs.util-linux
    ];
    text = ''
      mkdir -p "${queueDir}"

      while true; do
        has_batch=0
        (
          flock 9
          if [ -s "${queueDir}/queue" ]; then
            mv "${queueDir}/queue" "${queueDir}/batch"
            exit 0
          fi
          exit 1
        ) 9>"${queueDir}/lock" && has_batch=1 || true

        if [ "$has_batch" -eq 0 ]; then
          break
        fi

        mapfile -t paths < <(sort -u "${queueDir}/batch")
        rm -f "${queueDir}/batch"

        if [ "''${#paths[@]}" -eq 0 ]; then
          continue
        fi

        NIX_SSHOPTS="-i ${config.sops.secrets.nix-cache-upload-key.path} -o ConnectTimeout=5 -o BatchMode=yes" \
          nix copy \
            --no-check-sigs \
            --substitute-on-destination \
            --to "ssh-ng://nix-cache-upload@${cfg.host}" \
            "''${paths[@]}"
      done
    '';
  };

  nixCached = pkgs.writeShellApplication {
    name = "nix-cached";
    runtimeInputs = [ config.nix.package ];
    text = ''
      exec nix --option post-build-hook ${lib.getExe uploadHookScript} "$@"
    '';
  };
in
{
  options.my.nixos.nix.binaryCacheUploader = {
    enable = lib.mkEnableOption "binary cache upload hook and nix-cached helper";
    automatic = lib.mkEnableOption "automatically upload all local builds via post-build-hook";
    host = lib.mkOption {
      type = lib.types.str;
      default = tailscaleCacheHost;
      description = "Hostname to upload Nix store paths to via ssh-ng.";
    };
  };

  config = lib.mkIf (nixCfg.enable && cfg.enable) {
    assertions = [
      {
        assertion = config.my.nixos.sops.enable;
        message = "my.nixos.nix.binaryCacheUploader requires my.nixos.sops.enable = true";
      }
    ];

    environment.systemPackages = [ nixCached ];

    nix.settings.post-build-hook = lib.mkIf cfg.automatic (lib.getExe uploadHookScript);

    programs.ssh.knownHosts.skrapnel = {
      hostNames = [
        tailscaleCacheHost
        localCacheHost
        "skrapnel"
      ];
      # Skrapnel's SSH host key (/etc/ssh/ssh_host_ed25519_key.pub on skrapnel).
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKzMsPAOwFsEfnVIRE05tPeRu/XR6LfV+XL4YJ5UxySw";
    };

    sops.secrets.nix-cache-upload-key = { };

    systemd.services.nix-cache-upload = {
      description = "Upload queued Nix store paths to skrapnel";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe uploadWorkerScript;
      };
    };
  };
}
