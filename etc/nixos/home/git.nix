{ pkgs, ... }:

{
  programs = {
    git = {
      enable = true;

      userName = "K. Adam Christensen";
      userEmail = "pope@shifteleven.com";

      extraConfig = {
        init.defaultBranch = "main";

        user.signingkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILseU33TteTzteZ3/DLD8GDPje3STusw6HrckI0ozEPo";

        commit.gpgsign = true;

        gpg.format = "ssh";
        "gpg \"ssh\"" = {
          program = "${pkgs._1password-gui}/bin/op-ssh-sign";
          allowedSignersFile = "/home/pope/.ssh/allowed_signers";
        };
      };
    };

    gh.enable = true;
  };

  home.file.".ssh/allowed_signers".text = ''
    pope@shifteleven.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILseU33TteTzteZ3/DLD8GDPje3STusw6HrckI0ozEPo
  '';

  home.file.".ssh/config".text = ''
    Host *
      IdentityAgent ~/.1password/agent.sock
  '';
}