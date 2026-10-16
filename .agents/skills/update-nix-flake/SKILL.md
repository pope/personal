---
name: update-nix-flake
description: >-
  Updates the Nix flake inputs and custom packages, audits and removes obsolete
  overlay workarounds, runs flake checks to catch warnings or deprecations, and
  builds/pushes NixOS configurations to the remote binary cache (skrapnel). Use
  this skill whenever the user asks to update the flake, update packages, run
  post-update flake checks, or build to the NixOS cache.
---

# Update Nix Flake & Build to Cache

Follow this workflow when updating the Nix flake and custom packages in this repository.

> **Important — Always Confirm Changes First**:
> Before making any edits to Nix settings, modules, packages, or overlays (beyond running the automated `nix flake update` and `nix run .#update-my-packages` commands), **always present your findings and proposed plan to the user and ask for confirmation**. Whenever multiple reasonable approaches exist (e.g., patching a package via `overrideAttrs` vs. temporarily pinning to `final.stable.<pkg>`), outline the alternatives and their trade-offs (such as local compilation time vs. using prebuilt cache binaries) so the user can choose.

## 1. Update Flake Inputs & Custom Packages

If the user has not already run the update commands in the current session, run them from the repository root:

1. Update `flake.lock`:
   ```bash
   nix flake update
   ```
1. Update custom packages in `packages/` that define `passthru.updateScript`:
   ```bash
   nix run .#update-my-packages
   ```

## 2. Check for Obsolete Overlay Workarounds

Inspect [overlays/default.nix](../../../overlays/default.nix) and [flake.nix](../../../flake.nix) for temporary workarounds marked with `# TODO(pope):` (for example, `# TODO(pope): Remove after PR #... lands in nixpkgs/nixos-unstable`).

- Check the updated `nixpkgs` input (e.g., via `nix eval --impure --raw .#nixosConfigurations.soundwave.pkgs.<pkg>.meta.position`) to see if the upstream fix or version bump has landed in `nixos-unstable`.
- If an override is now redundant, report it to the user and ask for confirmation before removing it.

## 3. Run Flake Checks & Audit Warnings/Deprecations

1. Run the full flake check suite:
   ```bash
   NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 NIXPKGS_ALLOW_UNFREE=1 nix flake check --impure
   ```
1. Explicitly verify that `config.warnings` is empty across all NixOS and Home Manager configurations, and verify that the `aarch64-darwin` Home Manager configuration evaluates cleanly (since `nix flake check` on `x86_64-linux` omits `aarch64-darwin` by default):
   ```bash
   nix eval --impure --json .#nixosConfigurations --apply 'builtins.mapAttrs (n: v: v.config.warnings)'
   nix eval --impure --json .#nixosConfigurations --apply 'builtins.mapAttrs (n: v: v.config.home-manager.users.pope.warnings)'
   nix eval --impure --json .#homeConfigurations --apply 'builtins.mapAttrs (n: v: v.config.warnings)'
   NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 NIXPKGS_ALLOW_UNFREE=1 nix eval --impure .#homeConfigurations.\"pope@galvatron\".activationPackage.drvPath
   ```
1. **Interpreting Warnings & Confirming Fixes**:
   - `warning: file 'nar/...' does not exist in binary cache 'http://skrapnel:5000'` is expected for newly updated paths not yet populated on `skrapnel` (Nix automatically falls back to `https://cache.nixos.org`).
   - `warning: The check omitted these incompatible systems: aarch64-darwin` is expected when running on `x86_64-linux`.
   - For any NixOS, Home Manager, or Nixvim option deprecations, warnings, or evaluation errors, identify the affected files under `hosts/`, `modules/`, `packages/`, or `overlays/`, explain the root cause, present your proposed fix (along with any relevant alternatives), and wait for user confirmation before applying changes.

## 4. Build & Push to the Remote NixOS Cache

1. If any `.nix` or `.md` files were modified after user confirmation, format the repository first so `checks.x86_64-linux.formatting` (`deadnix`, `mdformat`, `nixfmt`, `statix`) stays clean:
   ```bash
   nix fmt
   ```
1. Build and copy the NixOS host configurations (`unicron`, `rumble`, `skrapnel`, `ravage`) to `ssh://skrapnel.lan`:
   ```bash
   nix run .#nixos-build-to-cache
   ```
1. **Handling Upstream Build Regressions**:
   - If a package fails to build due to an upstream `nixos-unstable` regression, search `NixOS/nixpkgs` issues and PRs for the root cause and fix.
   - Present the options to the user and ask how they want to proceed—for example:
     - **Option A**: Apply the upstream PR/patch in [overlays/default.nix](../../../overlays/default.nix) annotated with `# TODO(pope): Remove after PR #<number> lands in nixpkgs/nixos-unstable` (noting if it requires compiling from source locally).
     - **Option B**: Temporarily fall back to `final.stable.<pkg>` (or another version) in [overlays/default.nix](../../../overlays/default.nix) if prebuilt binaries are available in `cache.nixos.org`.
   - Once the user confirms their preferred approach, apply the change, run `nix fmt`, and re-run `nix run .#nixos-build-to-cache` to confirm all hosts build and copy cleanly.
