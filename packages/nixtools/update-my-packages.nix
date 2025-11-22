{
  nvfetcher,
  writeShellApplication,
}:

writeShellApplication {
  name = "update-my-packages";
  runtimeInputs = [
    nvfetcher
  ];
  text = # sh
    ''
      cd packages
      nvfetcher
    '';
}
