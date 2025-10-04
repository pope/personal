{
  wakelan,
  writeShellApplication,
}:

writeShellApplication {
  name = "wake-up-unicron";
  runtimeInputs = [
    wakelan
  ];
  text = "wakelan 58:11:22:d1:9c:0c";
}
