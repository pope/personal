{
  wakelan,
  writeShellApplication,
}:

writeShellApplication {
  name = "wake-up-soundwave";
  runtimeInputs = [
    wakelan
  ];
  text = "wakelan 18:c0:4d:06:5c:15";
}
