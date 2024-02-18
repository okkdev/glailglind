import os from "node:os"

export function os_arch() {
  return os.arch()
}

export function os_platform() {
  return os.platform()
}
