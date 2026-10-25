import os from "node:os"
import fs from "node:fs"
import crypto from "node:crypto"

export function os_arch() {
  return os.arch()
}

export function os_platform() {
  return os.platform()
}

export function sha256_file(path) {
  return crypto.createHash("sha256").update(fs.readFileSync(path)).digest("hex")
}
