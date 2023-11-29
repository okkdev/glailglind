//// Runs the TailwindCSS install.
//// 
//// ⚠️ This module is to be ran directly as gleam module.
//// 
////  ## Example
////     $ gleam run -m tailwind/install
//// 

import tailwind

/// ⚠️ This function is to be ran directly as gleam module.
/// 
///  # Example
///     $ gleam run -m tailwind/install
/// 
/// If you wish to install tailwind programmatically use `tailwind.install()`.
pub fn main() {
  tailwind.install()
}
