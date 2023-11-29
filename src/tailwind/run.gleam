//// Runs the TailwindCSS CLI with the args defined in the `gleam.toml`.
//// 
//// ⚠️ This module is to be ran directly as gleam module.
//// 
////  ## Example
////     $ gleam run -m tailwind/run
//// 

import gleam/io
import gleam/result
import tailwind

/// ⚠️ This function is to be ran directly as gleam module.
/// 
///  # Example
///     $ gleam run -m tailwind/run
/// 
/// If you wish to run tailwind programmatically use `tailwind.run(args)`.
pub fn main() {
  let output =
    tailwind.get_args()
    |> result.try(fn(args) { tailwind.run(args) })
    |> result.try(fn(output) {
      io.println(output)
      Ok(Nil)
    })

  case output {
    Error(err) -> io.println(err)
    _ -> Nil
  }
}
