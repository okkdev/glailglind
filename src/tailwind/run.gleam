import gleam/io
import tailwind

pub fn main() {
  let assert Ok(args) = tailwind.get_args()
  let assert Ok(output) = tailwind.run(args)
  io.println(output)
}
