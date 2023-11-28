import gleam/io
import gleam/result
import tailwind

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
