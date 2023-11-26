import gleam/io
import gleam/result
import simplifile.{type FileError}

const tailwind_config_path = "./tailwind.config.js"

pub fn main() {
  io.println("Hello from Tailwind!")
  let assert Ok(Nil) = generate_config()
}

fn generate_config() -> Result(Nil, FileError) {
  case simplifile.is_file(tailwind_config_path) {
    True -> {
      io.println("TailwindCSS config already exists.")
      Ok(Nil)
    }

    False ->
      "
// See the Tailwind configuration guide for advanced usage
// https://tailwindcss.com/docs/configuration

let plugin = require('tailwindcss/plugin')

module.exports = {
  content: [
    'src/*.gleam',
  ],
  theme: {
    extend: {},
  },
  plugins: [
    require('@tailwindcss/forms'),
  ]
}
"
      |> simplifile.write(tailwind_config_path)
      |> result.try(fn(_) {
        io.println("TailwindCSS config created.")
        Ok(Nil)
      })
  }
}
