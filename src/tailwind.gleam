import gleam/io
import gleam/result
import gleam/bit_array
import gleam/string
import simplifile
import gleam/httpc
import gleam/http.{Get}
import gleam/http/request
import gleam/http/response
import tom

const tailwind_config_path = "./tailwind.config.js"

const config_path = "./gleam.toml"

const latest_version = "3.3.3"

pub fn main() {
  io.println("Installing TailwindCSS...")
  // let assert Ok(Nil) = generate_config()

  let assert Ok(config) = simplifile.read(config_path)
  let assert Ok(version) = get_tailwind_version(config)
  io.debug(version)
  // let assert Ok(Nil) = download_tailwind(latest_version, "macos-arm64")
}

@external(erlang, "erlang", "system_info")
fn system_info(a: String) -> BitString

fn generate_config() -> Result(Nil, String) {
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
      |> result.map_error(fn(err) {
        "Error: Couldn't create tailwind config. Reason: " <> string.inspect(
          err,
        )
      })
      |> result.try(fn(_) {
        io.println("TailwindCSS config created.")
        Ok(Nil)
      })
  }
}

fn get_tailwind_version(config: String) -> Result(String, String) {
  config
  |> tom.parse()
  |> result.replace_error("Error: Couldn't parse config.")
  |> result.try(fn(parsed) {
    tom.get_string(parsed, ["tailwind", "version"])
    |> result.replace_error("Error: Version not found.")
  })
}

fn try_read_file(path: String) -> Result(String, String) {
  case simplifile.is_file(path) {
    True -> {
      simplifile.read(path)
      |> result.map_error(fn(err) {
        "Error: Couldn't read file. Reason: " <> string.inspect(err)
      })
    }

    False -> {
      Error("Error: " <> path <> " not found.")
    }
  }
}

// fn target() -> String {
//   arch_str = :erlang.system_info(:system_architecture)
//     [arch | _] = arch_str |> List.to_string() |> String.split("-")

//     case #(:os.type(), arch, :erlang.system_info(:wordsize) * 8) do
//       #(#(:win32, _), _arch, 64) -> "windows-x64.exe"
//       #(#(:unix, :darwin), arch, 64) when arch in ~w(arm aarch64) -> "macos-arm64"
//       #(#(:unix, :darwin), "x86_64", 64) -> "macos-x64"
//       #(#(:unix, :freebsd), "aarch64", 64) -> "freebsd-arm64"
//       #(#(:unix, :freebsd), "amd64", 64) -> "freebsd-x64"
//       #(#(:unix, :linux), "aarch64", 64) -> "linux-arm64"
//       #(#(:unix, :linux), "arm", 32) -> "linux-armv7"
//       #(#(:unix, :linux), "armv7" <> _, 32) -> "linux-armv7"
//       #(#(:unix, _osname), arch, 64) when arch in ~w(x86_64 amd64) -> "linux-x64"
//       #(_os, _arch, _wordsize) -> panic as "tailwind is not available for architecture: #{arch_str}"
//     end
// }

fn download_tailwind(version: String, target: String) -> Result(Nil, String) {
  let path =
    string.concat([
      "/tailwindlabs/tailwindcss/releases/download/v",
      version,
      "/tailwindcss-",
      target,
    ])

  let assert Ok(Nil) = simplifile.create_directory_all("./build/bin/")

  io.println("Downloading TailwindCSS...")

  request.new()
  |> request.set_method(Get)
  |> request.set_host("github.com")
  |> request.set_path(path)
  |> request.map(bit_array.from_string)
  |> httpc.send_bits()
  |> result.map_error(fn(err) {
    "Error: Couldn't download tailwind. Reason: " <> string.inspect(err)
  })
  |> result.try(fn(resp) {
    simplifile.write_bits(resp.body, to: "./build/bin/tailwindcss-cli")
    |> result.map_error(fn(err) {
      "Error: Couldn't write tailwind binary. Reason: " <> string.inspect(err)
    })
  })
}
