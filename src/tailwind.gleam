import gleam/io
import gleam/result
import gleam/bit_array
import gleam/string
import gleam/int
import gleam/list
import gleam/httpc
import gleam/http.{Get}
import gleam/http/request
import gleam/map.{type Map}
import gleam/erlang/os.{Darwin, Linux, WindowsNt}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/charlist.{type Charlist}
import simplifile
import tom.{type Toml, String}

const tailwind_config_path = "./tailwind.config.js"

const tailwindcli_path = "./build/bin/tailwindcss-cli"

const config_path = "./gleam.toml"

const latest_version = "3.3.5"

@external(erlang, "erlang", "system_info")
fn system_arch(a: Atom) -> String

@external(erlang, "erlang", "system_info")
fn system_wordsize(a: Atom) -> Int

@external(erlang, "os", "cmd")
fn cmd(cmd: Charlist) -> String

@external(erlang, "tailwind_erl", "change_file_permissions")
fn change_file_permissions(file: String, permission: Int) -> Result(Nil, Atom)

pub fn install() {
  io.println("Installing TailwindCSS...")
  let assert Ok(Nil) = generate_config()
  let version = get_tailwind_version()
  let assert Ok(Nil) = download_tailwind(version, target())
  io.println("TailwindCSS installed!")
}

pub fn run(args: List(String)) -> Result(String, String) {
  let cli = get_cli_path()
  case simplifile.is_file(cli) {
    True -> {
      charlist.from_string(string.join([cli, ..args], " "))
      |> cmd()
      |> Ok()
    }
    False -> {
      Error("Error: TailwindCSS CLI isn't installed.")
    }
  }
}

pub fn install_and_run(args: List(String)) -> Result(String, String) {
  install()
  run(args)
}

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
  content: ['./src/**/*.{html,gleam}'],
  theme: {
    extend: {},
  },
  plugins: [require('@tailwindcss/forms')],
}
"
      |> simplifile.write(to: tailwind_config_path)
      |> result.map_error(fn(err) {
        "Error: Couldn't create tailwind config. Reason: " <> string.inspect(
          err,
        )
      })
      |> result.map(fn(_) {
        io.println("TailwindCSS config created.")
        Nil
      })
  }
}

fn get_config() -> Result(Map(String, Toml), String) {
  simplifile.read(config_path)
  |> result.map_error(fn(err) {
    "Error: Couldn't read config. Reason: " <> string.inspect(err)
  })
  |> result.try(fn(config) {
    tom.parse(config)
    |> result.replace_error("Error: Couldn't parse config.")
  })
}

fn get_config_string(key: String) -> Result(String, String) {
  get_config()
  |> result.try(fn(parsed) {
    tom.get_string(parsed, ["tailwind", key])
    |> result.replace_error("Error: Version not found.")
  })
}

pub fn get_args() -> Result(List(String), String) {
  get_config()
  |> result.try(fn(parsed) {
    tom.get_array(parsed, ["tailwind", "args"])
    |> result.replace_error("Error: Version not found.")
    |> result.map(fn(args) {
      list.map(
        args,
        fn(arg) {
          case arg {
            String(a) -> a
            _ -> ""
          }
        },
      )
    })
  })
}

fn get_cli_path() -> String {
  get_config_string("path")
  |> result.unwrap(tailwindcli_path)
}

fn get_tailwind_version() -> String {
  get_config_string("version")
  |> result.unwrap(latest_version)
}

fn target() -> String {
  let [arch, ..] =
    atom.create_from_string("system_architecture")
    |> system_arch()
    |> string.split(on: "-")

  let wordsize =
    atom.create_from_string("wordsize")
    |> system_wordsize()
    |> int.multiply(8)

  case #(os.family(), arch, wordsize) {
    #(WindowsNt, "x86_64", 64) -> "windows-x64.exe"
    #(WindowsNt, "arm", 64) -> "windows-arm64.exe"
    #(Darwin, "aarch64", 64) | #(Darwin, "arm", 64) -> "macos-arm64"
    #(Darwin, "x86_64", 64) -> "macos-x64"
    #(Linux, "aarch64", 64) -> "linux-arm64"
    #(Linux, "arm", 32) | #(Linux, "armv7" <> _, 32) -> "linux-armv7"
    #(Linux, "x86_64", 64) | #(Linux, "amd64", 64) -> "linux-x64"
    #(_os, _arch, _wordsize) ->
      panic as "Error: Tailwind is not available for " <> string.inspect(os.family()) <> " " <> arch
  }
}

fn download_tailwind(version: String, target: String) -> Result(Nil, String) {
  case simplifile.is_file(tailwindcli_path) {
    True -> {
      io.println("TailwindCSS CLI already exists.")
      Ok(Nil)
    }

    False -> {
      let path =
        string.concat([
          "/tailwindlabs/tailwindcss/releases/download/v",
          version,
          "/tailwindcss-",
          target,
        ])

      let assert Ok(Nil) = simplifile.create_directory_all("./build/bin/")

      io.println("Downloading TailwindCSS " <> target <> "...")

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
        simplifile.write_bits(resp.body, to: tailwindcli_path)
        |> result.map_error(fn(err) {
          "Error: Couldn't write tailwind binary. Reason: " <> string.inspect(
            err,
          )
        })
        |> result.try(fn(_) {
          change_file_permissions(tailwindcli_path, 755)
          |> result.map_error(fn(err) {
            "Error: Can't change tailwindcli permissions. Reason: " <> string.inspect(
              err,
            )
          })
        })
      })
    }
  }
}
