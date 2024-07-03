//// Contains all the functions necessary to install and execute the TailwindCSS CLI.
//// 

@target(erlang)
import gleam/bit_array
import gleam/dict.{type Dict}
@target(erlang)
import gleam/http.{Get}
@target(erlang)
import gleam/http/request
@target(erlang)
import gleam/httpc
import gleam/io
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import shellout
import simplifile
import tom.{type Toml, String}

const tailwind_config_path = "./tailwind.config.js"

const tailwindcli_path = "./build/bin/tailwindcss-cli"

const config_path = "./gleam.toml"

const latest_version = "3.4.1"

@external(erlang, "tailwind_erl", "os_arch")
@external(javascript, "./tailwind_js.mjs", "os_arch")
fn os_arch() -> String

@external(erlang, "tailwind_erl", "os_platform")
@external(javascript, "./tailwind_js.mjs", "os_platform")
fn os_platform() -> String

/// Downloads the TailwindCSS CLI matching your operating system and architecture.
pub fn install() {
  io.println("Installing TailwindCSS...")

  let output =
    generate_config()
    |> result.try(fn(_) {
      let version = get_tailwind_version()
      download_tailwind(version, target())
    })

  case output {
    Ok(_) -> {
      io.println("TailwindCSS installed!")
      Ok(Nil)
    }
    Error(err) -> {
      io.println(err)
      Error(err)
    }
  }
}

/// Executes the TailwindCSS CLI with the passed arguments.
/// # Example
/// ```gleam
/// > run(["--config=tailwind.config.js", "--input=./test/input.css", "--output=./build/css/output.css"])
/// Rebuilding...
/// Done in 90ms.
/// ```
pub fn run(args: List(String)) -> Result(String, String) {
  let cli = get_cli_path()
  case simplifile.is_file(cli) {
    Ok(True) -> {
      shellout.command(run: cli, with: args, in: ".", opt: [])
      |> result.map_error(fn(err) { pair.second(err) })
    }
    _otherwise -> {
      Error("Error: TailwindCSS CLI isn't installed.")
    }
  }
}

/// Installs and then executes the TailwindCSS CLI with the passed arguments.
/// 
/// Check `install()` and `run()`.
pub fn install_and_run(args: List(String)) -> Result(String, String) {
  install()
  |> result.try(fn(_) { run(args) })
}

fn generate_config() -> Result(Nil, String) {
  case simplifile.is_file(tailwind_config_path) {
    Ok(True) -> {
      io.println("TailwindCSS config already exists.")
      Ok(Nil)
    }

    _otherwise ->
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
        "Error: Couldn't create tailwind config. Reason: "
        <> string.inspect(err)
      })
      |> result.map(fn(_) {
        io.println("TailwindCSS config created.")
        Nil
      })
  }
}

fn get_config() -> Result(Dict(String, Toml), String) {
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
    |> result.replace_error("Error: Config key \"" <> key <> "\" not found.")
  })
}

/// Fetches the argument list from the `gleam.toml`.
/// 
/// Public because it's needed in `tailwind/run`.
pub fn get_args() -> Result(List(String), String) {
  get_config()
  |> result.try(fn(parsed) {
    tom.get_array(parsed, ["tailwind", "args"])
    |> result.replace_error(
      "Error: Config arguments not found. Is the \"args\" key set in the \"gleam.toml\"?",
    )
    |> result.map(fn(args) {
      list.map(args, fn(arg) {
        case arg {
          String(a) -> a
          _ -> ""
        }
      })
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
  case os_platform(), os_arch() {
    "win32", "x86_64" | "win32", "x64" -> "windows-x64.exe"
    "win32", "arm" <> _ -> "windows-arm64.exe"
    "darwin", "aarch64" | "darwin", "arm" <> _ -> "macos-arm64"
    "darwin", "x86_64" | "darwin", "x64" -> "macos-x64"
    "linux", "aarch64" | "linux", "arm64" -> "linux-arm64"
    "linux", "armv7" <> _ -> "linux-armv7"
    "linux", "x86_64" | "linux", "x64" | "linux", "amd64" -> "linux-x64"
    os, arch ->
      panic as string.join(
        ["Error: TailwindCSS CLI is not available for", os, arch],
        with: " ",
      )
  }
}

fn download_tailwind(version: String, target: String) -> Result(Nil, String) {
  case simplifile.is_file(tailwindcli_path) {
    Ok(True) -> {
      io.println("TailwindCSS CLI already exists.")
      Ok(Nil)
    }

    _otherwise -> {
      let url_path =
        string.concat([
          "/tailwindlabs/tailwindcss/releases/download/v",
          version,
          "/tailwindcss-",
          target,
        ])

      let assert Ok(Nil) = simplifile.create_directory_all("./build/bin/")

      io.println("Downloading TailwindCSS " <> target <> "...")

      download_bin(url_path)
      |> result.try(fn(_) {
        simplifile.set_permissions_octal(tailwindcli_path, 0o755)
        |> result.map_error(fn(err) {
          "Error: Can't change tailwindcli permissions. Reason: "
          <> string.inspect(err)
        })
      })
    }
  }
}

@target(javascript)
fn download_bin(path: String) -> Result(Nil, String) {
  shellout.command(
    run: "curl",
    with: ["-sL", "https://github.com" <> path, "-o", tailwindcli_path],
    in: ".",
    opt: [],
  )
  |> result.replace(Nil)
  |> result.map_error(fn(err) {
    "Error: Couldn't download tailwindcss cli. Reason: " <> pair.second(err)
  })
}

@target(erlang)
fn download_bin(path: String) -> Result(Nil, String) {
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
      "Error: Couldn't write tailwind binary. Reason: " <> string.inspect(err)
    })
  })
}
