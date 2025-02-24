@target(erlang)
import gleam/erlang/atom
import gleam/result
import gleeunit
import gleeunit/should
import simplifile
import tailwind

pub fn main() {
  gleeunit.main()
}

@target(erlang)
pub fn install_erl_test_() {
  let assert Ok(timeout) = atom.from_string("timeout")
  #(timeout, 60.0, [install_test_body])
}

@target(javascript)
pub fn install_js_test() {
  install_test_body()
}

fn install_test_body() {
  tailwind.install()
  |> should.be_ok()

  simplifile.is_file("./build/bin/tailwindcss")
  |> result.unwrap(False)
  |> should.be_true()
}

pub fn tailwind_test() {
  ["--input=./test/input.css", "--output=./build/css/output.css"]
  |> tailwind.run()
  |> should.be_ok()

  simplifile.is_file("./build/css/output.css")
  |> result.unwrap(False)
  |> should.be_true()
}
