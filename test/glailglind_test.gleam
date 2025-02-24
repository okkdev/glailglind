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

// eunit hack to increase timeout
@target(erlang)
pub fn install_test_() {
  let assert Ok(timeout) = atom.from_string("timeout")
  #(timeout, 60.0, [fn() { install_test_body() }])
}

@target(javascript)
pub fn install_test() {
  install_test_body()
}

fn install_test_body() {
  tailwind.install()
  |> should.be_ok()

  simplifile.is_file("./build/bin/tailwindcss")
  |> result.unwrap(False)
  |> should.be_true()
}

// This test depends on the install test so it will fail if its run first
pub fn run_test() {
  install_test_()

  ["--input=./test/input.css", "--output=./build/css/output.css"]
  |> tailwind.run()
  |> should.be_ok()

  simplifile.is_file("./build/css/output.css")
  |> result.unwrap(False)
  |> should.be_true()
}
