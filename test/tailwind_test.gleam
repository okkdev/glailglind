import gleeunit
import gleeunit/should
import tailwind
import simplifile

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn install_test() {
  tailwind.install()
  |> should.be_ok()

  simplifile.is_file("./build/bin/tailwindcss-cli")
  |> should.be_true()
}

pub fn run_test() {
  [
    "--config=tailwind.config.js", "--input=./test/input.css",
    "--output=./build/css/output.css",
  ]
  |> tailwind.run()
  |> should.be_ok()

  simplifile.is_file("./build/css/output.css")
  |> should.be_true()
}
