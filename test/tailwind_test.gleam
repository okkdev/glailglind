import gleeunit
import gleeunit/should
import tailwind

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn install_test() {
  tailwind.install()
  |> should.equal(Nil)
}
