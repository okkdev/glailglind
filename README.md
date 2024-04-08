# Glailglind - Tailwind for Gleam

[![Package Version](https://img.shields.io/hexpm/v/glailglind)](https://hex.pm/packages/glailglind)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glailglind/)

Installs and invokes TailwindCSS CLI.

Heavily inspired by [phoenixframework/tailwind](https://github.com/phoenixframework/tailwind/).

Compatible with Erlang and Javascript targets. (Javascript target requires `curl` to be available on the host)

## Usage as module

### Install Package

```sh
gleam add glailglind --dev
```

### Configure TailwindCSS

You can configure TailwindCSS in your `gleam.toml` by adding a `tailwind` table:

```toml
[tailwind]
version = "3.4.1" # optional
args = [
    "--config=tailwind.config.js",
    "--input=./src/css/app.css",
    "--output=./priv/css/app.css"
]
path = "/path/to/tailwindcli" # optional
```

This lets you define the arguments being run.

Optionally define a specific TailwindCSS version or the direct path to the TailwindCSS CLI, if you don't want it to be installed automatically.

### Install TailwindCSS

```sh
gleam run -m tailwind/install
```

This downloads the Tailwind CLI to `./build/bin/tailwind-cli` and generates the `tailwind.config.js` in the root of the project.

### Import Tailwind in your CSS

```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

### Run TailwindCSS

```sh
gleam run -m tailwind/run
```

Executes tailwind with the defined arguments.

## Usage with [LustreSSG](https://github.com/lustre-labs/lustre_ssg)

#### [ℹ️ Lustre Dev Tools have their own TailwindCSS support!](https://github.com/lustre-labs/dev-tools)

### Install Package

```sh
gleam add glailglind
```

### (Optional) Specify version in config

`gleam.toml`

```toml
[tailwind]
version = "3.3.5"
```

### Install TailwindCSS

```sh
gleam run -m tailwind/install
```

### Import Tailwind in your CSS

```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

### Update build script

Import `tailwind` and add the run step to your build script.

Example:

```gleam
import gleam/list
import gleam/map
import gleam/io
import gleam/result

// Some data for your site
import app/data/posts

// Some functions for rendering pages
import app/page/index
import app/page/blog
import app/page/post

// Import the static site generator
import lustre/ssg

// Import Tailwind
import tailwind

pub fn main() {
  let posts = map.from_list({
    use post <- list.map(posts.all())
    #(post.id, post)
  })

  let build =
    ssg.new("./priv")
    |> ssg.add_static_route("/", index.view())
    |> ssg.add_static_route("/blog", blog.view(posts.all()))
    |> ssg.add_dynamic_route("/blog", posts, post.view)
    |> ssg.build
    |> result.map_error(fn(e) { string.inspect(e) })
    |> result.try(fn(_) {
      [
        "--config=tailwind.config.js", "--input=./assets/css/app.css",
        "--output=./priv/css/app.css",
      ]
      |> tailwind.run()
    })

  case build {
    Ok(m) -> {
      io.println(m)
      io.println("Build succeeded!")
    }
    Error(e) -> {
      io.debug(e)
      io.println("Build failed!")
    }
  }
}
```
