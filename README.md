
`cateq` is a program in OCaml which implements the solution to the word problem
on strict omega-categories. It is inspired from the article of M. Makkai “The
word problem on computads”. A description of the algorithm used by `cateq` can
be found in S. Forest “Computational Descriptions of Higher Categories” (PhD
thesis) and
[here](https://www.i2m.univ-amu.fr/perso/simon.forest/projects/cateq/index.html).

# Directly try `cateq` from a browser

For now, `cateq` can be tried from a browser
[here](https://www.i2m.univ-amu.fr/perso/simon.forest/projects/cateq/index.html).
Alternatively, you can launch the same webpage from the repository. Once the
repository is cloned, just do

``` shell
firefox web/index.html
```

in the repo directory.


# Compile the sources

## Install `opam`

In order to compile, we need an OCaml compiler and some additional packages. The
easiest way to install them is using `opam`. The latter can be installed with
the following commands:

``` shell
sudo apt update
sudo apt install opam m4
opam init
```

## Build `cateq`

We first install the required OCaml compiler and packages with the following command:

``` shell
opam install ocaml ocamlbuild menhir
```

We can now compile `cateq` with the following commands executed in the `src/`
directory:

``` shell
eval `opam config env`
make cateq
```


## Build the javascript code

In order to convert the OCaml code to javascript, we need additional packages
that we can install with

``` shell
opam install js_of_ocaml js_of_ocaml-ppx
```

The javascript code can then be compiled with the following commands executed in
the `src/` directory:

``` shell
eval `opam config env`
make web
```

The javascript file from the `web/` directory is then updated, and one can use
it by executing

``` shell
firefox web/index.html
```

as above.

# Use `cateq`

Once `cateq` is built, you can run it from the `src/` directory with:

``` shell
./cateq
```

For some examples on how to use `cateq`, open `web/index.html` in a browser.
