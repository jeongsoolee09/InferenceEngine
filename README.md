# InferenceEngine

<a href="https://develop.spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" alt="Made with Spacemacs"></a>

Watch the Demo: https://youtu.be/_tHX5Ud3P0o

## Deps

`opam install calendar ocamlgraph yojson utop core ppx_deriving dune re2`

## TroubleShooting

### `fatal error: 'cblas.h' file not found`

(Intel Mac)

- Run `brew reinstall openblas`
- and then `export PKG_CONFIG_PATH="/usr/local/opt/openblas/lib/pkgconfig:$PATH"`

### `mlgmpidl` related errors

(Apple Silicon, homebrew)

CFLAGS="-I`brew --prefix mpfr`/include -I`brew --prefix gmp`/include" LDFLAGS="-L`brew --prefix mpfr`/lib -L`brew --prefix gmp`/lib" opam install mlgmpidl
