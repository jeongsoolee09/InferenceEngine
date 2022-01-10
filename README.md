# InferenceEngine

<a href="https://develop.spacemacs.org"><img src="https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg" alt="Made with Spacemacs"></a>

## Deps

`opam install calendar ocamlgraph yojson utop core ppx_deriving dune re2`

더 있는거같은데 잘모르겟다 무야홍

## TroubleShooting

### `fatal error: 'cblas.h' file not found`

(인텔맥 기준)

- 이런 경우 일단 `brew reinstall openblas` 를 해 준다음
- `export PKG_CONFIG_PATH="/usr/local/opt/openblas/lib/pkgconfig:$PATH"` 를 해 준다.
