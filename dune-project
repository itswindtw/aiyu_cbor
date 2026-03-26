(lang dune 3.21)

(name aiyu_cbor)

(generate_opam_files true)

(package
 (name aiyu_cbor)
 (depends
  ocaml
  (ppx_deriving :with-test)
  (alcotest :with-test)
  (hex :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
