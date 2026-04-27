(lang dune 3.21)

(name lokto_cbor)

(generate_opam_files true)

(package
 (name lokto_cbor)
 (depends
  ocaml
  (ppx_deriving :with-test)
  (alcotest :with-test)
  (hex :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)))
