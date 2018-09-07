#!/bin/sh

ocamlbuild -use-ocamlfind -package pcre -package batteries main.native
