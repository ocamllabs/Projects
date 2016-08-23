#!/bin/sh
ocamlbuild -use-ocamlfind list_issues.native 
./list_issues.native -c private ocamllabs/Projects
