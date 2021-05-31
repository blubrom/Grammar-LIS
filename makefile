CORE=focus.cmo syntax.cmo lis.cmo
WEBAPP=jsutils.cmo webapp.cmo html.cmo widget_focus.cmo widget_suggestions.cmo widget_table.cmo widget_commandline.cmo
GRAMMAR=grammar.cmo grammar_focus.cmo grammar_lis.cmo grammar_syntax.cmo

FLAGS= -package yojson,ppx_deriving_yojson,csv,js_of_ocaml,js_of_ocaml-ppx -I ../fablis/core -I ../fablis/core/webapp

all: $(GRAMMAR) grammar_webapp.ml
	ocamlfind ocamlc $(FLAGS) -package js_of_ocaml,js_of_ocaml-lwt -linkpkg -o html/script.byte $(CORE) $(WEBAPP) $(GRAMMAR) grammar_webapp.ml
	js_of_ocaml html/script.byte

clean:
	rm -f *.cm[ioax]

.SUFFIXES: .ml .mli .cmo .cmi

%.cmo: %.ml
	ocamlfind ocamlc -c $(FLAGS) $<