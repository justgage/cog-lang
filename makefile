main: 
	pandoc -f markdown_github --number-sections preposal.md | pandoc -f html --reference-odt=reference.odt -t odt -o gages-senior-project-proposal.odt

ocaml:
	ocamlbuild.native tasks.ml

table:
	rm requirements.html
	clojure convert-reqs.clj > requirements.html
view: 
	firefox requirements.html
