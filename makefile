table:
	rm requirements.html
	clojure convert-reqs.clj > requirements.html

ocaml:
	ocamlbuild.native tasks.ml

proposal: 
	pandoc -f markdown_github --number-sections proposal.md | pandoc -f html --reference-odt=reference.odt -t odt -o gages-senior-project-proposal.odt

view: 
	firefox requirements.html
all: proposal table
