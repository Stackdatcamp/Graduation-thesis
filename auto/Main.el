(TeX-add-style-hook
 "Main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("report" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("scrextend" "fontsize=13pt") ("vietnam" "utf8") ("geometry" "a4paper") ("placeins" "section") ("caption" "justification=centering") ("hyperref" "colorlinks=true" "citecolor=blue" "linkcolor=blue") ("hypcap" "all") ("biblatex" "backend=bibtex" "bibstyle=luanan" "sorting=ydnt" "style=authoryear")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "./Cover/front-cover"
    "./Ch1/credit-risk"
    "report"
    "rep10"
    "scrextend"
    "vietnam"
    "verbatim"
    "listings"
    "afterpage"
    "xcolor"
    "graphicx"
    "enumitem"
    "indentfirst"
    "mathptmx"
    "geometry"
    "etoolbox"
    "fancyhdr"
    "titlesec"
    "titletoc"
    "placeins"
    "caption"
    "hyperref"
    "hypcap"
    "biblatex")
   (TeX-add-symbols
    "Title"
    "Author")
   (LaTeX-add-bibliographies
    "reference"))
 :latex)

