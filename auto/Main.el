(TeX-add-style-hook
 "Main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("report" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("scrextend" "fontsize=13pt") ("vietnam" "utf8") ("geometry" "a4paper") ("placeins" "section") ("caption" "justification=centering") ("hyperref" "colorlinks=true" "citecolor=blue" "linkcolor=blue") ("hypcap" "all") ("biblatex" "backend=bibtex" "bibstyle=luanan" "sorting=ydnt" "style=authoryear")))
   (add-to-list 'LaTeX-verbatim-environments-local "alltt")
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
    "./Ch1/credit-risk"
    "report"
    "rep10"
    "graphicx"
    "color"
    "framed"
    "amsmath"
    "alltt"
    "scrextend"
    "vietnam"
    "verbatim"
    "listings"
    "afterpage"
    "xcolor"
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
    "biblatex"
    "upquote")
   (TeX-add-symbols
    '("hlkwd" 1)
    '("hlkwc" 1)
    '("hlkwb" 1)
    '("hlkwa" 1)
    '("hlstd" 1)
    '("hlopt" 1)
    '("hlcom" 1)
    '("hlstr" 1)
    '("hlnum" 1)
    "maxwidth"
    "hlipl"
    "FrameCommand"
    "Title"
    "Author"
    "clearpage")
   (LaTeX-add-labels
    "fig:lasso_coef"
    "fig:lasso_cv"
    "tab:lasso_final"
    "fig:svm_train"
    "fig:svm_confution_mat"
    "tab:lasso_cm_overall"
    "tab:lasso_cm_byclass")
   (LaTeX-add-environments
    "kframe"
    "knitrout")
   (LaTeX-add-bibliographies
    "reference")
   (LaTeX-add-xcolor-definecolors
    "fgcolor"
    "shadecolor"
    "messagecolor"
    "warningcolor"
    "errorcolor"))
 :latex)

