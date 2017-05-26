(TeX-add-style-hook
 "Main"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("report" "a4paper")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("scrextend" "fontsize=13pt") ("vietnam" "utf8") ("geometry" "a4paper") ("placeins" "section") ("caption" "justification=centering") ("hyperref" "colorlinks=true" "citecolor=blue" "linkcolor=blue") ("hypcap" "all") ("biblatex" "backend=bibtex" "bibstyle=luanan" "sorting=ydnt" "style=authoryear")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "./Cover/front-cover"
    "./Ch1/credit-risk"
    "./Ch4/closing"
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
    "fig:maxim_margin_example"
    "fig:svm_kernel_example"
    "fig:corr_mat"
    "fig:pca"
    "fig:lasso_coef"
    "fig:lasso_cv"
    "tab:lasso_final"
    "fig:svm_train"
    "fig:lasso_dist"
    "fig:lasso_roc"
    "fig:svm_confution_mat"
    "tab:svm_cm"
    "fig:lasso_confusion_mat")
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

