#
# ~/.latexmkrc
#

$pdflatex = 'xelatex --shell-escape %O %S';
$pdf_mode = 1;
$dvi_mode = $postscript_mode = 0;
$bibtex_use = 1;