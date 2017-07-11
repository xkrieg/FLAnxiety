from PyPDF2 import PdfFileMerger, PdfFileReader

pdfList = []
for root, subdirs, files in os.walk('_output/'):
  for file in files:
    if os.path.splitext(file)[1].lower() in ('.pdf'):
      pdfList.append(os.path.join(root, file))

merger = PdfFileMerger()
for filename in pdfList:
    merger.append(PdfFileReader(file(filename, 'rb')))

merger.write("allReports.pdf")
