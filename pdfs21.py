#Import required functions
from PyPDF2 import PdfFileMerger, PdfFileReader
import os

#Create list from directory of pdfs
pdfList = []
for root, subdirs, files in os.walk('_output/'):
  for myfile in files:
    if os.path.splitext(myfile)[1].endswith('.pdf'):
      pdfList.append(os.path.join(root, myfile))

#Check list
print(pdfList)

#Merge pdfs
merger = PdfFileMerger()
for filename in pdfList:
    merger.append(PdfFileReader(file(filename.strip(), 'rb')))

#Save file
merger.write("allReports.pdf")

#Better alternative
#brew install poppler
#pdfunite *.pdf all.pdf
