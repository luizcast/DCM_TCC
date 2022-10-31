library(oro.dicom)
# no terminal:
#find ~/Documents/1.2.840.113704.1.111.3180.1469042540.1/* -name '*.dcm' -exec dcmdjpeg {} {} \;
#find ~/Documents/1.2.840.113704.1.111.3180.1469042540.1/* -size +540k -exec rm -r {} {} \;
#find ~/Documents/1.2.840.113704.1.111.3180.1469042540.1/* -size -100k -exec rm -r {} {} \;

readDICOM("~/Documents/1.2.840.113704.1.111.3180.1469042540.1/", verbose = TRUE) -> exemplo_dicom
bench::bench_time(readDICOM("~/Documents/1.2.840.113704.1.111.3180.1469042540.1/", verbose = TRUE))
exemplo_dicom$hdr[1]

bench::bench_time(readDICOMFile("~/Documents/1.2.840.113704.1.111.3180.1469042540.1/1.2.840.113704.1.111.39856.1469042983.3220.dcm"))

paste(round(c(41*165794/630)), "segundos") # tempo total de processamento da base com 165794.

