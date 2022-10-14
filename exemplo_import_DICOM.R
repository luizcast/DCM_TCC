library(oro.dicom)
# no terminal:
#find ~/Documents/1.2.840.113704.1.111.3180.1469042540.1/* -name '*.dcm' -exec dcmdjpeg {} {} \;
#find ~/Documents/1.2.840.113704.1.111.3180.1469042540.1/* -size +540k -exec rm -r {} {} \;
#find ~/Documents/1.2.840.113704.1.111.3180.1469042540.1/* -size -100k -exec rm -r {} {} \;

readDICOM("~/Documents/1.2.840.113704.1.111.3180.1469042540.1/", verbose = TRUE) -> exemplo_dicom
bench::bench_time(readDICOM("~/Documents/1.2.840.113704.1.111.3180.1469042540.1/", verbose = TRUE))
exemplo_dicom$hdr[1]
