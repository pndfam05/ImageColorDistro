-# ImageColorDistro
Match identical and similar images

This is not a hash matching approach.

This code uses statistical correlation to match identical and similar images across and between the three most common internet formats: .jpg, .bmp and .png

The match is based upon pixel colors rather than the bits and bytes of the image file. It is immune to image orientation differences, metadata differences, and is robust against image resizing.
#######
2016-09-30
Sorry it took me so long to get the code up on GitHub. The .png feature stopped working as intended. I've been diagnosing the cause but have yet to find a solution. Since this is a work in progress, a prototype, I thought I'd put out what I've got so far.

Test images are too large to upload to GitHub

This is written in R code. R is an incredibly powerful statistical analysis program available to all. R can be downloaded from [https://www.r-project.org/]. R is not image processing software. It's being used here for purposes not otherwise intended. So expect some pain and slow performance. 

You'll find R very difficult to use unless you also install R Studio. It is available from [https://www.rstudio.com/products/rstudio/download/]. Even with the two programs downloaded and installed, R is not an easy language to get started with.

Brian R Deering

Cyber Crime Analyst

805.889.1676

pndfam05@gmail.com (preferred)
