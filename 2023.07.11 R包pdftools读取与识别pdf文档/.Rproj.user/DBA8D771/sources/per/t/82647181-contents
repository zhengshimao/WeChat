#install.packages("pdftools")
#install.packages("tesseract")
# install.packages("webp")
library(magrittr)

library(pdftools)
library(tesseract)
download.file("http://arxiv.org/pdf/1403.2805.pdf", "1403.2805.pdf", mode = "wb")

# 读取pdf文本内容
txt <- pdf_text("1403.2805.pdf")

class(txt)
length(txt) # 每一页内容是一个元素

# first page text
cat(txt[1]) # 用cat可以将“\n”转为回车。

# second page text
cat(txt[2])
# 
# Table of contents
toc <- pdf_toc("1403.2805.pdf")
class(toc)
length(toc)
toc$title
toc$children

# Show as JSON # 将目录转为json格式
toc2 <- jsonlite::toJSON(toc, auto_unbox = TRUE, pretty = TRUE)

# 提取pdf其他信息
## 作者版本等信息
info <- pdf_info("1403.2805.pdf")
info$version # 版本
info$pages #页数
info$encrypted
info$keys
info$created
info$modified
# info$metadata
info$locked
info$attachments
info$layout
## 字体
fonts <- pdf_fonts("1403.2805.pdf")
fonts$name
fonts$type
fonts$embedded
fonts$file
## 
files <- pdf_attachments("1403.2805.pdf")
#########################################################
# pdf渲染
# renders pdf to bitmap array
bitmap <- pdf_render_page("1403.2805.pdf", page = 1,dpi = 600)
class(bitmap)
## 将第一页转化为图片
# save bitmap image
png::writePNG(bitmap, "page.png")
webp::write_webp(bitmap, "page.webp")

## 表格
if(F){
  # 也可以使用tabulizer包提取pdf中的表格，但它需要以来rjava。而tabulizer则不依赖于rjava
  # install.packages("tabulizer") # 表格所用依赖安装
  library(tabulizer)
}
txt <- pdf_text("http://arxiv.org/pdf/1406.4806.pdf")

# some tables
cat(txt[18])
cat(txt[19])

## 光学识别OCR

# 依赖于R包tesseract的Tesseract OCR引擎
## 该功能普遍与pdf中的图片质量有关，所以不要过多期待。
ocr_txt <- pdf_ocr_text(pdf = "page_ocr.pdf", pages = 1, language = "eng", dpi = 600)
class(ocr_txt)
cat(ocr_txt) # 查看转换结果

ocr_data <- pdf_ocr_data(pdf = "page_ocr.pdf", pages = 1, language = "eng", dpi = 600)
class(ocr_data)
ocr_data[[1]]
ocr_data[[1]]$word[1:10]
ocr_data[[1]]$confidence %>% summary() # 可信度分布
