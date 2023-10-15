library(vroom)

command <- "axel -n 20 https://ftp.ensemblgenomes.ebi.ac.uk/pub/plants/release-57/fasta/arabidopsis_thaliana/dna/Arabidopsis_thaliana.TAIR10.dna.toplevel.fa.gz"
system(command = command)

ath_file <- "Arabidopsis_thaliana.TAIR10.dna.toplevel.fa.gz"

file.size(ath_file)
# [1] 36462703 # 35M左右

ath <- vroom_lines(file = ath_file)
class(ath)
# 未压缩
vroom_write_lines(ath, file = "ath.fa", eol = "\n")
file.size("ath.fa")
# [1] 121662600  # 116M左右

# 压缩
vroom_write_lines(ath, file = "ath.fa.gz", eol = "\n")
file.size("ath.fa.gz")

# [1] 36461631 # 35M左右




