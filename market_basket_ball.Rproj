library(readr)
library(arules)
transaksi_tabular <- read.csv('C:\Users\ARY - PC\OBJECT ORIENTED PROGRAMING\DATA\data_transaksi.txt', sep="\t")
print(transaksi_tabular)
transaksi <- read.transactions(file='C:\Users\ARY - PC\OBJECT ORIENTED PROGRAMING\DATA\data_transaksi.txt', format="single", sep="\t", cols=c(1,2), skip=1)
print(transaksi)

#menampilkan transaksi
transaksi@itemInfo

#menampilkan kode transaksi
transaksi@itemsetInfo

#menampilkan frequensi
data_item <-itemFrequency(transaksi, type="absolute")

#Melakukan sorting pada data_item
data_item <- sort(data_item, decreasing = TRUE)

#Mengambil 3 item pertama
data_item <- data_item[1:3]
#Konversi data_item menjadi data frame dengan kolom Nama_Produk dan Jumlah
data_item <- data.frame("Nama Produk"=names(data_item), "Jumlah"=data_item, row.names=NULL)

print(data_item)

#Menulis File Statistik Top 3
write.csv(data_item, file="top3_item_retail.txt",eol="\r\n")
itemFrequencyPlot(transaksi)
inspect(transaksi)

mba <- apriori(transaksi,parameter = list(supp = 0.1, confidence = 0.5))
inspect(mba)
inspect(subset(mba, lhs %in% "Teh Celup" | rhs %in% "Teh Celup"))
inspect(subset(mba,(lhs %in% "Teh Celup" | rhs %in% "Teh Celup") & lift>1))
inspect(subset(mba,(lhs %ain% c("Pet Food", "Gula"))))
plot(subset(mba, lift>1.1), method="graph")
