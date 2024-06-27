# Load necessary libraries
library(pROC)

# Read the data DATA MODIFIKASI FRAGMENTASI LAHAN MENJADI 0
data_smg <- read.table(text = "
Kelurahan Kode Kemarau Hujan
Cisaranten_Kulon	0	0.592657429533	0.548266271558
Ciumbuleuit	0	0.565610910298	0.584427749331
Lagadar	0	0.506970341091	0.544719508867
Mandalajati	1	0.508246308709	0.517123646243
Nanjung	0	0.540551235908	0.582804248048
Palasari	0	0.502049492188	0.508048150193
Pasirjati	0	0.524570335566	0.517981160638
Sayang	1	0.537901209889	0.521587106451
Sukapura	0	0.526346855335	0.538537322067
Babakan_Ciparay	1	0.387303491558	0.426656335347
Babakanasih	1	0.296797290408	0.344294927996
Babakansari	1	0.402867274511	0.407041552413
Cibadak	1	0.31622671886	0.357484561385
Cibuntu	1	0.362870480738	0.398815282745
Cihaurgeulis	1	0.392058158155	0.421709465843
Cisurupan	1	0.558060239755	0.566183890494
Citarum	1	0.4797738189	0.495239245972
Jatisari	1	0.52376553665	0.494248102537
Kebonjeruk	1	0.348985415957	0.390165258495
Kebonpisang	1	0.368278406527	0.397163956209
Kebonwaru	0	0.39134858897	0.417691300596
Lebakgede	1	0.466756248841	0.482559067398
Malabar	1	0.429800189992	0.450082967943
Mekarmulya	1	0.483166177122	0.490484226843
Mengger	1	0.523341813692	0.538664981566
Pasawahan	1	0.342629169896	0.375659666696
Sadangserang	1	0.354452521267	0.388915940398
Sukagalih	1	0.4888155486	0.511299410377
Sukamaju	0	0.390295654875	0.419210252036
Turangga	1	0.42311592959	0.439601608726
", header = TRUE)

# Split the data into training and testing sets
set.seed(99)
trainIndex_smg <- sample(1:nrow(data_smg), 0.7 * nrow(data_smg))
trainData_smg <- data_smg[trainIndex_smg, ]
testData_smg <- data_smg[-trainIndex_smg, ]

# Perform ROC AUC analysis
roc_kemarau_smg <- roc(trainData_smg$Kode, trainData_smg$Kemarau)
roc_hujan_smg <- roc(trainData_smg$Kode, trainData_smg$Hujan)

testData_smg

# Plot ROC curves
plot(roc_kemarau_smg, col = "blue", main = "ROC Curve Analysis", legacy.axes = TRUE)
plot(roc_hujan_smg, col = "red", add = TRUE, legacy.axes = TRUE)

# Display cutoff values for "Kemarau"
coords(roc_kemarau_smg, "best", ret = c("threshold", "accuracy"))

# Display cutoff values for "Hujan"
coords(roc_hujan_smg, "best", ret = c("threshold", "accuracy"))

roc_kemarau_smg
roc_hujan_smg




