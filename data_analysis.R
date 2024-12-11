library(readxl)
library(dplyr)
library(openxlsx)

file_path <- "~/Plant_Height_2023-I.xlsx"


data_list <- lapply(sheets, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  df <- df %>%
    mutate(Bloque = as.character(Bloque)) %>%
    mutate(Sheet = sheet)
  return(df)
})

combined_data <- bind_rows(data_list)

# Function to calculate mean and minimum for each sheet
calculate_stats <- function(combined_data) {
  combined_data %>%
    group_by(Bloque) %>%
    summarise(
      mean_value = mean(Altura, na.rm = TRUE),
      min_value = min(Altura, na.rm = TRUE)
    )
}

results <- combined_data %>%
  group_by(Sheet, Bloque) %>%
  summarise(
    mean_value = mean(Altura, na.rm = TRUE),
    min_value = min(Altura, na.rm = TRUE)
  )



##################
##LiDAR analysis
data_lidar8 <- read_excel("C:/Users/dortega/Desktop/data_lidar8.xlsx")
data_lidar3$mean=(data_lidar2$mean)*100
data_lidar3$X95.=(data_lidar2$X95.)*100
data_lidar3$max=(data_lidar2$max)*100



subset_data <- data_lidar8 %>% filter(lidar_file == "LIDAR_6")
cor(subset_data$mean, subset_data$Height_mean)
cor(subset_data$X95., subset_data$Height_mean)
cor(subset_data$max, subset_data$Height_mean)
cor(subset_data$max, subset_data$Height_min)


subset_data1 <- data_lidar8 %>% filter(lidar_file == "LIDAR_8")
cor(subset_data1$mean, subset_data1$Height_mean)
cor(subset_data1$X95., subset_data1$Height_mean)
cor(subset_data1$max, subset_data1$Height_mean)
cor(subset_data1$max, subset_data1$Height_min)


subset_data2 <- data_lidar8 %>% filter(lidar_file == "LIDAR_9")
cor(subset_data2$mean, subset_data2$Height_mean)
cor(subset_data2$X95., subset_data2$Height_mean)
cor(subset_data2$max, subset_data2$Height_mean)
cor(subset_data2$max, subset_data2$Height_min)


subset_data3 <- data_lidar8 %>% filter(lidar_file == "LIDAR_10")
cor(subset_data3$mean, subset_data3$Height_mean)
cor(subset_data3$X95., subset_data3$Height_mean)
cor(subset_data3$max, subset_data3$Height_mean)
cor(subset_data3$max, subset_data3$Height_min)


########################################
###Emissions analysis
########################################

library(dplyr)

summary_emissions <- emissions %>%
  group_by(Fecha, Plot) %>%
  summarize(
    co2_mean = mean(CO2, na.rm = TRUE),
    co2_median = median(CO2, na.rm = TRUE),
    co2_min = min(CO2, na.rm = TRUE),
    co2_max = max(CO2, na.rm = TRUE),
    co2_p5 = quantile(CO2, probs = 0.05, na.rm = TRUE),
    co2_p95 = quantile(CO2, probs = 0.95, na.rm = TRUE),
    
    ch4_mean = mean(CH4, na.rm = TRUE),
    ch4_median = median(CH4, na.rm = TRUE),
    ch4_min = min(CH4, na.rm = TRUE),
    ch4_max = max(CH4, na.rm = TRUE),
    ch4_p5 = quantile(CH4, probs = 0.05, na.rm = TRUE),
    ch4_p95 = quantile(CH4, probs = 0.95, na.rm = TRUE),
    
    n2o_mean = mean(N2O, na.rm = TRUE),
    n2o_median = median(N2O, na.rm = TRUE),
    n2o_min = min(N2O, na.rm = TRUE),
    n2o_max = max(N2O, na.rm = TRUE),
    n2o_p5 = quantile(N2O, probs = 0.05, na.rm = TRUE),
    n2o_p95 = quantile(N2O, probs = 0.95, na.rm = TRUE),
    
    agua_mean = mean(Lamina_agua, na.rm = TRUE),
    agua_median = median(Lamina_agua, na.rm = TRUE),
    agua_min = min(Lamina_agua, na.rm = TRUE),
    agua_max = max(Lamina_agua, na.rm = TRUE),
    agua_p5 = quantile(Lamina_agua, probs = 0.05, na.rm = TRUE),
    agua_p95 = quantile(Lamina_agua, probs = 0.95, na.rm = TRUE),
    
    tratamiento = first(Tratamiento)
    
  )

export(summary_emissions, "C:/Users/dortega/Desktop/summary_emissions.xlsx") 

###########################Analysis###################################################
##Corrleations Emissions####
summary_emissions <- read_excel("C:/Users/dortega/Desktop/summary_emissions.xlsx")
summary_emissions <- summary_emissions %>%
  mutate(Fecha = as.Date(Fecha))

subset_emissions2 <- summary_emissions %>% filter(Fecha == as.Date("2023-02-14"))
subset_emissions2 <- subset_emissions2 %>%
  dplyr::select(-c(Fecha, Plot, tratamiento, Min, Max), -starts_with("agua"), -starts_with("n2o"))

##correlation matrix
library(corrplot)
cor_matrix <- cor(subset_emissions2, method = "spearman", use = "complete.obs")
col <- colorRampPalette(c("blue", "white", "red"))(200)
corrplot(cor_matrix, method = "number", type="lower", cl.pos = "r", number.cex = 0.65,  tl.cex = 0.8)

# Create the heatmap
library(plotly)
library(ggplot2)
cor_text <- formatC(cor_matrix, format = "f", digits = 2)
cor_colors <- matrix("white", nrow = nrow(cor_matrix), ncol = ncol(cor_matrix))  # Default to white
cor_colors[cor_matrix > 0.6] <- "red" 


plot_ly(
  x = colnames(cor_matrix),
  y = rownames(cor_matrix),
  z = cor,
  type = "heatmap",
  text = cor_text,
  colorscale = "Viridis"
) %>% 
  add_annotations(
    x = rep(colnames(cor_matrix), each = nrow(cor_matrix)),
    y = rep(rownames(cor_matrix), times = ncol(cor_matrix)),
    text = cor_text,
    showarrow = FALSE,
    font = list(color = as.vector(cor_colors), size = 8) # Adjust text size as needed
  )

##correlation by variable
cor(subset_emissions2$ch4_mean, subset_emissions2$Q95)
cor(subset_emissions2$ch4_mean, subset_emissions2$Q90)
cor(subset_emissions2$ch4_mean, subset_emissions2$Mean)
cor(subset_emissions2$ch4_mean, subset_emissions2$Median)


cor(subset_emissions2$co2_mean, subset_emissions2$Q90)
cor(subset_emissions2$co2_mean, subset_emissions2$Q95)
cor(subset_emissions2$co2_mean, subset_emissions2$Mean)
cor(subset_emissions2$co2_mean, subset_emissions2$Median)
cor(subset_emissions2$co2_p95, subset_emissions2$Q95)

cor(subset_emissions2$n2o_mean, subset_emissions2$Q95)
cor(subset_emissions2$n2o_mean, subset_emissions2$Q90)
cor(subset_emissions2$n2o_mean, subset_emissions2$Mean)
cor(subset_emissions2$n2o_mean, subset_emissions2$Median)


##plots by time
library(ggplot2)
ggplot(summary_emissions, aes(x = Fecha, y = Q95)) +
  geom_point() +              
  labs(title = "Time Series Intensity p95", x = "Date", y = "CH4/m2/d") +  
  theme_minimal()            

###########################Height analysis######################################

data_lidar7 <- read_excel("C:/Users/dortega/Desktop/data_lidar7.xlsx")
subset_data <- data_lidar7 %>% filter(lidar_file == "LIDAR_6")
subset_data <- subset_data %>%
  dplyr::select(-c(lidar_file, polygon_id, polygon_BEF, yield))

cor_matrix <- cor(subset_data, method = "spearman", use = "complete.obs")
cor_text <- formatC(cor_matrix, format = "f", digits = 2)

my_colorscale <- list(
  list(-1, "blue"),  # Low value
  list(-0.5, "cyan"),  # Mid-low value
  list(0, "white"),  # Midpoint
  list(0.5, "yellow"),  # Mid-high value
  list(1, "red")    # High value
)

plot_ly(
  x = colnames(cor_matrix),
  y = rownames(cor_matrix),
  z = cor_matrix,
  type = "heatmap",
  text = cor_text,
  colorscale = my_colorscale
) %>% 
  add_annotations(
    x = rep(colnames(cor_matrix), each = nrow(cor_matrix)),
    y = rep(rownames(cor_matrix), times = ncol(cor_matrix)),
    text = cor_text,
    showarrow = FALSE,
    font = list(color = "black", size = 8) # Adjust text size as needed
  )


##plots by time real height vs lidar
library(ggplot2)
ggplot(data_lidar7, aes(x = lidar_file, y = Height_mean)) +
  geom_point() +              
  labs(title = "Time Series Intensity p95", x = "Date", y = "CH4/m2/d") +  
  theme_minimal()   

ggplot(data_lidar7, aes(x = X95., y = Height_mean)) +
  geom_point() +              
  labs(title = "Plot real height vs chm", x = "chm (mts)", y = "real height (cms)") +  
  theme_minimal()  


#chm for the 10 measures
data_fechas <- read_excel("C:/Users/dortega/Desktop/data_fechas.xlsx", sheet = "rice_complete")
data_fechas <- data_fechas %>%
  mutate(date = as.Date(date))

ggplot(data_fechas, aes(x = date, y = X95.)) +
  geom_point() +              
  labs(title = "Time Series canopy height model p95", x = "Date", y = "Height (mts)") +  
  theme_minimal() 


ggplot(summary_emissions, aes(x = co2_p95, y = Q75)) +
  geom_point() +              
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +  # Adding linear regression line
  labs(title = "Plot CO2 vs LiDAR Intensity", x = "CO2 (95th Percentile)", y = "Intensity (75th Percentile)") +  
  theme_minimal()

ggplot(subset_emissions2, aes(x = co2_p95, y = Q95)) +
  geom_point() +              
  #geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +  # Adding linear regression line
  labs(title = "Plot CO2 vs LiDAR Intensity", x = "CO2 (95th Percentile)", y = "Intensity (95th Percentile)") +  
  theme_minimal()


##regression model for Yield

## first is the relationship linear or not
ggplot(data_fechas, aes(x = X95., y = yield)) +
  geom_point() +              
  #geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +  # Adding linear regression line
  labs(title = "Plot CO2 vs LiDAR Intensity", x = "CO2 (95th Percentile)", y = "Intensity (95th Percentile)") +  
  theme_minimal()


data_lidar7 <- read_excel("C:/Users/dortega/Desktop/data_lidar7.xlsx")
subset_data <- data_lidar7 %>% filter(lidar_file == "LIDAR_10")


model <- lm(yield ~ X95., data = subset_data)
summary(model)

cor(subset_data$yield, subset_data$mean)
cor(subset_data$yield, subset_data$max)
cor(subset_data$yield, subset_data$X90.)
cor(subset_data$yield, subset_data$X95.)
cor(subset_data$yield, subset_data$Height_mean)


#GAM model
install.packages("mgcv")
library(mgcv)

model <- gam(yield ~ s( X95.), data = subset_data)
summary(model)

##functional data
library(fda)

#first reshape the data
library(tidyr)
library(dplyr)

data_fechas2 =  data_fechas %>%
  dplyr::select(c(polygon_id, X95., yield, lidar_file))

data_fechas_wide = data_fechas2 %>% pivot_wider(names_from = lidar_file, 
                                                values_from = X95.,
                                                names_prefix = "height")
time_points_numeric <- 1:11

height_data <- as.matrix(data_fechas_wide[, -c(1, 2)])
basis <- create.bspline.basis(rangeval = range(time_points_numeric), nbasis = 11) 
height_fd <- Data2fd(argvals = time_points_numeric, y = t(height_data), basisobj = basis)
yield = data_fechas_wide$yield

fit <- fRegress(yield ~ height_fd)
summary(fit)


######################################################################
#############Emission regression analysis############################


#climate data

Data_base_SOC_weather <- read_excel("C:/Users/dortega/Desktop/Data base SOC-weather.xlsx", sheet = "Datos clima C1 Palmira")
Data_base_SOC_weather$Fecha = as.Date(Data_base_SOC_weather$Fecha)

summary_emissions <- read_excel("C:/Users/dortega/Desktop/summary_emissions.xlsx", sheet = "Sheet1")
summary_emissions$Fecha = as.Date(summary_emissions$Fecha)

merged_emissions_clima <- merge(summary_emissions, Data_base_SOC_weather, by = "Fecha", all.x = TRUE)


# vegetation index Data
BEFData_VIs <- read_excel("C:/Users/dortega/Desktop/BEFData_VIs_2023-A.xlsx")
names(BEFData_VIs)

BEFData_VIs2 =  BEFData_VIs %>%
  dplyr::select(c(ID, Plot, Genotype, TIMESTAMP, NDRE_MEDIAN, NDVI_MEDIAN, GNDVI_MEDIAN, BNDVI_MEDIAN, NDREI_MEDIAN,
                  GRVI_MEDIAN, NGBDI_MEDIAN))

BEFData_VIs2$TIMESTAMP <- as.Date(BEFData_VIs2$TIMESTAMP)

BEFData_VIs2Median <- BEFData_VIs2 %>%
  group_by(Plot, TIMESTAMP) %>%
  summarise(
    NDRE_MEDIAN = median(NDRE_MEDIAN, na.rm = TRUE),
    NDVI_MEDIAN = median(NDVI_MEDIAN, na.rm = TRUE),
    GNDVI_MEDIAN = median(GNDVI_MEDIAN, na.rm = TRUE),
    BNDVI_MEDIAN = median(BNDVI_MEDIAN, na.rm = TRUE),
    NDREI_MEDIAN = median(NDREI_MEDIAN, na.rm = TRUE),
    NGRVI_MEDIAN = median(GRVI_MEDIAN, na.rm = TRUE),
    NGBDI_MEDIAN = median(NGBDI_MEDIAN, na.rm = TRUE),
    Genotype = first(Genotype)
  )

BEFData_VIs2Median <- BEFData_VIs2Median %>%
  mutate(TIMESTAMP = ifelse(TIMESTAMP == as.Date("2023-02-13"), as.Date("2023-02-14"), TIMESTAMP),
         TIMESTAMP = ifelse(TIMESTAMP == as.Date("2023-02-20"), as.Date("2023-02-21"), TIMESTAMP),
         TIMESTAMP = ifelse(TIMESTAMP == as.Date("2023-03-06"), as.Date("2023-03-07"), TIMESTAMP),
         TIMESTAMP = ifelse(TIMESTAMP == as.Date("2023-03-15"), as.Date("2023-03-14"), TIMESTAMP),
         TIMESTAMP = ifelse(TIMESTAMP == as.Date("2023-04-10"), as.Date("2023-04-05"), TIMESTAMP),
         TIMESTAMP = ifelse(TIMESTAMP == as.Date("2023-04-25"), as.Date("2023-04-21"), TIMESTAMP),
  )

##merged
names(BEFData_VIs2Median)[names(BEFData_VIs2Median) == "TIMESTAMP"] <- "Fecha"
BEFData_VIs2Median$Fecha <- as.Date(BEFData_VIs2Median$Fecha, origin = "1970-01-01")
merged_emissions_clima_VI <- merge(merged_emissions_clima, BEFData_VIs2Median, by = c("Fecha", "Plot"), all.x = TRUE)

##Soil data
Soil_data <- read_excel("C:/Users/dortega/Desktop/Data base SOC-Soil data.xlsx", sheet = "Linea Base-Qumica S-Palmira")
names(Soil_data)
parcelas <- c(5, 8, 9, 10, 14, 23)
filtered_Soil_data <- subset(Soil_data, Parcela %in% parcelas)

#filtered_Soil_data$Parcela[filtered_Soil_data$Parcela == 5] <- 23
#filtered_Soil_data$Parcela[filtered_Soil_data$Parcela == 16] <- 5
#filtered_Soil_data$Parcela[filtered_Soil_data$Parcela == 15] <- 8
#filtered_Soil_data$Parcela[filtered_Soil_data$Parcela == 10] <- 9
#filtered_Soil_data$Parcela[filtered_Soil_data$Parcela == 7] <- 10
#filtered_Soil_data$Parcela[filtered_Soil_data$Parcela == 6] <- 14

#variables names
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Profundidad (cm)"] <- "Profundidad"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "pH          (Un)"] <- "pH"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "C Oxid (g/kg)"] <- "C_oxid"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "MO        (g/kg)"] <- "MO"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "P-BrayII (mg/kg)"] <- "P-BrayII"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Ca (cmol/kg)"] <- "Ca"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Mg (cmol/kg)"] <- "Mg"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "K     (cmol/kg)"] <- "K"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Na (cmol/kg)"] <- "Na"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "CIC (cmol/kg)"] <- "CIC"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Fe       (mg/kg)"] <- "Fe"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Mn     (mg/kg)"] <- "Mn"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Cu      (mg/kg)"] <- "Cu"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "Zn       (mg/kg)"] <- "Zn"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "B         (mg/kg)"] <- "B"
colnames(filtered_Soil_data)[colnames(filtered_Soil_data) == "S         (mg/kg)"] <- "S"

names(filtered_Soil_data)[names(filtered_Soil_data) == "Parcela"] <- "Plot" 

summary_filtered_Soil_data <- filtered_Soil_data %>%
  group_by(Plot) %>%
  summarize(
    pH = median(pH, na.rm = TRUE),
    C_oxid = median(C_oxid, na.rm = TRUE),
    MO = median(MO, na.rm = TRUE),
    `P-BrayII` = median(`P-BrayII`, na.rm = TRUE),
    Ca = median(Ca, na.rm = TRUE),
    Mg = median(Mg, na.rm = TRUE),
    K = median(K, na.rm = TRUE),
    Na = median(Na, na.rm = TRUE),
    CIC = median(CIC, na.rm = TRUE),
    Fe = median(Fe, na.rm = TRUE),
    Mn = median(Mn, na.rm = TRUE),
    Cu = median(Cu, na.rm = TRUE),
    Zn = median(Zn, na.rm = TRUE),
    B = median(B, na.rm = TRUE),
    S = median(S, na.rm = TRUE),
    Repetecion = first(Repeteción),
    Rango = first(Rango),
    Cultivo = first(Cultivo),
    Genotipo = first(Genotipo),
    Profundidad = first(Profundidad)
  )


merged_emissions_clima_FINAL <- merge(merged_emissions_clima_VI, summary_filtered_Soil_data, by = "Plot", all.x = TRUE)


#Varuable names weather
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "T° max (°C)"] <- "Tmax"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "T° min (°C)"] <- "Tmin"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "Humedad relativa (%)"] <- "Humedad"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "Evaporación total (mm)"] <- "Evapora"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "Radiación total"] <- "radia"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "Precipitación total (mm)"] <- "prec"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "Tensión de vapor"] <- "Ten_vapor"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "Velocidad (Km/h)"] <- "Vel"
colnames(merged_emissions_clima_FINAL)[colnames(merged_emissions_clima_FINAL) == "P-BrayII"] <- "PBray"


names(merged_emissions_clima_FINAL)
library(rio)
export(merged_emissions_clima_FINAL, "C:/Users/dortega/Desktop/merged_emissions_clima_FINAL.xlsx")

#####################
###Correlations

merged_emissions_clima_FINAL2 <- merged_emissions_clima_FINAL %>%
  dplyr::select(-c(Fecha, Plot, Estado, Genotype, Estado, tratamiento, Repetecion, Rango, Cultivo, Genotipo, Profundidad))

cor_matrix <- cor(merged_emissions_clima_FINAL2, use = "complete.obs")

##emissions

emissions <- merged_emissions_clima_FINAL %>%
  dplyr::select(starts_with("co2"), starts_with("ch4"), c(Mean, Median, Std_Dev), 
                 starts_with("Q"))
library(corrplot)
cor_matrix <- cor(emissions, method = "spearman", use = "complete.obs")
col <- colorRampPalette(c("navy", "purple", "lightblue"))(200)
corrplot(cor_matrix, method = "number", type="lower", cl.pos = "r", number.cex = 0.6,  tl.cex = 0.8, col = col)


##climate

climate <- merged_emissions_clima_FINAL %>%
  dplyr::select(c(co2_median, Mean, Median), starts_with("Q"), c(Tmax, Tmin, Humedad, Evapora, radia, prec, Ten_vapor, Vel))

cor_matrix <- cor(climate, method = "spearman", use = "complete.obs")

col <- colorRampPalette(c("blue", "green", "red"))(200)
corrplot(cor_matrix, method = "number", type="lower", cl.pos = "r", number.cex = 0.6,  tl.cex = 0.8, col = col)


##Vegetation index

VI <- merged_emissions_clima_FINAL %>%
  dplyr::select(c(co2_median, Mean, Median), starts_with("Q"), c(NDRE_MEDIAN, NDVI_MEDIAN, GNDVI_MEDIAN, BNDVI_MEDIAN, NDREI_MEDIAN,
                                                                 NGRVI_MEDIAN, NGBDI_MEDIAN))

library(corrplot)
cor_matrix <- cor(VI, method = "spearman", use = "complete.obs")
col <- colorRampPalette(c("navy", "purple", "lightblue"))(200)
corrplot(cor_matrix, method = "number", type="lower", cl.pos = "r", number.cex = 0.6,  tl.cex = 0.8, col = col)


#SOil Chemical conditions

soil <- merged_emissions_clima_FINAL %>%
  dplyr::select(c(co2_median, Mean, Median), starts_with("Q"), c(pH, C_oxid, MO, PBray, Ca, Mg, K, Na, CIC, Fe, Mn, Cu, Zn, B, S))

cor_matrix <- cor(soil, method = "spearman", use = "complete.obs")

col <- colorRampPalette(c("navy", "purple", "lightblue"))(200)
corrplot(cor_matrix, method = "number", type="lower", cl.pos = "r", number.cex = 0.5,  tl.cex = 0.8, col = col)


###########################
#Random forest model CO2
###########################

#install.packages("randomForest")
library(randomForest) 
library(caret)
library(readxl)
library(dplyr)

set.seed(123)

merged_emissions_clima_FINAL <- read_excel("C:/Users/dortega/Desktop/merged_emissions_clima_FINAL.xlsx")

merged_emissions_clima_FINAL_2 <- merged_emissions_clima_FINAL %>%
  dplyr::select(c(Fecha, tratamiento, Estado, Genotipo), c(co2_median, Mean, Median), starts_with("Q"), c(Tmax, Evapora, radia, Vel), 
                c(NDRE_MEDIAN, GNDVI_MEDIAN, NGRVI_MEDIAN), c(Mg, Na, S, PBray))

colnames(merged_emissions_clima_FINAL_2)[colnames(merged_emissions_clima_FINAL_2) == "Fecha"] <- "Time"
colnames(merged_emissions_clima_FINAL_2)[colnames(merged_emissions_clima_FINAL_2) == "tratamiento"] <- "Treatment"
colnames(merged_emissions_clima_FINAL_2)[colnames(merged_emissions_clima_FINAL_2) == "Estado"] <- "State"
colnames(merged_emissions_clima_FINAL_2)[colnames(merged_emissions_clima_FINAL_2) == "Genotipo"] <- "Genotype"


randomSample <- createDataPartition(merged_emissions_clima_FINAL_2$co2_median, p=0.7, list=FALSE)
training <- merged_emissions_clima_FINAL_2[randomSample,] 
testing <- merged_emissions_clima_FINAL_2[-randomSample,] 

rf_model <- randomForest(co2_median ~ ., data = training,  importance = TRUE)

##MSE
predictions <- predict(rf_model, testing)
mse <- mean((predictions - testing$co2_median)^2)
print(mse)

#MAE
mae <- mean(abs(predictions - testing$co2_median))
print(mae)

#R square
rss <- sum((testing$co2_median - predictions)^2)  # Residual sum of squares
tss <- sum((testing$co2_median - mean(testing$co2_median))^2)  # Total sum of squares
r_squared <- 1 - (rss/tss)
print(r_squared)

#MAPE
mape <- mean(abs((testing$co2_median - predictions) / testing$co2_median)) * 100
print(mape)

#Importance values
importance_values <- importance(rf_model)
print(importance_values)
varImpPlot(rf_model)

importance_matrix <- importance(rf_model)
incMSE_values <- importance_matrix[, "%IncMSE"]

library(ggplot2)

# Convert to a data frame for ggplot
importance_df <- data.frame(Variables = rownames(importance_matrix),
                            IncMSE = incMSE_values)

# bar plot
ggplot(importance_df, aes(x = reorder(Variables, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Variable Importance - %IncMSE", 
       x = "Variables", 
       y = "% Increase in MSE") +
  theme_minimal()



###########################
#Random forest model CH4
###########################

#install.packages("randomForest")
library(randomForest) 
library(caret)
library(readxl)
library(dplyr)

set.seed(123)

merged_emissions_clima_FINAL <- read_excel("C:/Users/dortega/Desktop/merged_emissions_clima_FINAL.xlsx")

merged_emissions_clima_FINAL_m2 <- merged_emissions_clima_FINAL %>%
  dplyr::select(c(Fecha, tratamiento, Estado, Genotipo), c(ch4_median, Mean, Median), starts_with("Q"), c(Evapora, prec), 
                c(NDRE_MEDIAN, GNDVI_MEDIAN, NGRVI_MEDIAN, NGBDI_MEDIAN), c(C_oxid, MO, PBray, Ca, Na, CIC))

colnames(merged_emissions_clima_FINAL_m2)[colnames(merged_emissions_clima_FINAL_m2) == "Fecha"] <- "Time"
colnames(merged_emissions_clima_FINAL_m2)[colnames(merged_emissions_clima_FINAL_m2) == "tratamiento"] <- "Treatment"
colnames(merged_emissions_clima_FINAL_m2)[colnames(merged_emissions_clima_FINAL_m2) == "Estado"] <- "State"
colnames(merged_emissions_clima_FINAL_m2)[colnames(merged_emissions_clima_FINAL_m2) == "Genotipo"] <- "Genotype"


randomSample2 <- createDataPartition(merged_emissions_clima_FINAL_m2$ch4_median, p=0.7, list=FALSE)
training2 <- merged_emissions_clima_FINAL_m2[randomSample2,] 
testing2 <- merged_emissions_clima_FINAL_m2[-randomSample2,] 

rf_model2 <- randomForest(ch4_median ~ ., data = training2,  importance = TRUE)


##MSE
predictions2 <- predict(rf_model2, testing2)
mse <- mean((predictions2 - testing2$ch4_median)^2)
print(mse)

#MAE
mae <- mean(abs(predictions2 - testing2$ch4_median))
print(mae)

#R square
rss <- sum((testing2$ch4_median - predictions2)^2)  # Residual sum of squares
tss <- sum((testing2$ch4_median - mean(testing2$ch4_median))^2)  # Total sum of squares
r_squared <- 1 - (rss/tss)
print(r_squared)

#MAPE
mape <- mean(abs((testing2$ch4_median - predictions2) / testing2$ch4_median)) * 100
print(mape)

#Importance values
importance_values <- importance(rf_model2)
print(importance_values)
varImpPlot(rf_model2)

importance_matrix <- importance(rf_model2)
incMSE_values <- importance_matrix[, "%IncMSE"]

library(ggplot2)

# Convert to a data frame for ggplot
importance_df <- data.frame(Variables = rownames(importance_matrix),
                            IncMSE = incMSE_values)

# bar plot
ggplot(importance_df, aes(x = reorder(Variables, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  
  labs(title = "Variable Importance - %IncMSE", 
       x = "Variables", 
       y = "% Increase in MSE") +
  theme_minimal()



