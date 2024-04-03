setwd("~/Desktop/BA Year 2 Sem 2/BC2407/Project Docs")
Competitor <- read.csv('Expedia.csv')

# Data cleaning ----------------------------------------------------------------------------------------------------------------------
# Competitor 1 
Comp1 <- subset(Competitor, select = c(comp1_rate))

# NULL value 
Comp1[Comp1 == "NULL"] <- NA

Comp1 <-na.omit(Comp1) 

summary(Comp1)

# Generate the table
counts <- table(Comp1)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
# comp1_rate.         
#-1     0     1 
#12399 54011 88654 

#Expedia gave more discount
#----------------------------------------------------------------------------------------------------------------------
# Competitor 2
Comp2 <- subset(Competitor, select = c(comp2_rate))

# NULL value 
Comp2[Comp2 == "NULL"] <- NA

Comp2 <-na.omit(Comp2) 

# Generate the table
counts <- table(Comp2)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)

#comp2_rate
#-1       0       1 
#214518 2149965  335169 

#Expedia gave more discount

#----------------------------------------------------------------------------------------------------------------------
# Competitor 3
Comp3 <- subset(Competitor, select = c(comp3_rate))

# NULL value 
Comp3[Comp3 == "NULL"] <- NA

Comp3 <-na.omit(Comp3) 

# Generate the table
counts <- table(Comp3)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
#comp3_rate
#-1       0       1 
#195437 1605520  231442 

# Expedia gave more discount


#----------------------------------------------------------------------------------------------------------------------
# Competitor 4
Comp4 <- subset(Competitor, select = c(comp4_rate))

# NULL value 
Comp4[Comp4 == "NULL"] <- NA

Comp4 <-na.omit(Comp4) 

# Generate the table
counts <- table(Comp4)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
#comp4_rate
#-1      0      1 
#73592 278081  66113

#  Competitor 4 gave more discount

#----------------------------------------------------------------------------------------------------------------------
# Competitor 5
Comp5 <- subset(Competitor, select = c(comp5_rate))

# NULL value 
Comp5[Comp5 == "NULL"] <- NA

Comp5 <-na.omit(Comp5) 

# Generate the table
counts <- table(Comp5)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
#comp5_rate
#-1       0       1 
#371413 2165285  439328 

# Expedia gave more discount

#----------------------------------------------------------------------------------------------------------------------
# Competitor 6
Comp6 <- subset(Competitor, select = c(comp6_rate))

# NULL value 
Comp6[Comp6 == "NULL"] <- NA

Comp6 <-na.omit(Comp6) 

# Generate the table
counts <- table(Comp6)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
#comp6_rate
#-1      0      1 
#32726 216188  75719 

#Expedia gave more discount

#----------------------------------------------------------------------------------------------------------------------
# Competitor 7
Comp7 <- subset(Competitor, select = c(comp7_rate))

# NULL value 
Comp7[Comp7 == "NULL"] <- NA

Comp7 <-na.omit(Comp7) 

# Generate the table
counts <- table(Comp7)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
#comp7_rate
#-1      0      1 
#44791 273631 108168

# Expedia gave more discount

#----------------------------------------------------------------------------------------------------------------------
# Competitor 8
Comp8 <- subset(Competitor, select = c(comp8_rate))

# NULL value 
Comp8[Comp8 == "NULL"] <- NA

Comp8 <-na.omit(Comp8) 

# Generate the table
counts <- table(Comp8)

# -1 = higher price than comp1, 0 = same price, 1 = lower price than comp1
print(counts)
#comp8_rate
#-1       0       1 
#362085 1974286  208824

# Competitor 8 gave more discount 
