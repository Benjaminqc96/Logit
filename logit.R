require(readxl)
########################################read the excel sheet###########################################
base<-read_excel("C://Users//lenovo//Documents//ARF veronica//TABLA-AR.xlsx")
#######################################transform to numerical##########################################
base$FRAC<-as.numeric(base$FRAC)
base$R1<-as.numeric(base$R1)
base$R2<-as.numeric(base$R2)
base$R3<-as.numeric(base$R3)
base$R4<-as.numeric(base$R4)
base$R5<-as.numeric(base$R5)
base$R6<-as.numeric(base$R6)
base$R7<-as.numeric(base$R7)
base$R8<-as.numeric(base$R8)
base$R9<-as.numeric(base$R9)
base$R10<-as.numeric(base$R10)
base$R11<-as.numeric(base$R11)
base$R12<-as.numeric(base$R12)
base$R13<-as.numeric(base$R13)
base$R14<-as.numeric(base$R14)
base$R15<-as.numeric(base$R15)
base$R16<-as.numeric(base$R16)
base$R17<-as.numeric(base$R17)
base$R18<-as.numeric(base$R18)
base$R19<-as.numeric(base$R19)
base$R20<-as.numeric(base$R20)
base$R21<-as.numeric(base$R21)
base$R22<-as.numeric(base$R22)
base$R23<-as.numeric(base$R23)
base$R24<-as.numeric(base$R24)
#################################omit the void values#######################################
subbase<-na.omit(base)
y<-subbase$FRAC
x<-cbind(subbase$R1,subbase$R2,subbase$R3,subbase$R4,subbase$R5,subbase$R6,subbase$R7,
         subbase$R8,subbase$R9,subbase$R10,subbase$R11,subbase$R12,subbase$R13,subbase$R14,
         subbase$R15,subbase$R16,subbase$R17,subbase$R18,subbase$R19,subbase$R20,subbase$R21,
         subbase$R22,subbase$R23,subbase$R24)
logitmod<-glm(y~x,family = binomial(link = "logit"),maxit=10,trace=T)

logitmod<-glm(y~subbase$R1+subbase$R2+subbase$R3,family = binomial(link = "logit"))


modlin<-lm(y~x)
