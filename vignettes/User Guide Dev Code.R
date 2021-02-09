
library(SLICK)
library(DiagrammeR)


# level 1
#Ds<-"graph LR"
Ds<-"digraph rmarkdown {"
j=0; rec<-list()

nlevs1<-length(SLICKobj)
L1c<-letters[1:nlevs1]
L1n<-names(SLICKobj)

for(L1 in 1:nlevs1){
  j<-j+1; rec[[j]]<-L1c[L1]

  nlevs2<-length(SLICKobj[[L1]])
  L2c<-paste0(L1c[L1],"_",1:nlevs2)
  L2n<-names(SLICKobj[[L1]])

  if(nlevs2>1){
    for(L2 in 1:nlevs2){
      j<-j+1; rec[[j]]<-L2c[L2]
      Ds<-paste0(Ds," ",L1c[L1],"[",L1n[L1],"]-->",L2c[L2],"[",L2n[L2],"]")
    }

  }

}

Ds<-paste0(Ds,"}")

DiagrammeR::grViz(Ds,height=1000)
DiagrammeR(Ds)

DiagrammeR("graph LR;A(Rounded)-->B[Squared];B-->C{A Decision};
 C-->D[Square One];C-->E[Square Two];
 style A fill:#E5E25F;  style B fill:#87AB51; style C fill:#3C8937;
 style D fill:#23772C;  style E fill:#B6E6E6;"
)


obj<-SLICKobj[[1]]


# level 1
#Ds<-"graph LR"

make2lev<-function(obj){
  Ds<-"digraph rmarkdown {
   graph[layout = dot,
         overlap = T,
         outputorder = edgesfirst]
   node[shape=rectangle, style='filled',
   fillcolor='lightblue']
  "
  j=0; rec<-list()

  nlevs1<-length(obj)
  L1c<-letters[1:nlevs1]
  L1n<-names(obj)

  for(L1 in 1:nlevs1){
    j<-j+1; rec[[j]]<-L1c[L1]

    nlevs2<-length(obj[[L1]])
    L2c<-paste0(L1c[L1],"_",1:nlevs2)
    L2n<-names(obj[[L1]])

    if(nlevs2>1){
      for(L2 in 1:nlevs2){
        j<-j+1; rec[[j]]<-L2c[L2]
        Ds<-paste0(Ds," ",L1n[L1], " -> ", L2n[L2])
      }

    }

  }

  Ds<-paste0(Ds,"}")
  DiagrammeR::grViz(Ds,height=500)
}
