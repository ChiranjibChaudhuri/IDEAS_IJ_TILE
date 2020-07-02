library("shiny")

library("DBI")
library("leaflet")
library("dggridR")
library("leafgl")
library("sf")
library("mapview")
library("leafem")
library("colourvalues")
library("dplyr")
library(pracma)
#library(prob)

# library(foreach)
# library(doSNOW)
# library(parallel)
# 
# ncpu=detectCores()
# cl=makeCluster(ncpu-1)
# registerDoSNOW(cl)
# 
# clusterEvalQ(cl, library(sf))

con = dbConnect(RNetezza::Netezza(), dsn='NZSQL_C')
grid=tbl(con,"A_TILE_13")
grid=mutate(grid,WKT=inza..ST_AsText(GEOM))
grid=select(grid,RESOLUTION,DGGID,I,J,NTILE,WKT) 

dggs13 = dgconstruct(res=13, metric=TRUE, resround='nearest', pole_lat_deg = 37,pole_lon_deg =-178)
grid13=filter(grid,RESOLUTION==13)
data=tbl(con,"AVG_MELT_DAYS")%>%mutate(VALUE=ifelse(VALUE>12,12,VALUE))
#data=tbl(con,"TSN")%>%mutate(VALUE=TSN)
data=data%>%inner_join(grid13,by=c("DGGID"="DGGID"))%>%select(NTILE,WKT,VALUE)

mami=data%>%summarise(max=max(VALUE),min=min(VALUE))%>%collect()
vmin=mami$max
vmax=mami$min
list=c(vmin,vmax)
print(list)
numpal = colorNumeric(rev(viridis::viridis(256)), list)


m = leaflet() %>%
  addProviderTiles(provider = providers$Stamen.Terrain) %>% setView(-114.19, 64.19, zoom = 10) 


ui = fluidPage(
  leafletOutput("mymap")
)

server = function(input, output) {
  
  output$mymap = renderLeaflet(m)
  proxy=leafletProxy("mymap")
  RV=reactiveValues(V=list())
  observeEvent(input$mymap_bounds, {
    mapBox = input$mymap_bounds
    north = mapBox[[1]]
    east = mapBox[[2]]
    south = mapBox[[3]]
    west = mapBox[[4]]
  
    res=13
    dggs = dgconstruct(res=res, metric=TRUE, resround='nearest', pole_lat_deg = 37,pole_lon_deg =-178)
    ptsSeq1 = dgGEO_to_Q2DI(dggs, west, south)
    ptsSeq2 = dgGEO_to_Q2DI(dggs, east, north)
    ptsSeq3 = dgGEO_to_Q2DI(dggs, west, north)
    ptsSeq4 = dgGEO_to_Q2DI(dggs, east, south)
    mini=min(ptsSeq1$i, ptsSeq2$i,ptsSeq3$i, ptsSeq4$i)
    maxi=max(ptsSeq1$i, ptsSeq2$i,ptsSeq3$i, ptsSeq4$i)
    minj=min(ptsSeq1$j, ptsSeq2$j,ptsSeq3$j, ptsSeq4$j)
    maxj=max(ptsSeq1$j, ptsSeq2$j,ptsSeq3$j, ptsSeq4$j)
    
    I=seq(mini,maxi,by=1)
    J=seq(minj,maxj,by=1)
    
    IJ=meshgrid(I,J)
    NT=((floor(1831/300)*(as.vector(IJ$Y)-1))+floor(as.vector(IJ$X)/300))
    NT=as.character(unique(NT))
    
    V=RV$V
    VT=setdiff(NT,V)
    RT=setdiff(V,NT)
    RV$V=NT

    #print (RT)
    
    proxy%>%clearControls()%>%clearGroup(.,RT)%>%
      {  
    for (n in VT){
    #foreach (n = NT)%dopar%{
      name=paste("F:/Chiranjib/TILE_TRY/TILE/AVG_MELT_DAYS/",n,".geojson",sep="") 
      if (!file.exists(name)){ 
      dataf=filter(data,NTILE==n)
      chunk=collect(dataf)
      df = st_as_sf(chunk, wkt='WKT', crs = 4326)
      st_write(df, name,quiet=TRUE)
      }    
      df=st_read(name,quiet=TRUE)  
      #list=c(df$VALUE,vmin,vmax)
      #cols = colour_values_rgb(list, palette="blue2red",include_alpha = FALSE) / 255
      #cols=cols[1:(dim(cols)[1]-2),]
      #numpal = colorNumeric(rev(viridis::viridis(256)), list)
      .=addPolygons(.,data = df, color=NA,group=n, fillOpacity = 0.5,fillColor=~numpal(VALUE))
      #print(n)
    }
    return(.)
      }%>%addLegend(data=df,pal = numpal, values = list, opacity = 1)
  })
}

shinyApp(ui, server)

