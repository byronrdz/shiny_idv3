library(shiny)
library(dplyr)
library(RPostgreSQL)
library(googleVis)
library(ggplot2)
con <- dbConnect(dbDriver("PostgreSQL"),user="postgres",password="XXXXXXX",dbname="XXXXXXX",host="XX.XX.XX.XX")

indicador<- dbGetQuery(con,'
select i.anio as año,i.provincia,i.parroquia,i.c_codigo,i.codigo,p.poblacion,round(i.idv3::numeric,4)as idv3,round(v.promedio::numeric,4) as avg_provincia
from
(
                       SELECT 
                       dim_tiempo.tie_anio anio, 
                       fact_indicadores_td.fit_valor as idv3, 
                       dim_ubicacion_geografica.ubi_parroquia as parroquia,
                       dim_ubicacion_geografica.ubi_parroquia_cod as codigo, 
                       dim_ubicacion_geografica.ubi_canton_cod as c_codigo, 
                       dim_ubicacion_geografica.ubi_provincia as provincia, 
                       dim_indicador_territorio_digitales.itd_sk as sk1
                       FROM 
                       "TD".dim_indicador_territorio_digitales, 
                       "TD".dim_ubicacion_geografica, 
                       "TD".fact_indicadores_td, 
                       public.dim_tiempo
                       WHERE 
                       dim_indicador_territorio_digitales.itd_sk = fact_indicadores_td.fit_itd_sk AND
                       dim_ubicacion_geografica.ubi_sk = fact_indicadores_td.fit_ubi_sk AND
                       dim_indicador_territorio_digitales.itd_sk in(159) AND
                       dim_tiempo.tie_sk = fact_indicadores_td.fit_tie_sk 
)as i
                       join
                       (
                       SELECT 
                       dim_tiempo.tie_anio anio, 
                       fact_indicadores_td.fit_valor as poblacion, 
                       dim_ubicacion_geografica.ubi_parroquia as parroquia,
                       dim_ubicacion_geografica.ubi_parroquia_cod as codigo, 
                       dim_ubicacion_geografica.ubi_canton_cod as c_codigo, 
                       dim_ubicacion_geografica.ubi_provincia as provincia, 
                       dim_indicador_territorio_digitales.itd_sk as sk1
                       FROM 
                       "TD".dim_indicador_territorio_digitales, 
                       "TD".dim_ubicacion_geografica, 
                       "TD".fact_indicadores_td, 
                       public.dim_tiempo
                       WHERE 
                       dim_indicador_territorio_digitales.itd_sk = fact_indicadores_td.fit_itd_sk AND
                       dim_ubicacion_geografica.ubi_sk = fact_indicadores_td.fit_ubi_sk AND
                       dim_indicador_territorio_digitales.itd_sk in(160) AND
                       dim_tiempo.tie_sk = fact_indicadores_td.fit_tie_sk 
                       )as p
                       on(i.codigo = p.codigo)
                       join
                       (
                       SELECT 
                       dim_ubicacion_geografica.ubi_provincia as provincia,
                       avg(fact_indicadores_td.fit_valor) as promedio
                       FROM 
                       "TD".dim_indicador_territorio_digitales, 
                       "TD".dim_ubicacion_geografica, 
                       "TD".fact_indicadores_td, 
                       public.dim_tiempo
                       WHERE 
                       dim_indicador_territorio_digitales.itd_sk = fact_indicadores_td.fit_itd_sk AND
                       dim_ubicacion_geografica.ubi_sk = fact_indicadores_td.fit_ubi_sk AND
                       dim_indicador_territorio_digitales.itd_sk in (159) AND
                       dim_tiempo.tie_sk = fact_indicadores_td.fit_tie_sk 
                       group by dim_ubicacion_geografica.ubi_provincia,dim_indicador_territorio_digitales.itd_sk
                       )as v
                       on(i.provincia = v.provincia)
                       --where to_number(i.codigo,\'999999\' ) - to_number(i.c_codigo,\'9999\')*100 >= 50 
                       order by i.provincia,i.idv3
                       ')
dbDisconnect(con)

base=100

filtros <- function(input,datos){
  datos<-indicador
  datos<-switch(input$tipo_t,
                "1" = datos%>%filter((as.numeric(datos$codigo)-as.numeric(datos$c_codigo)*100)==50),
                "2" = datos%>%filter((as.numeric(datos$codigo)-as.numeric(datos$c_codigo)*100)>50),
                "3" = datos%>%filter((as.numeric(datos$codigo)-as.numeric(datos$c_codigo)*100)>=50)
  )
  datos <- datos%>%filter(datos$poblacion > base * 10^as.numeric(input$habs_t[1]) & datos$poblacion < base * 10^as.numeric(input$habs_t[2]))
  if(nrow(datos) == 0) 
  {
    datos<-data.frame(t(c(0,0,0,0,0,0,0,0)))
    names(datos) <- names(indicador)
  }
  return(datos)
}




shinyServer(function(input, output) {
  output$grafico1 <- renderGvis({
    datos<-filtros(input,datos);
    gvisHistogram(datos[c(3,7)], options=list(
      colors="['#6090f0']",
      width=750, height=250))
  })
  
  
  output$valor_estad_idv3 <- renderTable(digits=4,{
    datos<-filtros(input,datos)
    media_idv3 = mean(datos$idv3,na.rm=TRUE)
    sd_idv3 = sd(datos$idv3,na.rm=TRUE)
    max_idv3 = max(datos$idv3,na.rm=TRUE)
    min_idv3 = min(datos$idv3,na.rm=TRUE)
    Estdst <- c("Media", "Desv.St.", "Máximo","Mínimo")
    Valor <-c(media_idv3,sd_idv3,max_idv3,min_idv3)
    valores <- data.frame(Estdst,Valor)
    valores
  })
  

  output$valor_estad_pob <- renderTable(digits=0,{
    datos<-filtros(input,datos)
    media_pob = mean(datos$poblacion,na.rm=TRUE)
    sd_pob = sd(datos$poblacion,na.rm=TRUE)
    max_pob = max(datos$poblacion,na.rm=TRUE)
    min_pob = min(datos$poblacion,na.rm=TRUE)
    total_pob = sum(indicador$poblacion)
    suma_pob = sum(datos$poblacion)
    porcent_pob = suma_pob/total_pob*100
    Estdst <- c("Media", "Desv.St.", "Máximo","Mínimo","Suma","%")
    Valor <-c(media_pob,sd_pob,max_pob,min_pob,suma_pob,porcent_pob)
    valores <- data.frame(Estdst,Valor)
    valores
  })

  output$dat_slider <- renderTable(digits = 0, caption = "<h4>Rango de población</h4>" ,
                                   caption.placement = getOption("xtable.caption.placement", "top"),
                                   caption.width = 1000,{
    datos<-filtros(input,datos)
    desde_pob = base * 10^as.numeric(input$habs_t[1])
    hasta_pob = base * 10^as.numeric(input$habs_t[2])
    num_pob = nrow(datos)
    
    Datos <- c("Desde","Hasta", "N")
    Valor <-c(desde_pob,hasta_pob,num_pob)
    Unidad <- c("habitantes","habitantes","poblaciones")
    valores <- data.frame(Datos,Valor,Unidad)
    valores
  })
  
  output$tabla_top <- renderTable(digits=4,{
    datos<-filtros(input,datos)
    valores <- head(datos%>%arrange(desc(idv3))%>%mutate(poblacion = as.character(poblacion)),20)
    valores
  })
  
  output$grafico2 <- renderGvis({
    datos<-filtros(input,datos);
    gvisHistogram(datos[c(3,6)], options=list(
      colors="['#6090f0']",
      width=750, height=250))
    
  })

  output$grafico3 <- renderGvis({
    datos<-filtros(input,datos);
    datos<-head(datos%>%arrange(desc(idv3)),20)
    gvisBubbleChart(datos,
                   idvar="parroquia",
                   xvar="poblacion",yvar="idv3",
                   colorvar="provincia",
                   sizevar="poblacion",
                   options=list(width=1000, height=500),
                   )
    
    
  })
  
    
  output$titulo_idv3 <- renderText("<h4>Estadísticas del IDV3 (Histograma)</h4><hr>")
  output$titulo_pob <- renderText("<h4>Estadísticas de población por territorio (Histograma)</h4><hr>")
  output$titulo_top <- renderText("<h4>Top 20 en rango de población seleccionado</h4><hr>")
})
