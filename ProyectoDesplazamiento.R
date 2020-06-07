library(shiny) 
library(shinythemes)
library(readxl)
library(readr)
library(ggplot2)
library(qcc)
library(SixSigma)
library(Rlab)
library(RColorBrewer)

Data1<-read.csv("https://raw.github.com/jhonarredondo/Bases/master/Pasteleria.csv",sep = ";",header = T, dec = ",", quote = "", check.names = T)
names(Data1)<-c("Dia","Pasteles Defectuosos", "Cantidad de Pasteles Revisados")
Utilidades1<-read.csv("https://raw.github.com/jhonarredondo/Bases/master/Utilidades1.csv",sep = ";",header = T, dec = ",", quote = "", check.names = T)

dias<-rep(1:6)
Data2<-read.csv("https://raw.github.com/jhonarredondo/Bases/master/Impresiones2.csv",sep = ";",header = T, dec = ",", quote = "", check.names = T)
names(Data2)<-c("Fecha","Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sábado")
Defectuosos2<-cbind(dias, dias, (t(Data2[13,2:7])))
Totales2<-cbind(dias,as.data.frame(t(Data2[12,2:7])))
Data2<-Data2[-c(12:18),]
Data2<-Data2[,c(1,2,5,3,4,6,7)]

Data3 <- read.csv("https://raw.github.com/jhonarredondo/Bases/master/Utilidades3.csv",sep = ";",header = T, dec = ",", quote = "", check.names = T)
names(Data3) <- c("Fecha", "Total", "Tiendas U", "Unal","UdeA","UdeM", "EAFIT", "UPB", "S.Buena.", "ITM", "Colmayor","TdeA", 
              "Centros C.", "Santa Fe", "Monterrey", "Puerta del Norte")
Utilidades3 <- Data3[,cbind(1,3)]

Data4<-read.csv("https://raw.github.com/jhonarredondo/Bases/master/Produccion 4.csv",sep = ";",header = T, dec = ",", quote = "", check.names = T)
Defectos4<-read.csv("https://raw.github.com/jhonarredondo/Bases/master/Defectos4.csv",sep = ";",header = T, dec = ",", quote = "", check.names = T)
Defectos4<-Defectos4[1:4,1:2]

#Definir UI
ui <- fluidPage(

  navbarPage(title="JAJ",theme = shinytheme("darkly"), 
    tabPanel(
  
    h2(textOutput("TituloPrincipal")),
    hr(), #Trazar linea horizontal
    
    #Titulo
    titlePanel("Panel de opciones"),
    
    #Barra lateral para el ingreso y salida de datos----
    sidebarLayout(position = "left",
      
      #Panel de datos --------------------
      sidebarPanel(
        fluidRow(   #Inserta una "fila" en el panel de datos
        
        selectInput(inputId = "Base",label = "Base de Datos",
                    choices = c("Caso 1: Pasteleria"="Caso 1",
                                "Caso 2: Impresiones"="Caso 2",
                                "Caso 3: Sanduchitos"="Caso 3",
                                "Caso 4: Camisetas"="Caso 4",
                                "Elegir Base..."="Caso 5")),
        
        br(), #Dejar varias filas de espacio en blanco
        br(),
        
        # Input: Seleccionar Distribucion para numeros aleatorios
        radioButtons("dist", "Elija la distribucion de probabilidad que le interesa:",  
                     c("Normal" = "Distribucion Normal",
                       "Uniforme" = "Distribucion Uniforme",
                       "Exponencial" = "Distribucion Exponencial",
                       "Binomial" = "Distribucion Binomial",
                       "Poisson" = "Distribucion Poisson")),
        
        # Input: Tamano de muestra numeros aleatorios iniciales
        sliderInput("m", paste("Número Subgrupos (m)", sep = ""), 
                    value = 100, min = 5, max = 200, step=5, round=F, animate = T),
        sliderInput("n", paste("Tamaño Muestral (n)", sep = ""), 
                    value = 10, min = 1, max = 20, step=1, round=F, animate = T),
        
        hr()
        ), #Fin primera fila
        
        fluidRow( #segunda fila
          column(img(src="UdeA.png",width="225px",height="225px"), width = 12, align="center")
        ), #Fin segunda fila
        
        selectInput(inputId = "type",label = "Tipo de Carta",
                    choices = c("Carta de Medias"= "xbar",
                                "Carta de Rangos"= "R",
                                "Carta NP"= "np",
                                "Carta P"="p",
                                "Carta C"="c",
                                "Carta U"="u")),
        numericInput(inputId = "columna", label="Columna de Datos", value=2)
        
      ), #Fin Panel de datos
      
      #Panel principal con graficos e informacion:----------------------------------
      mainPanel(
        
        #Titulos sobre pestanas
        h4(span("Debajo encuentra una serie de pestañas, cada una con información diferente y útil",
                style="color:chocolate"),align="center"),
        
        tabsetPanel(type = "tabs",
                    tabPanel("Contextualización",icon =  icon("align-justify"), verbatimTextOutput("Contexto")),
                    
                    tabPanel("Base de Datos", icon = icon("table"), tableOutput("Tabla")),
                    
                    tabPanel("Estudio Descriptivo Previo", icon = icon("chart-line"), 
                             
                             column(
                             plotOutput("serie1"), width = 6),
                             
                             column(
                               plotOutput("serie2"), width = 6),
                             
                             br(),
                             h3(span("Estadisticas y Pruebas",style="color:chocolate"),align="center"),
                             tags$head(tags$style("#summary{color:navy;text-align:center}")),
                             verbatimTextOutput("summary"),
                             hr(),
                             tags$head(tags$style("#shapiro{color:navy;text-align:center}")),
                             verbatimTextOutput("shapiro"),
                             hr(),
                             tags$head(tags$style("#Distribución y Comparaciones{color:navy;text-align:center}")),
                             plotOutput("Boxplot"),
                             hr(),
                             plotOutput("dens")),
                    
                    tabPanel("AMEF", icon = icon("check-circle"), br(), imageOutput("AMEF"),align="center"),
                    
                    tabPanel("Ishikawa", icon = icon("fish"), plotOutput("CE2")),
                    
                    tabPanel("Pareto", icon = icon("exclamation-triangle"), plotOutput("8020"), 
                             hr(),
                             fileInput("file1", "Elija un Archivo de Excel(.xls) estructurado como se indica en el manual de usuario", # Input: Datos Usuario
                                       multiple = FALSE,
                                       accept = c(".xls"))),
                    
                    tabPanel("Carta de Control", icon = icon("chart-area"), 
                             plotOutput("carta"),
                             br(),
                             plotOutput("Carta2"),
                             br()),
                    
                    tabPanel("Capacidad del Proceso", icon = icon("chart-bar"), plotOutput("CP")),
                    
                    tabPanel("Lista de Recomendaciones", icon = icon("hand-point-right"),
                             h4(span("Señor Usuario, a continuacion presentamos unas recomendaciones para usted:",style="color:chocolate"),align="center"),
                             verbatimTextOutput("Recomendaciones"),
                             hr(),
                             h3(span("Tras implementarlo, su carta debería verse asi:",style="color:chocolate"),align="center"),
                             plotOutput("Carta3"))
            
        ) #Fin TabsetPanel
      ) #Fin Mainpanel
      
    ) #FIn sidebarLayout
    
    ) #Fin TabPanel
  ) #Fin navbar
) #Fin ui

#Definir un servidor para la eleccion de casos ---------------------
server <- function(input, output, session) {

  ##Poner titulo al aplicativo
  output$TituloPrincipal <- renderText({
    invalidateLater(1000, session)
    paste("Bienvenido al proyecto Desplazamiento. Hoy es", Sys.time())
  })
  
  ##Elegir caso-------------------------------------------------------
  caso <- reactive({

    Base1 <- switch (input$Base,
                    "Caso 1" = Data1,
                    "Caso 2" = Data2,
                    "Caso 3" = Data3,
                    "Caso 4" = Data4,
                    "Caso 5" = read_excel(file.choose())
    )
    Base1
  })
    
##Generar numeros aleatorios para la variable de calidad 
#Aleatorio2-------------------------------------------------------
    Aleatorio2 <- reactive({
      Subgrupos <- input$m
      Tamano=input$n
      k=input$columna
      Base1 <- switch (input$Base,
                       "Caso 1" = {
                         mu <- rep(c(210.5,209), c(0.8*Subgrupos,0.2*Subgrupos))
                         sigma_W <- runif(1)},
                       
                       "Caso 2" = {
                         mu <- rep(c(1, 3, 4, 6, 8, 4), each=11)
                         sigma_W <- rnorm(1,0,1)
                         Tamano=1},
                       
                       "Caso 3" = {
                         mu <- rep(c(40.5,43,50,48,35), each=10)
                         sigma_W <- rnorm(1,5,5)
                         Tamano=1},
                       
                       "Caso 4" ={
                         mu <- rep(c(4, 3.1, 2.5, 3), each=9)
                         sigma_W <- rnorm(1,0,1)
                         Tamano=1},
                       
                       "Caso 5" ={
                         a <- caso()
                         mu <- a[,k] ##Importante que el usuario cumpla con la indicaciÃ³n de que la segunda columna tiene los valores de la variable
                         mu=as.matrix(mu)
                         sigma_W <- 0
                         Tamano = 1}
      )
      epsilon <- switch(input$dist,
                      "Distribucion Normal" = rnorm(n = Subgrupos*Tamano), 
                      "Distribucion Uniforme" = runif(n = Subgrupos*Tamano),
                      "Distribucion Exponencial" = rexp(n = Subgrupos*Tamano),
                      "Distribucion Binomial" = rbinom(n=Subgrupos*Tamano, size = Tamano, prob = runif(n = 1,0,0.65)),
                      "Distribucion Poisson" = rpois(n=Subgrupos*Tamano, lambda = 1)
      )
      Desplazar = rep(mu, rep(Tamano, length(mu))) + sigma_W*epsilon
      Desplazar
    })
    
  output$Contexto <- renderText({
    Mensaje <- switch (input$Base,
                     "Caso 1" = "En la pastelería Doña Ana se producen alrededor de 50 pasteles de
cada tipo por hora en un turno diario de 12 horas, para cumplir con una demanda 
de alrededor de 580 pasteles solicitados por el cliente. Normalmente, el 
proceso de elaboracion dependia de una maquina moldeadora pero debido a 
una falla en el sistema de extrusion, el proceso se continuo realizando 
de manera manual puesto que se considero innecesario y costoso comprar 
otra. En un principio, dicho cambio no afecto para nada las labores de 
reposteria, exceptuando claro un mayor esfuerzo para los operarios. 

Sin embargo, recientemente Doña Ana tiene la sensación de que 
ha ocurrido un incremento en el número de productos que debe desechar, 
pasando de una media de 2 unidades a 7 unidades por hora. A pesar de que aun 
se logran realizar los 600 pasteles diarios, el número de defectuosos 
que salen de estos es mucho mas grande que lo que se acostumbraba, y por 
tanto las utilidades han disminuido (costos de no calidad). Doña Ana, 
preocupada por las implicaciones que esto pueda traer a su negocio decidió 
analizar su proceso para determinar los puntos criticos de su pasteleria. 
Inicialmente se le podria atribuir a la falta de la máquina el que los 
pasteles no salgan en las mismas condiciones que antes, sin embargo, resulta 
curioso que en apariencia sea este el unico tipo producto que se haya visto 
fuertemente afectado.",
                     
                     "Caso 2" = "En la papelería TAL se presta el servicio de 
internet, impresiones y venta de materiales escolares y varios. La papelería 
actualmente cuenta con 2 impresoras y 3 computadoras. El dueño de la papelería, 
don Henry, ha  venido percibiendo problemas con las impresoras: a la hora de la 
impresión, a una velocidad de 10 libros por hora, durante el proceso se presentan 
páginas con errores en las márgenes, numeración o impresión borrosa, por lo 
que hay que reimprimir estas páginas. Esta problemática trae consigo sobrecostos 
en las hojas, tinta y también el efecto temporal de los reprocesos.",
                     
                     "Caso 3" = "Gloria es la gerente de la Pyme Sanduchitos, empresa 
del sector secundario que se especializa en la elaboración de productos de consumo rápido 
y nutritivos. Su principal zona de impacto son las universidades del departamento de 
Antioquia (UdeA, Unal, ITM, etc.), debido a que por las características de sus sanduches 
(producto principal de la microempresa y que además le dió nombre a esta) son un gran 
atractivo para los universitarios que en el día a día requieren de fuentes de alimentación 
económicas. Sin embargo, también tienen puestos de comida rÃ¡pida en algunos centros 
comerciales de la ciudad (Santa Fe, Monterrey y Puerta del Norte).
Sin embargo, es común que frente a los puestos de venta se generen largas colas de espera. 
Incluso, según comentan los empleados, es común que muchas personas desistan de comprar 
por no tener que esperar. 
Ante el riesgo de perder cuota del mercado por otras empresas o vendedores emergentes 
(como los propios estudiantes que realizan ventas dentro de algunas universidades), 
Sanduchitos emprendió un proyecto para encontrar la razón de tal comportamiento.",
                     
                     "Caso 4" = "Una empresa de confecciones quiere sostener un contrato 
con un grande Comercializador Textil a nivel internacional. Entre los requisitos está el 
cumplir con la siguiente ecuación:
Defectuosos por cada 30 Prendasi< 3. 
Donde i puede ser camisetas, pantalones y camisas.
La empresa también es medida a través de muestreos dobles con un NCA=6%.
Los directivos de la empresa, sin embargo, se sienten preocupados debido a que 2 de los 
últimos 7 lotes de vestimentas que han enviado han sido rechazados, uno de ellos al primer 
premuestreo. Con el fin de evitar mayor dureza en la recepción de sus productos, deciden 
que es urgente prestar atención a su sistema de control de la calidad.",
    
"Caso 5" = "Señor Usurio, el presente aplicativo fue construido principalmente con la 
librería Shiny de Rstudio, ademÃ¡s de librerías adicionales para formatos y gráficos. A
continuación, puede experimentar la diversidad de análisis gráficos y estadísticos a 
partir de una base de datos que usted elija de su sistema operativo. Para conocer los
detalles de como funciona, dirijáse al manual de usuario. También puede revisar los 4
casos de estudios subidos por defecto (y que funcionan en gran parte con números 
aleatorios) como guía de apoyo.
Lamentamos cualquier error o incoherencia que se presentase.")
    
paste(Mensaje)})
  
  output$dens <- renderPlot({ #Histograma------------------------------------
      dist <- input$dist
      ggplot(NULL,aes(x=Aleatorio2())) +
        geom_histogram(binwidth=0.4, aes(y=..density..,fill=..count..), col="darkblue",lwd=0.6) +
        geom_density(col="brown", fill="aquamarine1", alpha=0.4, lty=10) + 
        ylab("Frecuencia") + 
        xlab("Numeros aleatorios") +
        ggtitle(paste(dist, "(", (input$n)*(input$m), ")", sep = "")) + theme(plot.title = element_text(hjust = 0.5))})
  
  ##Probar diferencia de medias y Distribución---------------------------------------------
  output$Boxplot <- renderPlot({
    
    Base1 <- switch (input$Base,
                     "Caso 1" = Data1[,2],
                     "Caso 2" = Data2[,2:7],
                     "Caso 3" = Data3[,4:12, 14:16],
                     "Caso 4" = Data4[,2:4],
                     "Caso 5" = caso()[,-c(1)]) 
    
    c1 <- rainbow(10)
    c2 <- rainbow(10, alpha=0.2)
    c3 <- rainbow(10, v=0.7)
    boxplot(Base1, col=c2, medcol=c3, whiskcol=c1, staplecol=c3, 
            boxcol=c3, outcol=c3, pch=16, cex=0.5, main="Distribuciones de Datos")
    
  })
  # Resumen de los datos aleatorios-------------------------------
  output$summary <- renderPrint({
    summary(Aleatorio2())})
  ##Resolver para caso 5--------------------------------------------
  output$shapiro <- renderPrint({
    shapiro.test(Aleatorio2())})
  
  output$Tabla <- renderTable({ #Tabla de Datos-------------------------
    caso()})

  output$serie1 <- renderPlot({ #Serie1----------------------------------------------------------
    Base <- caso()[,1:2]
    n=length(Base[,1])
    seriec<-data.frame(y=as.matrix(Base), x=rep(1:n,each=1,len=n))
    ggplot(data=seriec, aes(y=as.double(seriec[,2]),x=as.double(seriec[,3])))+
      geom_point(shape=19, size=2,col="darkgreen", fill="seagreen")+
      geom_smooth(method = "lm",linetype="solid",color="skyblue2",fill="violet")+
      xlab(names(Base[1]))+
      ylab(names(Base[2]))+
      ggtitle(paste("Serie", input$Base)) +
      theme(plot.title = element_text(hjust = 0.5, face="bold", color="palegreen4"))})
  
  output$serie2 <- renderPlot({ #Serie2-------------------------------------------------------
    Base1 <- switch (input$Base,
                     "Caso 1" = Utilidades1,
                     "Caso 2" = Defectuosos2,
                     "Caso 3" = Utilidades3,
                     "Caso 4" = Data4[,c(1,6)],
                     "Caso 5" = caso())
    
    n=length(Base1[,1])
    GBase<-data.frame(y=as.matrix(Base1), x=rep(1:n,each=1,len=n))
    ggplot(data=GBase, aes(y=as.double(GBase[,2]),x=as.double(GBase[,3])))+
      geom_point(shape=16, size=3,col="darkgreen", fill="seagreen")+
      geom_smooth(method = "loess",linetype="solid",color="skyblue2",fill="violet")+
      xlab("Tiempo")+
      ylab(names(Base1[2]))+
      ggtitle("Serie temporal con intervalos") + 
      theme(plot.title = element_text(hjust = 0.5, face="bold", color="palegreen4"))})
  
  output$AMEF <- renderImage({ #AMEF------------------------------
    Amef <- switch (input$Base,
                     "Caso 1" = 'www/AMEF1.jpg',
                     "Caso 2" = 'www/AMEF2.jpg',
                     "Caso 3" = 'www/AMEF3.jpg',
                     "Caso 4" = 'www/AMEF4.jpg',
                     "Caso 5" = 'www/Caracterizacion.jpg')
    
    filename <- normalizePath(file.path(Amef))
    list(src = filename)
  }, deleteFile = FALSE)
  
  output$CE2 <- renderPlot({ #-------------------------Ishikawa
    Base1 <- switch (input$Base,
                     "Caso 1" = {
                       H<-c("Pasteleros Nuevos", "Descuidos Recurrentes", "Fatiga")
                       Mq<-c("Mantenimiento Inadecuado     ", "Horno Sucio/Desorganizado      ")
                       MP<-c("Mala Calidad", "Insumos Viejos", "Almacenamiento Inadecuado")
                       Medio<-c("Demanda", "Ruido Externo")
                       Mt<-c("Diseño de Planta", " Procedimiento")
                       Md<-c("Tolerancias mal definidas", "Equipos de medicion descalibrados")
                       Efecto<-"Pasteles Desechados"},
                     
                     "Caso 2" = {
                       H<-c("Desatención", "Fatiga")
                       Mq<-c("Impresoras Descalibradas", "Falta Mantenimiento", "Derrame")
                       MP<-c("Falta Tinta","Almacenamiento Inadecuado")
                       Medio<-c("Alta Demanda", "Sobrecalentamiento")
                       Mt<-c("Procedimiento Inadecuado")
                       Md<-c("Mal fijamiento de los Materiales")
                       Efecto<-"Impresiones Defectuosas"
                     },
                     
                     "Caso 3" = {
                       H<-c("Empleados Nuevos", "Desatencion", "Fatiga")
                       Mq<-c("Equipos Descalibrados")
                       MP<-c("Escasez","Almacenamiento Inadecuado")
                       Medio<-c("Demanda", "Ruido Externo")
                       Mt<-c("Falta Estandarizacion", "Procedimiento")
                       Md<-c("Falta Inspeccion")
                       Efecto<-"Quejas Cliente"},
                     
                     "Caso 4" ={
                       H<-c("Personal No Calificado", "Desatencion", "Fatiga")
                       Mq<-c("Máquina de
 Costura Defectuosa", "Instrumento de
 Corte desgastado")
                       MP<-c("Mala Calidad Hilo","Tela Inadecuada")
                       Medio<-c("Presión Laboral", "Ruido Externo")
                       Mt<-c("Falta Estandarización", "Molde Con Defecto")
                       Md<-c("Falta Inspección")
                       Efecto<-"Camisetas 
Con Defectos"},
                     "Caso 5" ={
                       H<-c("Cambio Personal", "Mayor/Menor AtenciÃ³n 
Operarios")
                       Mq<-c("Cambio Maquinaria y Equipos")
                       MP<-c("Proveedores nuevos", "Problemas Almacenamiento MP")
                       Medio<-c("Presion Laboral", "Ruido Externo", "Distracciones")
                       Mt<-c("Proceso Mejoro/Empeoró", "Cambio en el Procedimiento")
                       Md<-c("Cambio en los metodos de inspección")
                       Efecto<-"Desplazamiento"})

      cause.and.effect(cause=list(Hombre=H,Maquinaria=Mq, Material=MP, Entorno=Medio,
Metodo=Mt,Medida=Md), effect=Efecto)})
  
  #Pareto------------------------
  output$'8020' <- renderPlot({
    #Hacer tabla de defectos: En el caso de estudio se deberia tener en una hoja listo ;)
    Base1 <- switch (input$Base,
                     "Caso 1" = {
                        Tipo<-c(175,120,60,55)
                        names(Tipo) <- c("Peso Inadecuado", "Textura", "Mal Horneado", "Color")},
                     "Caso 2" = {
                       Tipo<-Defectuosos2[,3]
                       names(Tipo) <- c("Lunes", "Martes","Miercoles","Jueves","Viernes","Sábado")},
                     
                     "Caso 3" = {
                        Tipo<-c(10,15,2,3,0,1,6,3,1)
                        names(Tipo) <- c("Unal","UdeA","UdeM", "EAFIT", "UPB", "S.Buena.", "ITM", "Colmayor","TdeA")},
                     "Caso 4" = {
                       Tipo<-Defectos4[,2]
                       names(Tipo) <- Defectos4[,1]},
                     "Caso 5" = {
                       Defectos <-read_excel(input$file1$datapath)
                       Defectos<-as.matrix(Defectos)
                       Tipo <- as.numeric(Defectos[,2])
                       names(Tipo) <- Defectos[,1]
                       })
    
      pareto.chart(Tipo, ylab="Frecuencia",ylab2="% Acumulado", 
                   cumperc = seq(0,80,by=5), 
                   main="AnÃ¡lisis de Pareto",col=rainbow(9), las=1)
      abline(h=0.8, col="purple")})
  
  ##Carta1---------------------------------------------------
  output$carta <- renderPlot({
    Subgrupos <-input$m
    Tamano=input$n
    n <- Tamano*Subgrupos
    CBase <- as.vector(Aleatorio2())
    Base1 <- switch (input$Base,
                     "Caso 1" = {
                       Tipo <- "R"
                       Cant <- Tamano
                       nombre <- "Pesos Pasteles"
                       muestra<-rep(1:Subgrupos,each=Tamano,len=n)
                       Subgrupo<-qcc.groups(CBase, muestra)},
                    
                      "Caso 2"={
                        Tipo <- "np"
                        Cant <- 20
                        nombre <- "Impresiones con manchas"
                        Subgrupo<- as.integer(CBase)
                        for (i in 1:Subgrupos) {
                          if (Subgrupo[i]<0) {
                            Subgrupo[i]=0
                          }}},
                     
                     "Caso 3" = {
                       Tipo <- "c"
                       Cant <- 1
                       nombre <- "Clientes Atendidos"
                       Subgrupo<- as.integer(CBase)},
                     
                     "Caso 4" = {Tipo <- "np"
                       Cant <- 30
                       nombre <- "Camisetas Defectuosas detectadas en inspecciones de 30 camisetas por lote en la empresa"
                       Subgrupo<- as.integer(CBase)
                       for (i in 1:Subgrupos) {
                         if (Subgrupo[i]<0) {
                           Subgrupo[i]=0
                         }}},
                     
                     "Caso 5" = {
                       nombre <- "Datos Usuario De la Columna Seleccionada"
                       Tipo <- input$type
                       if (input$type=="xbar" | input$type=="R") {
                         Cant=Tamano
                         muestra<-rep(1:Subgrupos,each=Tamano,len=(Tamano*Subgrupos))
                         CBase<-as.matrix(CBase)
                             if ((Subgrupos*Tamano)>length(CBase)) {
                               length(muestra)<-length(CBase)
                               Subgrupo<- qcc.groups(CBase,muestra)
                             }
                             else{
                              CBase<-CBase[1:(Tamano*Subgrupos),1]
                             Subgrupo<- qcc.groups(CBase,muestra)}}
                       else{
                         Subgrupo=CBase[1:Subgrupos]
                         if (max(Subgrupo)>Tamano) {
                           Cant=max(Subgrupo)
                         }
                         else{
                          Cant=Tamano}}}
    )
      CartaUno <-qcc(data=Subgrupo, type=Tipo, data.name = nombre, sizes = Cant)

  })
  
  ###Carta2-------------------
  output$Carta2 <- renderPlot({
    Subgrupos <- input$m
    Tamano=input$n
    n <- Tamano*Subgrupos
    CBase2 <- as.vector(Aleatorio2())
    Base1 <- switch (input$Base,
                     "Caso 1" = {
                       Tipo <- "xbar"
                       Cant <- Tamano
                       nombre <- "Pesos Pasteles"
                       muestra<-rep(1:Subgrupos,each=Tamano,len=n) 
                       Subgrupo<-qcc.groups(CBase2, muestra)},
                     
                     "Caso 2" = {
                       Tipo <- "p"
                       nombre <- "Impresiones con manchas"
                       Subgrupo <- as.integer(CBase2)
                       for (i in 1:Subgrupos) {
                         if (Subgrupo[i]<0) {
                           Subgrupo[i]=0
                         }}
                       Cant <- runif(length(Subgrupo),7,25)},
                     
                     "Caso 3" = {
                       Tipo <- "u"
                       Cant <- Tamano
                       nombre <- "Clientes Atendidos"
                       Subgrupo<- as.integer(CBase)},
                     
                     "Caso 4" = {
                       Tipo <- "p"
                       nombre <- "Proporcion de Camisetas Con Defectos Detectadas por Muestreo Doble en la Comercializadora"
                       Subgrupo <- as.integer(CBase2)
                       for (i in 1:Subgrupos) {
                         if (Subgrupo[i]<0) {
                           Subgrupo[i]=1
                         }}
                       a<-rbern(1,0.99)
                       Cant <- (1-a)*(rbinom(length(Subgrupo),2,0.5)*8+8) + (a)*(rbinom(length(Subgrupo),2,0.5)*13+13)},
                     
                     "Caso 5" = {
                         k=input$columna
                         k=as.numeric(k)+1
                         a=caso()
                         a=as.matrix.data.frame(a)
                         CBase=a[,k]
                         nombre <- "Datos Usuario de la Columna Siguiente"
                         Tipo <- input$type
                         if (input$type=="xbar" | input$type=="R") {
                           Cant=Tamano
                           muestra<-rep(1:Subgrupos,each=Tamano,len=(Tamano*Subgrupos))
                           CBase<-as.matrix(CBase)
                           if ((Subgrupos*Tamano)>length(CBase)) {
                             length(muestra)<-length(CBase)
                             Subgrupo<- qcc.groups(CBase,muestra)
                           }
                           else{
                             CBase<-CBase[1:(Tamano*Subgrupos),1]
                             Subgrupo<- qcc.groups(CBase,muestra)}}
                         else{
                           Subgrupo=CBase[1:Subgrupos]
                           if (max(Subgrupo)>Tamano) {
                             Cant=max(Subgrupo)
                           }
                           else{
                             Cant=Tamano}}})
    
    CartaDos<-qcc(data=Subgrupo, type=Tipo, sizes = Cant, data.name = nombre)
    })
  
  output$CP<-renderPlot({ #Capacidad del proceso-----------------------------------
    Base1 <- switch (input$Base,
                     "Caso 1" = {
                       muestra<-rep(1:input$m,each=input$n,len=(input$n)*(input$m)) 
                       Subgrupo<-qcc.groups(Aleatorio2(), muestra)
                       CartaDos<-qcc(data=Subgrupo, type="xbar", sizes = input$n, data.name = "Carta de Medias")    
                       process.capability(CartaDos, spec.limits=c(208,211), nsigmas = 3)},
                     
                     "Caso 2" = {hist(Aleatorio2(), main="Histograma de Proceso", xlab="Defectuosos Por Lote")
                       abline(v=4, col="red",lty=2)
                       legend(x="topright",legend = c("Especificacion Superior"), fill=c(2),cex=1,text.font=1, bg='gray97')},
                     
                     "Caso 3" = {hist(Aleatorio2(), main="Histograma de Proceso", xlab="Clientes Atendidos Por Hora")
                       abline(v=50, col="red",lty=2)
                       legend(x="topright",legend = c("Especificacion Superior"), fill=c(2),cex=1,text.font=1, bg='gray97')},
                     
                     "Caso 4" = {hist(Aleatorio2(), main="Histograma de Proceso", xlab="Defectuosos Por Lote")
                       abline(v=3, col="red",lty=2)
                       legend(x="topright",legend = c("Especificacion Superior"), fill=c(2),cex=1,text.font=1, bg='gray97')},
                     
                     "Caso 5" = {hist(Aleatorio2(), col=brewer.pal(n = 8, name = 'RdBu'),main="Histograma de Proceso", xlab="Datos Usuario")})
    Base1})
  
  output$Recomendaciones <- renderText({
    Base1 <- switch (input$Base,
                     "Caso 1" = {mensaje="Manejar diferentes modelos de moldes para cada producto
Calibracion del equipo de medicion
Revision de los procedimientos
Realizar practicas de las tareas manuales antes de realizarlas en el proceso
Limpieza en el lugar de trabajo
Disminuir fuentes Externas que irrumpan durante el proceso (ruido, conversaciones, etc.)
Acuerdos con Proveedores y adecuado almacenamiento de alimentos (Relaciones mutuamente beneficiosas con el proveedor)"},
                     
"Caso 2" = {mensaje="Llenado de tinta cuando no se este prestando servicio
Revision periodica del nivel de tinta y cantidad de hojas
Mantenimiento preventivo a las impresoras
Pronostico de gasto de recursos para la toma de decisiones"},
                     
"Caso 3" = {mensaje="Persistir en las mediciones
Efectuar el estudio en otras universidades
Crear incentivos que mantengan los dos locales de impacto de la UdeA con el mismo nivel de servicio
Hacer estudios de metodos y tiempos, para aumentar la productividad del servicio"},
                     
"Caso 4" = {mensaje="Inspeccionar Materia Prima
Realizar el mismo estudio en otros subprocesos y productos
Brindar capacitación a los empleados de todos los turnos en temas de buen manejo de las máquinas
Seguimiento estricto a los índices y cartas de defectos y defectuosos"},

"Caso 5" = {mensaje="Capacitar nuevamente a los empleados
Mantenimiento Preventivo Máquinas
Calibrar equipos continuamente
Relaciones Mutuamente Beneficiosas con el Proveedor
Estandarizar Procesos
Implementar metodologíaas que impidan el efecto de cambios en las 6M, como SMED
Como ultimo recurso, evaluar el cambio de carta de control"})
    paste(mensaje)
  })
  
  ###Carta ideal-------------------------------------------
  output$Carta3 <- renderPlot({
    Subgrupos <- input$m
    Tamano=input$n
    n <- Tamano*Subgrupos

    Base1 <- switch (input$Base,
                     "Caso 1" = {
                       Tipo <- "xbar"
                       Cant<-Tamano
                       nombre <- "Pesos Pasteles Ideales"
                       CBase2 <- rnorm(n,210,0.9)
                       muestra<-rep(1:Subgrupos,each=Tamano,len=n) 
                       Subgrupo<-qcc.groups(CBase2, muestra)},
                     
                     "Caso 2" = {
                       Tipo <- "p"
                       nombre <- "Impresiones con manchas - Defectuosos ideales"
                       Cant <- runif(Subgrupos,7,28)
                       CBase2=rbinom(Subgrupos, n = Cant, prob = 0.01)
                       Subgrupo <- CBase2},
                     
                     "Caso 3" = {
                       Tipo <- "c"
                       Cant<-1
                       nombre <- "Clientes Atendidos Tras Instalación Nuevos Puestos"
                       CBase2 <- rnorm(Subgrupos,71,8)
                       Subgrupo<-as.integer(CBase2)},
                     
                     "Caso 4" = {
                       Tipo <- "p"
                       a<-rbern(1,0.99)
                       Cant <- (1-a)*(rbinom(Subgrupos,2,0.5)*8+8) + (a)*(rbinom(Subgrupos,2,0.5)*13+13)
                       nombre <- "Camisetas Defectuosas"
                       CBase2 <- rbinom(Subgrupos, Cant, 0.05)
                       Subgrupo<-CBase2},
                     
                     "Caso 5" = {
                       nombre <- "Datos Usuario Ideales"
                       Tipo <- input$type
                       a<-as.vector(Aleatorio2())
                       Caso=as.matrix(a)
                       if (input$type=="xbar" | input$type=="R") {
                         Cant=Tamano
                         muestra<-rep(1:Subgrupos,each=Tamano,len=(Tamano*Subgrupos))
                         CBase<-rnorm(Tamano*Subgrupos, mean(Caso),sd(Caso))
                         Subgrupo<- qcc.groups(CBase,muestra)}
                        else{
                          CBase=rpois(Subgrupos,mean(Caso))
                          Subgrupo=CBase[1:Subgrupos]
                          if (max(Subgrupo)>Tamano) {
                            Cant=max(Subgrupo)
                          }
                          else{
                            Cant=Tamano}}}
    )
      CartaFinal<-qcc(data=Subgrupo, type=Tipo, sizes = Cant, data.name = nombre)
    })
}
# Create Shiny app ----
shinyApp(ui, server)