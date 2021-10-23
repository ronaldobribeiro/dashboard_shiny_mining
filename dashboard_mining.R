
#############################################################################################
############                      PRÉ ESCOPO AMBIENTE do R                       ############    
############################################################################################
Sys.setlocale('LC_CTYPE', 'en_US.UTF-8')
library(shiny)
library(sqldf)
library(odbc)
library(RODBC)
library(DT)
library(shinydashboard)
library(stringr)
library(shinymanager)
library(shinyjs)

shinyOptions(shiny.maxRequestSize = 10000*1024^2)

options(shiny.port = 3000)
options(shiny.host = "0.0.0.0")



agora <- Sys.time()

inactivity <- "function idleTimer() {
 var t = setTimeout(logout, 120000);
 window.onmousemove = resetTimer; // catches mouse movements
 window.onmousedown = resetTimer; // catches mouse movements
 window.onclick = resetTimer;     // catches mouse clicks
 window.onscroll = resetTimer;    // catches scrolling
 window.onkeypress = resetTimer;  //catches keyboard actions
 
 function logout() {
 window.close();  //close the window
 }
 
 function resetTimer() {
 clearTimeout(t);
 t = setTimeout(logout, 120000);  // time is in milliseconds
 }
 }
 idleTimer();
 "

#CONEXÃO ODBC COM BANCO DE DADOS 
con   <- dbConnect(odbc(),
                   Driver = "ODBC Driver 17 for SQL Server",
                   Server = "18.116.160.115",
                   Database = "bd_teste",
                   UID = " ",
                   PWD = " ",
                   Port = 1433)

#CREDENCIAIS DE ACESSO AO SPILMING 
credentials <- data.frame(dbGetQuery(con,paste0("SELECT membros.id, membros.login, pass.pass, hierarquia.hierarquia ",
                                                "FROM lista_membros AS membros ",
                                                "LEFT JOIN pass_membros AS pass ON pass.id = membros.id ",
                                                "LEFT JOIN hierarquia_membros AS hierarquia ON hierarquia.id = membros.hierarquia")))
#TRAZENDO AS CREDENCIAIS 
colnames(credentials) <- c("id","user","password","hierarquia")



#EXECUTANDO ETL DA BASE SPILMINING
dbExecute(
  con,
  "DELETE FROM testando123"
)



df2 <- ""
df2 <- data.frame(dbGetQuery(con,"SELECT nome_cliente, categoria_produto, volume_produto, nome_produto FROM tb_mineracao"))


#TRATAMENTO DE DADOS DA COLUNA 
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<e0>", "a")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<e1>", "a")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<e1>", "a")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<e3>", "a")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+0080>", "A")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+0081>", "A")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<e7>", "c")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<e9>", "e")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ea>", "e")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+00A8>", "e")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+0089>", "E")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+00AD>", "i")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+00AC>", "i")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+008D>", "I")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+00B2>", "o")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<d5>", "o")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<f3>", "o")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<f4>", "o")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+0094>", "O")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<fa>", "u")
df2$nome_produto <-  df2$nome_produto %>% str_replace_all("<ed><U\\+009A>", "U")


#CRIAÇÃO DAS SEGMENTACOES 
df2$segmento<-ifelse(str_detect(df2$nome_produto,'Kit')==TRUE, 'KIT',
                     ifelse(str_detect(df2$nome_produto,'Shampoo')==TRUE,'Shampoo',
                            ifelse(str_detect(df2$nome_produto,'Creme')==TRUE,'Creme',
                                   ifelse(str_detect(df2$nome_produto,'Caixa')==TRUE,'Caixa',
                                          ifelse(str_detect(df2$nome_produto,'Corretivo')==TRUE,'Corretivo',
                                                 ifelse(str_detect(df2$nome_produto,'Base')==TRUE,'Base',
                                                        ifelse(str_detect(df2$nome_produto,'Protetor')==TRUE,'Protetor',
                                                               ifelse(str_detect(df2$nome_produto,'Coloracao')==TRUE,'Coloracao',
                                                                      ifelse(str_detect(df2$nome_produto,'Condicionador')==TRUE,'Condicionado',
                                                                             ifelse(str_detect(df2$nome_produto,'Esmalte')==TRUE,'Esmalte',
                                                                                    ifelse(str_detect(df2$nome_produto,'Oleo')==TRUE,'Oleo',
                                                                                           ifelse(str_detect(df2$nome_produto,' cola')==TRUE,'Sacola',
                                                                                                  ifelse(str_detect(df2$nome_produto,'Gel')==TRUE,'Gel',
                                                                                                         ifelse(str_detect(df2$nome_produto,'Caixinha')==TRUE,'Caixinha','Demais'))))))))))))))



colnames(df2) <- c("nome_cliente","categoria_produto","volume_produto","nome_produto","segmento")

df2 <- df2[,c(1,2,3,5)]
df_temp2 <- data.frame(sqldf("SELECT '-' AS nome_cliente, '-' AS categoria_produto, '-' AS volume_produto, '-' AS segmento"))
colnames(df_temp2) <- c("nome_cliente","categoria_produto", "volume_produto", "segmento")
df2 <- rbind.data.frame(df_temp2, df2)

#DESCONECTANDO DO BANCO DE DADOS
dbDisconnect(con)





################################# PARAMETROS DO UI ########################################
ui <- secure_app(head_auth = tags$script(inactivity), 
                 dashboardPage(
                   dashboardHeader(title ="Intel. Mercado"),
                   dashboardSidebar(
                     tags$head(tags$script(
                       "function get_id(clicked_id) {
               Shiny.setInputValue('current_id', clicked_id, {priority: 'event'});
          }
          
          ")),
                     
                     selectInput("cliente_selecionado", 
                                 label = "Cliente",
                                 choices = unique(df2$nome_cliente),
                                 selected = "-"),
                     
                     selectizeInput("cliente_categoria", 
                                    label = "Categoria Cliente",
                                    choices = unique(df2$segmento),
                                    selected = "-"), 
                     
                     selectInput("cliente_volume", 
                                 label = "Volume",
                                 choices = unique(df2$volume_produto),
                                 selected = "-"),
                     
                     selectInput("cliente_descarte", 
                                 label = "Desinteresse Produtos",
                                 choices =  c("-","Descartes", "Interessante"),
                                 selected = "Interessante"),
                     
                     
                     htmlOutput('rotulo_botao'),
                     actionButton("btn_desconsidera", "Atualizar"),
                     selectInput("tamanho_foto2", 
                                 label = "Qualidade Foto",
                                 choices = c("Deslizar","Baixo", "Medio", "Alto"),
                                 selected = "-"),
                     
                     sliderInput("tamanho_foto", 
                                 label = "Tamanho Foto",
                                 min = 100, max = 250, value = 100)
                   ) ,
                   
                   
                   dashboardBody(useShinyjs(),
                                 tags$style(type="text/css",
                                            ".shiny-output-error { visibility: hidden; }",
                                            ".shiny-output-error:before { visibility: hidden; }"
                                 ),
                                 
                                 tabsetPanel(type = "tabs", id = "abas",
                                             tags$head(
                                               tags$script(
                                                 "$(function() {
                          $('li a').on('click', function() {
                            var val = $(this).attr('data-value');
                            Shiny.setInputValue('currentTab', val);
                            if (val === 'Portfolio Clientes'){
                              var div = document.getElementById('sidebarItemExpanded');
                              div.style.visibility = 'visible';
                              var div = document.getElementById('sidebarCollapsed');
                              div.setAttribute('data-collapsed', false);
                            } else {
                              var div = document.getElementById('sidebarItemExpanded');
                              div.style.visibility = 'hidden';
                              var div = document.getElementById('sidebarCollapsed');
                              div.setAttribute('data-collapsed', true);
                            }
                          
                          });
                 
                 setTimeout(function(){
                    $('li a').first().remove();
                    $('li a').second().click();
                  }, 1);
                 
                });
                "
                                               )
                                             ),
                                             tabPanel("Dashboard",
                                                      fluidRow( column(width = 12, box(width = '100%',  htmlOutput('geral') )))
                                                      , selected = 1
                                             ),
                                             tabPanel("Portfolio Clientes", 
                                                      DT::dataTableOutput('table')
                                             )
                                             
                                 )
                   )
                 )
)


################################# PARAMETROS SERVER #########################################

server <- function(input, output, session) {
  #TESTE DE CONECTIVIDADE 
  result_auth<- secure_server(check_credentials = check_credentials(credentials))
  
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  
  output$rotulo_botao <- renderText({
    
    paste0(
      "<div class='form-group shiny-input-container'><strong id='rotulo_btn'>Atualizar Interesse Produto</strong></div>"  
    )
  })
  
  observeEvent(input$btn_desconsidera, {
    
    con   <- dbConnect(odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = "18.116.160.115",
                       Database = "bd_teste",
                       UID = " ",
                       PWD = " ",
                       Port = 1433)
    
    
    df_fila <- dbGetQuery(con, "SELECT REPLACE([texto],'row','') AS texto FROM (SELECT texto, COUNT(texto) AS qtd FROM testando123 GROUP BY texto ) AS validado WHERE (qtd % 2) != 0")
    seletor = "-"
    mudou = 0
    
    if ( nrow(data.frame(df_fila)) > 0 ){
      fim <- 1
      varre_desconsidera <- 1
      fim <- as.numeric(nrow(data.frame(df_fila)))
      
      for(varre_desconsidera in 1:fim){
        
        atualizador <- as.character(dbGetQuery(con, paste0("SELECT descartar FROM tb_mineracao WHERE id = ",unlist(df_fila)[varre_desconsidera])))
        
        if(atualizador == '0'){
          atualizador = 1
        } else {
          atualizador = 0
        }
        
        dbSendQuery(con, paste0("UPDATE tb_mineracao SET descartar='",atualizador,"' WHERE id IN (",unlist(df_fila)[varre_desconsidera],");"))
        varre_desconsidera <-  varre_desconsidera + 1
        mudou = 1
      }
      
      dbExecute(con,"DELETE FROM testando123")
      
      
      if (input$cliente_descarte == '-'){
        seletor = "Interessante"
      } else {
        seletor = "-"
      }
      
      alert(paste0(nrow(df_fila)," Produto(s) Atualizado(s) ! Recarregando Lista de Produtos."));
      
      
      
      
      
    } else {
      alert("Favor selecionar antes de desconsiderar!");
    }
    
    dbDisconnect(con)
    
    if(mudou == 1){
      updateSelectInput(session, "cliente_descarte", choices = c("-","Descartes", "Interessante"), selected = seletor )
    }
    
  })
  
  
  
  df_fila <- "-"
  df_fila <- data.frame(df_fila)
  
  
  
  
  shiny::observeEvent(input$current_id,{
    print(paste0("Selecionado item: ",input$current_id))
    df_fila <- update_df_fila()
    
  })
  
  
  update_df_fila <- reactive({
    fila <- input$current_id
    dados_df = ""
    dados_df = fila
    dados_df <- data.frame(dados_df)
    renomear <- names(dados_df)
    dados_df <- sqldf(paste0("SELECT [",renomear,"] AS texto FROM dados_df"))
    
    
    con   <- dbConnect(odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = "18.116.160.115",
                       Database = "bd_teste",
                       UID = " ",
                       PWD = " ",
                       Port = 1433)
    
    
    dbWriteTable(conn = con, 
                 name = "testando123", 
                 value = dados_df,
                 append = TRUE)
    
    Sys.sleep(1)
    df_fila <- dbGetQuery(con, "SELECT texto FROM (SELECT texto, COUNT(texto) AS qtd FROM testando123 GROUP BY texto ) AS validado WHERE (qtd % 2) != 0")
    dbDisconnect(con)
    
    df_fila
  })
  
  
  output$aba_atual <- renderText({  
    aba_atual <- input$currentTab  
    aba_atual
  })
  
  #ENTRADA DE NOME DO CLIENTE 
  output$geral<-renderText({
    '<iframe width="100%" height="900" src="https://app.powerbi.com/reportEmbed?reportId=835b3f85-ee45-41fa-9777-782b8d377954&autoAuth=true&ctid=37742533-c331-460c-8229-de723c449df9&config=eyJjbHVzdGVyVXJsIjoiaHR0cHM6Ly93YWJpLWJyYXppbC1zb3V0aC1yZWRpcmVjdC5hbmFseXNpcy53aW5kb3dzLm5ldC8ifQ%3D%3D%22" frameborder="0" allowFullScreen="true"></iframe>'
  })
  
  
  myData <- reactive({
    
    if (input$cliente_selecionado == "-" ){
      cliente_selecionado <- "<> '7512'"
    } else {
      cliente_selecionado <- paste0("= '",input$cliente_selecionado,"'")
    }
    
    if (input$cliente_categoria == '-'){
      cliente_categoria <- "<> '7512'"
    } else {
      cliente_categoria <- paste0("= '",input$cliente_categoria,"'")
    }
    
    if (input$cliente_volume == '-'){
      cliente_volume <- "<> '7512'"
    } else {
      cliente_volume <- paste0("= '",input$cliente_volume,"'")
    }
    
    
    
    if (input$cliente_descarte == '-'){
      cliente_descarte <- "<> 3"
    } else if (input$cliente_descarte == 'Interessante'){
      cliente_descarte <- "= 0"
    } else if (input$cliente_descarte == 'Descartes'){
      cliente_descarte <- "= 1"
    }
    
    
    
    
    
    
    con   <- dbConnect(odbc(),
                       Driver = "ODBC Driver 17 for SQL Server",
                       Server = "18.116.160.115",
                       Database = "bd_teste",
                       UID = " ",
                       PWD = " ",
                       Port = 1433)
    
    
    
    
    #OBTENDO DADOS QUE FORAM CAPTADOS PARA DESCARTAR DO MONTANTE GERAL
    ja_minerados <- ""
    
    
    
    
    
    ja_minerados <- dbGetQuery(con, paste0("SELECT id, data_criacao, data_atualizacao, nome_cliente, descartar, CAST(cod_produto AS VARCHAR(250)) AS cod_produto, url_produto, url_imagem_produto  FROM tb_mineracao WHERE nome_cliente ",cliente_selecionado  ," AND  segmento", cliente_categoria," AND volume_produto ",cliente_volume," AND descartar ",cliente_descarte  ))
    temp_query1 <- dbGetQuery(con, paste0("SELECT preco_produto, volume_produto, largura_produto, altura_produto, categoria_produto, segmento  FROM tb_mineracao WHERE nome_cliente ",cliente_selecionado  ," AND  segmento", cliente_categoria," AND volume_produto ",cliente_volume," AND descartar ",cliente_descarte))
    temp_query2 <- dbGetQuery(con, paste0("SELECT nome_produto, descricao_produto  FROM tb_mineracao WHERE nome_cliente ",cliente_selecionado  ," AND  segmento", cliente_categoria," AND volume_produto ",cliente_volume," AND descartar ",cliente_descarte))
    
    
    #ja_minerados <- sqldf(paste0("SELECT *, '",agora,"' AS hora_tentativa, leftstr(data_atualizacao,4)||'-'||rightstr(leftstr(data_atualizacao,6),2)||'-'||rightstr(leftstr(data_atualizacao,8),2) AS dif_dias  FROM ja_minerados"))
    #ja_minerados$dif_dias <- as.Date.character(ja_minerados$dif_dias, "%Y-%m-%d")
    #ja_minerados$dif_dias <- as.Date(agora) - as.Date(ja_minerados$dif_dias)
    ja_minerados <- cbind.data.frame(ja_minerados, temp_query1, temp_query2)
    ja_minerados$nome_produto <- enc2native(ja_minerados$nome_produto)
    
    
    
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<e0>", "a")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<e1>", "a")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<e1>", "a")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<e3>", "a")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+0080>", "A")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+0081>", "A")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<e7>", "c")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<e9>", "e")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ea>", "e")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+00A8>", "e")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+0089>", "E")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+00AD>", "i")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+00AC>", "i")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+008D>", "I")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+00B2>", "o")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<d5>", "o")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<f3>", "o")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<f4>", "o")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+0094>", "O")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<fa>", "u")
    ja_minerados$nome_produto <-  ja_minerados$nome_produto %>% str_replace_all("<ed><U\\+009A>", "U")
    
    
    rm(temp_query1,temp_query2)
    
    dbDisconnect(con)
    
    
    resultado_consulta <- data.frame(sqldf(paste0("SELECT id, cod_produto, nome_produto, url_imagem_produto, preco_produto, volume_produto, descartar, largura_produto, altura_produto, url_produto, data_criacao  FROM ja_minerados")))
    resultado_consulta$data_criacao<-as.Date(paste0(substr(ja_minerados$data_criacao,1,4),'-',substr(ja_minerados$data_criacao,5,6),'-',substr(ja_minerados$data_criacao,7,8)),"%Y-%m-%d")
    #resultado_consulta$data_criacao<-as.Date(ja_minerados$data_criacao)
    colnames(resultado_consulta) <- c("id", "SKU", "Nome Produto", "url_imagem_produto", "Preco", "Volume", "Descartar", "Largura", "Altura", "url_produto", "data_criacao")
    resultado_consulta
  })
  
  
  output$table <- DT::renderDataTable({
    req(input$tamanho_foto)
    req(input$tamanho_foto2)
    #data.frame(
    temp <- myData()
    #print(paste0(nrow(temp))," linhas")
    temp2 <- temp[,2:3]
    temp3 <- temp[,5:7]
    
    if(input$tamanho_foto2 != "Deslizar"){
      if(input$tamanho_foto2 =="Baixo") {tamanho_foto <- as.numeric(75)}
      else if(input$tamanho_foto2 =="Medio") {tamanho_foto <- as.numeric(150)}
      else if(input$tamanho_foto2 =="Alto") {tamanho_foto <- as.numeric(250)}
      else {tamanho_foto <- as.numeric(75)}
    } else {
      tamanho_foto <- as.numeric(input$tamanho_foto)
    }
    
    Link     <- paste0('<a href="',temp$url_produto,'" target="_blank">Ver Produto</a>')
    #addCheckboxButtons <- paste0('<input type="checkbox" name="row', temp$id, '" id="row', temp$id, '" value="',temp$id, '" onchange="alerta_checkbox(this, this.id)">',"")
    addCheckboxButtons <- paste0('<input type="checkbox" name="row', temp$id, '" id="row', temp$id, '" value="',temp$id, '" onchange="get_id(this.id)">',"")
    
    Foto     <- paste0('<img id="imagem_', temp$id, '" src="',temp$url_imagem_produto, '" " alt="',temp$url_imagem_produto, '" width="',tamanho_foto,'" height="',tamanho_foto,'">')
    
    DT::datatable(cbind(Desinteresse=addCheckboxButtons, temp2,Foto, temp3,Link),
                  options = list(orderClasses = TRUE,
                                 lengthMenu = c(5, 10, 20),
                                 pageLength = 5, 
                                 language = list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/Portuguese-Brasil.json'),
                                 callback = JS(
                                   "function(table) {
    table.on('change.dt', 'tr td input:checkbox', function() {
          setTimeout(function () {
          Shiny.onInputChange('rows', $(this).add('tr td input:checkbox:checked').parent().siblings(':last-child').map(function() {
          return $(this).text();
          }).get())
          }, 10); 
          });
          }")),escape = FALSE,
                  
    )
  },
  extensions = 'Scroller',
  options = list(
    # dom = 't',
    ordering = TRUE,
    rowCallback = JS("function(r,d) {$(r).attr('height', '20px')}")
  )
  )
}


#INICIAR A APLICACAO
shinyApp(ui,server)
