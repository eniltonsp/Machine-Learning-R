#packs

#install.packages("shinydashboard")

library(shiny)
library(shinydashboard)
library(tidyverse)
library(tidymodels)


#modelo

final_mdl_xgb_ra <- readRDS("C:/Users/enilt/Documents/Fiap bigdata/Zeeng/fase 5/Trabalho Final/R/ReclameAqui_model.rds")

final_mdl_xgb_ra_emp <- readRDS("C:/Users/enilt/Documents/Fiap bigdata/Zeeng/fase 5/Trabalho Final/R/ReclameAqui_model_empresa.rds")



# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Zeeng Machine Learning Reclame Aqui"),
    dashboardSidebar(
         menuItem(
            text = "Analise por segmento",
            tabName = "tab_a"
         # icon = icon("ReclameAqui-alt")
        ),
        menuItem(
            text = "Analise por Empresa",
            tabName = "tab_b"
            # icon = icon("ReclameAqui-alt")
        )
        
   ),
    dashboardBody(
        tabItems(  
              tabItem(
                    tabName = "tab_a",
                    valueBoxOutput("prediction_ra"),
                    box(selectInput("v_ramo", label = "Segmento Empresa", choices = c("Varejo","Financeiro"))),
                    box(sliderInput("v_nota_cli", label = "Nota final do cliente", min = 0, max = 10, value = 5)),
                    box(sliderInput("v_qtd_dias_emp_resp", label = "Quantidade de dias para responder", min = 0, max = 100, value = 10)),
                    box(sliderInput("v_qtd_dias_conclusao", label = "Quantidade de dias para conclusão", min = 0, max = 100, value = 10)),
                    box(selectInput("v_status", label = "Reclamacao Status", choices = c("Resolvido","Não Resolvido"))),
                    box(selectInput("v_estado", label = "Estado", choices = c("Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal",
                    "Espírito Santo","Goiás", "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí","Rio de Janeiro",
                    "Rio Grande do Norte", "Rio Grande do Sul","Rondônia","Roraima","Santa Catarina", "São Paulo","Sergipe", "Tocantins")))
                    ),
               tabItem(
                   tabName = "tab_b",
                   valueBoxOutput("prediction_ra_emp"),
                   box(selectInput("v_ramo", label = "Segmento Empresa", choices = c("Varejo","Financeiro"))),
                   box(selectInput("v_empresa", label = "Empresa", choices = c("Banco Bradesco","Banco do Brasil","Banco Itaú","Banco Next",
                                                                               "Banco Santander","C6 Bank","Nubank","Sicredi","Angeloni - Loja Física",
                                                                               "Assai Atacadista","Atacadão - Lojas Físicas","BIG - Lojas Físicas",
                                                                               "Carrefour - Loja Física","Extra - Lojas Físicas","Fort Atacadista",
                                                                               "Pão de Açúcar - Lojas Físicas","WalMart - Loja Física"))),           
                   box(sliderInput("v_nota_cli", label = "Nota final do cliente", min = 0, max = 10, value = 5)),
                   box(sliderInput("v_qtd_dias_emp_resp", label = "Quantidade de dias para responder", min = 0, max = 100, value = 10)),
                   box(sliderInput("v_qtd_dias_conclusao", label = "Quantidade de dias para conclusão", min = 0, max = 100, value = 10)),
                   box(selectInput("v_status", label = "Reclamacao Status", choices = c("Resolvido","Não Resolvido"))),
                   box(selectInput("v_estado", label = "Estado", choices = c("Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Distrito Federal",
                                                                             "Espírito Santo","Goiás", "Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí","Rio de Janeiro",
                                                                             "Rio Grande do Norte", "Rio Grande do Sul","Rondônia","Roraima","Santa Catarina", "São Paulo","Sergipe", "Tocantins")))
        
                  )
            )
       )  
)

server <- function(input,output){
    output$prediction_ra <- renderValueBox({
        
        pred_class_ra <- predict(final_mdl_xgb_ra, 
                                 new_data = tibble(
                                     "segmento" = input$v_ramo,
                                     "status" = input$v_status,
                                     "estado" = input$v_estado,
                                     "qtd_dias_resposta" = input$v_qtd_dias_emp_resp,
                                     "qtd_dias_conclusao" = input$v_qtd_dias_conclusao,                                 
                                     "nota_cliente" = input$v_nota_cli
                                 ))
        
        pred_prob_ra <- predict(final_mdl_xgb_ra, 
                                new_data = tibble(
                                    "segmento" = input$v_ramo,
                                    "status" = input$v_status,
                                    "estado" = input$v_estado,
                                    "qtd_dias_resposta" = input$v_qtd_dias_emp_resp,
                                    "qtd_dias_conclusao" = input$v_qtd_dias_conclusao,                                 
                                    "nota_cliente" = input$v_nota_cli
                                ),
                                type = "prob" 
        ) %>%
            pivot_longer(cols = everything()) %>%
            arrange(desc(value)) %>%
            dplyr::slice(1) %>%
            select(value)
        
        pred_class_color <- if_else(pred_class_ra$.pred_class =="Sim","green","red")
        
        valueBox(value = paste0(round(100*pred_prob_ra$value,0),"%"),
                 subtitle = paste0("Voltaria a fazer negocio? ", pred_class_ra$.pred_class),
                 color = pred_class_color)
    })    
    
    output$prediction_ra_emp <- renderValueBox({
        
    pred_class_ra_emp <- predict(final_mdl_xgb_ra_emp, 
                             new_data = tibble(
                                 "empresa" = input$v_empresa,
                                 "segmento" = input$v_ramo,
                                 "status" = input$v_status,
                                 "estado" = input$v_estado,
                                 "qtd_dias_resposta" = input$v_qtd_dias_emp_resp,
                                 "qtd_dias_conclusao" = input$v_qtd_dias_conclusao,                                 
                                 "nota_cliente" = input$v_nota_cli
                            ))
        
    pred_prob_ra_emp <- predict(final_mdl_xgb_ra_emp, 
                              new_data = tibble(
                                  "empresa" = input$v_empresa,                                  
                                  "segmento" = input$v_ramo,
                                  "status" = input$v_status,
                                  "estado" = input$v_estado,
                                  "qtd_dias_resposta" = input$v_qtd_dias_emp_resp,
                                  "qtd_dias_conclusao" = input$v_qtd_dias_conclusao,                                 
                                  "nota_cliente" = input$v_nota_cli
                              ),
                              type = "prob" 
                            ) %>%
                    pivot_longer(cols = everything()) %>%
                    arrange(desc(value)) %>%
                    dplyr::slice(1) %>%
                    select(value)
                    
                    pred_class_color <- if_else(pred_class_ra$.pred_class =="Sim","green","red")
                    
                    valueBox(value = paste0(round(100*pred_prob_ra$value,0),"%"),
                             subtitle = paste0("Voltaria a fazer negocio? ", pred_class_ra$.pred_class),
                             color = pred_class_color)
    })
    
}
    
shinyApp(ui, server)