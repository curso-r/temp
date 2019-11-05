library(httr)
library(rvest)
library(tidyverse)
library(glue)
library(janitor)
library(progress)

######################################################################################
# Scraper do site ada ANEEL

# Função que baixa uma página do site ------------------------------------------------
baixa_html <- function(pagina) {
  url = glue("http://www2.aneel.gov.br/scg/gd/VerGD.asp?pagina={pagina}&acao=buscar&login=&NomPessoa=&IdAgente=&DatConexaoInicio=&DatConexaoFim=")
  nome_do_html = glue("htmls/aneel_pg_{pagina}.html")
  cat(nome_do_html, "\n")
  while(!file.exists(nome_do_html) | file.size(nome_do_html) < 1e4) {
    httr::GET(url, write_disk(nome_do_html, overwrite = TRUE))
    Sys.sleep(0.5)
  }
}

# Procedimento de varrer todas as 131 páginas do site --------------------------------
# as páginas html ficarão salvas na pasta "htmls/"
safe_baixa_html <- purrr::safely(baixa_html, NA)

paginas <- 1:131

pb <- progress::progress_bar$new(total = length(paginas))
purrr::walk(paginas, baixa_html)

# Leitura dos HTMLs e tratamento das tabelas ------------------------------------------
arruma_tabela <- function(tabela_crua) {
  tabela_crua[[1]] %>%
    set_names(.[.[,1] == "Distribuidora",]) %>%
    clean_names() %>%
    dplyr::select(-starts_with("na")) %>%
    dplyr::filter(
      !str_detect(distribuidora, "UNIDADES CONSUMIDORAS|Distribuidora"),
      str_length(distribuidora) < 1000
    ) %>%
    set_names(c("distribuidora", "codigo_da_gd", "titular_da_uc", "classe",
                "subgrupo", "modalidade", "qtd_de_ucs_que_recebem_os_creditos",
                "municipio", "uf", "cep", "data_conexao", "tipo",
                "fonte", "potencia_instalada_kw")) %>%
    mutate(
      potencia_instalada_kw = parse_number(potencia_instalada_kw, locale = locale(decimal_mark = ",")),
      qtd_de_ucs_que_recebem_os_creditos = as.integer(qtd_de_ucs_que_recebem_os_creditos),
      data_conexao = as.Date(data_conexao, format = "%d/%m/%Y")
    )
}
a <- read_html("htmls/aneel_pg_1.html", encoding = "latin1") %>%
  xml_find_all(xpath = "//td[@colspan='16']/parent::node()/../..") %>%
  html_table(fill = TRUE) %>%
  arruma_tabela()


