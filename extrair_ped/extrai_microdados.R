# Instalar e carregar os pacotes necessários
install.packages("httr")
install.packages("utils")
library(httr)
library(utils)

# Diretório
pasta <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(pasta)

dir.create("dados_ped")

diretorio <- paste0(pasta,"/dados_ped")

#Função para baixar e extrair arquivos
baixar_e_extrair <- function(ano) {
  
  if (ano < 2017) {
    
    print(paste0("Extraindo dados do ano de ",ano))
    url <- paste0("https://www.dieese.org.br/analiseped/", ano, "/", ano, "microdadospedbsb.rar")
    arquivo <- paste0("microdados_", ano, ".rar")
    
    # Baixar o arquivo
    GET(url, write_disk(arquivo, overwrite = TRUE))
    
    # Extrair arquivos .txt
    untar(arquivo, files = grep("*.sav$", iconv(untar(arquivo, list = TRUE), from = "latin1", to = "UTF-8"),value = TRUE), exdir = diretorio)
    
    # Remover o arquivo .rar
    unlink(arquivo)
    
  }
  
  else {
    print("Esse ano não tem")
  }

}

# Loop para baixar e extrair os dados de 2009 a 2016
lapply(2016:2017,baixar_e_extrair)
