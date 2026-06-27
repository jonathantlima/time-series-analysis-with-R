# Análise de Vazões no Alto Vale do Itajaí para Predição de Cheias

## Descrição

Este projeto foi desenvolvido como trabalho da disciplina **INE5649 – Técnicas Estatísticas de Predição**, do Curso de Sistemas de Informação da Universidade Federal de Santa Catarina (UFSC).

O objetivo é analisar a série histórica de vazões do Rio Itajaí-Açu, utilizando dados disponibilizados pela Agência Nacional de Águas e Saneamento Básico (ANA) por meio do sistema HidroWeb. Além da modelagem da série temporal, o projeto também investiga a relação entre precipitação e vazão por meio de modelos de regressão linear.

---

## Objetivos

* Preparar e tratar os dados hidrológicos obtidos no HidroWeb;
* Construir uma série temporal diária de vazões;
* Realizar análise exploratória dos dados;
* Ajustar modelos de previsão de séries temporais;
* Comparar o desempenho dos modelos utilizando métricas estatísticas;
* Avaliar a influência da precipitação na vazão através de regressão linear.

---

## Área de Estudo

* **Rio:** Itajaí-Açu
* **Município:** Rio do Sul – SC
* **Estação fluviométrica:** 83300200
* **Período analisado:** 1978–2023

---

## Estrutura do Projeto

```text
.
├── scripts/
│   ├── 01_preparacao_dados.R
│   ├── 02_localizar_estacoes.R
│   └── 03_analise_series.R
│
├── dados/
│   ├── 83300200_Vazoes.csv
│   ├── 83300200_Vazoes_Tratado.csv
│   └── Estacoes_Pluviometricas.csv
│
└── figuras/
```

---

## Fluxo de Trabalho

### 1. Preparação dos dados

O script `01_preparacao_dados.R` realiza:

* leitura do arquivo original do HidroWeb;
* remoção dos metadados;
* transformação dos dados para o formato:

| Data | Vazao |
| ---- | ----: |

* tratamento de datas inválidas;
* remoção de valores ausentes;
* geração do arquivo tratado;
* geração de gráficos exploratórios.

---

### 2. Seleção das estações pluviométricas

O script `02_localizar_estacoes.R`:

* lê o cadastro das estações pluviométricas;
* calcula a distância até a estação fluviométrica;
* identifica as estações mais próximas;
* gera um mapa de localização;
* exporta a lista das estações candidatas.

---

### 3. Análise estatística

O script `03_analise_series.R` contempla:

* análise exploratória da série;
* decomposição da série temporal;
* análise de autocorrelação;
* ajuste dos modelos de previsão;
* comparação entre modelos;
* avaliação das métricas de desempenho.

Modelos previstos:

* Regressão Linear
* Suavização Exponencial Simples (SES)
* Holt
* Holt-Winters Aditivo
* Holt-Winters Multiplicativo
* ETS

---

## Pacotes Utilizados

* dplyr
* tidyr
* lubridate
* readr
* ggplot2
* forecast
* tseries
* sf

---

## Fonte dos Dados

Agência Nacional de Águas e Saneamento Básico (ANA)

Sistema HidroWeb

https://www.snirh.gov.br/hidroweb/

---

## Produto Final

O projeto gera:

* série histórica tratada;
* gráficos exploratórios;
* mapas das estações pluviométricas;
* modelos estatísticos;
* relatório em LaTeX (Overleaf).

---
