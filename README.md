# O Brasil nos Jogos Olímpicos

### Índice

1. [Instalação](#installation)
2. [Motivação do Projeto](#motivation)
3. [Estrutura dos Arquivos](#files)
4. [Results](#results)

## Instalação <a name="installation"></a>

Para rodar o código do arquivo analise_brasil_olimpiadas.R, basta instalar as duas bibliotecas abaixo, no console do R:
```
install.packages(c("tidyverse", "ggplot2"))
```

## Motivação do Projeto <a name="motivation"></a>

Analisar a participação do Brasil no Jogos Olímpicos de Verão, tendo o detalhamento por gênero. Assim como, a performance do Brasil até 2016 com base nas medalhas conquistadas.


## Estrutura dos Arquivos <a name="files"></a>
<pre>
<code>.
├── <b>README.md</b>
├── <b>analise_brasil_olimpiadas.R </b> : arquivo em R para rodar o projeto
├── <b>dados</b> : contém todos os dados em .csv 
│ ├── <b>athlete_events.csv</b> :  cada linha corresponde a um atleta individual competindo em um evento olímpico individual (eventos-atleta)
│ └── <b>noc_regions.csv</b> :  tabela dinâmica com o nome do comitê e observações
└── <b>graficos</b>: pasta com os gráficos criados
 </code>
</pre>


## Resultados <a name="results"></a>

Os resultados estão disponíveis no Medium [aqui](https://matheusvclls.medium.com/o-brasil-nos-jogos-ol%C3%ADmpicos-6bc7d56cbc9).
