# PAR RUE — Macrorregião Noroeste (PR)

Scripts em R que refazem, a partir dos microdados do SIM e do SIH/SUS, o capítulo
"O perfil de morbimortalidade da Macrorregião Noroeste" do Plano de Ação Regional de
Atenção às Urgências e Emergências (PAR RUE), com dados fornecidos pela SCVGE
(tabulados em 06/11/26). O script também compara cada número recalculado com o valor
publicado no PAR.

Regiões de saúde: 11ª RS Campo Mourão, 12ª RS Umuarama, 13ª RS Cianorte,
14ª RS Paranavaí e 15ª RS Maringá (115 municípios).

## Como rodar

```bash
Rscript instalar_dependencias.R   # uma vez
Rscript baixar_populacao.R        # população por município e sexo, 2015–2025 (Tabnet)
Rscript baixar_datasus.R          # SIM 2015–2025 e SIH 2025 → ~/PAR_RUE_dados
Rscript analise_par.R             # tabelas, gráficos e conferência
Rscript gerar_relatorio.R         # relatório HTML (saidas/relatorio_par_noroeste_2025.html)
```

`baixar_datasus.R` aceita `sim <ano_ini> <ano_fim>` ou `sih <ano>`.
Os anos e os grupos de CID ficam no `BLOCO 0` de `analise_par.R`.

## De onde vêm os dados

| Dado | Fonte | Onde fica |
|---|---|---|
| Óbitos | SIM, `DOPR<ano>.dbc` no FTP do DATASUS (anos fechados em `SIM/CID10/DORES`, preliminares em `SIM/PRELIM/DORES`) | `~/PAR_RUE_dados/sim_<ano>.rds` |
| Internações | SIH/SUS, AIH reduzida `RDPR<aa><mm>.dbc` (`SIHSUS/200801_/Dados`), por mês de competência | `~/PAR_RUE_dados/sih_<ano>.rds` |
| População | Estimativas MS/DATASUS por município e sexo (Tabnet) | `dados_apoio/populacao/` |
| Municípios, RS e macrorregiões | `dados_apoio/municipios_pr_regionais.csv` (copiado do projeto virus-respiratorios) | — |
| Valores publicados no PAR | Transcritos do PDF | `dados_apoio/referencia_par_2025.csv` |

**Base estadual.** O PAR usou a base do SIM da SESA. Para 2025, o arquivo do DATASUS é
preliminar e pode ter menos óbitos. Se você tiver o DBF/DBC estadual, coloque-o em
`~/PAR_RUE_entrada/` com o ano no nome (ex.: `DO_PR_2025.dbf`, `RD_PR_202503.dbf`).
O arquivo local tem prioridade sobre o FTP.

**LGPD.** Os microdados ficam fora do repositório (`~/PAR_RUE_dados`, variável
`PAR_RUE_DADOS`). `baixar_datasus.R` guarda só as colunas necessárias (sem nome, número
da DO/AIH, endereço ou data de nascimento) e nunca imprime registros. As saídas em
`saidas/` têm só contagens e taxas agregadas.

## Saídas

- `saidas/relatorio_par_noroeste_2025.html` — relatório em um único arquivo HTML (abre sem
  internet, pode ser impresso em PDF pelo navegador). Os textos são montados a partir dos
  números: maior e menor taxa entre as RS, diferença em relação à macro, variação na série.
- `saidas/previa_relatorio_valores_par.html` — prévia do mesmo relatório com os valores
  publicados no PAR (`Rscript gerar_relatorio.R --previa-par`), para conferir o formato.

- `saidas/tabelas/par_noroeste_2025.xlsx` — uma aba por quadro, tabela ou gráfico do PAR,
  mais as abas de conferência.
- `saidas/graficos/` — Gráficos 1, 2, 3, 4 e 8.
- `saidas/conferencia_par.csv` — para cada número do PAR: valor recalculado, diferença e
  situação (`confere` = diferença de até 0,5%, `diverge`, `não calculado`).
- `saidas/resultados_longos.csv` — todos os indicadores calculados.

Definições e limitações: ver `METODOLOGIA.md`.
