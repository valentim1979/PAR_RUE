# Metodologia

Descreve o que `analise_par.R` faz. Ao mudar uma definição no script, atualize este
arquivo no mesmo commit e acrescente uma linha no histórico.

## 1. População e recorte

- Recorte por município de **residência**: `CODMUNRES` (SIM) e `MUNIC_RES` (SIH), 6 dígitos,
  cruzados com `dados_apoio/municipios_pr_regionais.csv` (macrorregião Noroeste).
- População: estimativas do Ministério da Saúde por município e sexo (Tabnet, estudo
  2000–2025), uma por ano. Se faltar o ano, usa o mais próximo e avisa no console.
- A população de 2025 baixada bate com a do PAR: macro 1.997.527; 11ª RS 349.296;
  12ª 300.064; 13ª 158.732; 14ª 279.946; 15ª 909.489; homens 974.376; mulheres 1.023.151.

## 2. Óbitos (SIM)

- Ano: ano da data do óbito (`DTOBITO`). O script aborta se menos de 95% dos registros de
  um arquivo forem do ano pedido.
- Causa: causa básica (`CAUSABAS`), agrupada por capítulo da CID-10 (faixas de 3 caracteres).
- Taxa de mortalidade = óbitos / população do ano × 100.000.
- Proporção (Gráfico 1) = óbitos do capítulo / óbitos com capítulo definido no ano.
- Por sexo: `SEXO` 1 = masculino, 2 = feminino; ignorados ficam fora da Tabela 3.
- Idade: código de 3 dígitos do SIM (1º dígito 0–3 = menos de 1 ano; 4 = anos; 5 = 100 anos e mais).

## 3. Grupos de causa

| Indicador | CID-10 | Situação |
|---|---|---|
| Doenças isquêmicas do coração (internação) | I20–I25 | descrito no PAR |
| IAM (óbito e internação) | I21 | descrito no PAR |
| AVC — internação | I64 | descrito no PAR |
| AVC — óbito | I63, I64 | o Quadro 4 cita "I63/I64"; a Tabela 5 e o Gráfico 2 não dizem |
| Outras doenças cerebrovasculares | I60–I69 menos I63, I64 | [Inferência] não descrito no PAR |
| Outras doenças isquêmicas | I20–I25 menos I21 | [Inferência] não descrito no PAR |
| Covid-19 | B34.2, U07.1, U07.2, U10.9 | [Inferência] não descrito no PAR |
| Acidentes de transporte | V01–V99 | [Inferência] |
| Quedas | W00–W19 | [Inferência] |
| Agressões | X85–Y09 | [Inferência] |
| Lesões autoprovocadas | X60–X84 | [Inferência] |
| Relacionado ao trabalho | `ACIDTRAB` = 1 | [Inferência] |
| Tabagismo na DO (câncer de pulmão) | F17 ou Z72.0 em `LINHAA`–`LINHAD` ou `LINHAII` | [Inferência] |

Os grupos marcados com [Inferência] seguem o uso comum dos capítulos da CID-10, mas o PAR
não informa o recorte usado. Confirme com a SCVGE e ajuste a lista `CID` no `BLOCO 0`.

## 4. Internações (SIH/SUS)

- Arquivos RD por mês de **competência** (processamento) do ano de referência.
- Causa: diagnóstico principal (`DIAG_PRINC`).
- Conta só AIH tipo 1 (`IDENT` = 1). A AIH tipo 5 (longa permanência) é continuação de
  uma internação já contada. Parâmetro `SIH_SO_AIH_TIPO1`.
- Taxa de internação = internações / população × 10.000.
- [Não verificado] O PAR não diz se usou ano de competência ou de internação, nem se
  contou só AIH tipo 1.

## 5. Ocupação (Tabela 8)

Grande grupo da CBO 2002 = 1º dígito do código `OCUP` de 6 dígitos. Códigos que começam com
99, vazios ou com outro tamanho vão para "Sem ocupação, aposentado, dona de casa ou
ignorada". A aba "T8 pulmao codigos CBO" lista os códigos para conferir essa regra.
[Não verificado] Não conferi a lista oficial de códigos especiais do SIM (aposentado, dona
de casa etc.).

## 6. Conferência com o PAR

`dados_apoio/referencia_par_2025.csv` traz 508 valores transcritos do PDF (quadros, tabelas e
valores citados no texto dos gráficos). Situação "confere" = diferença de até 0,5% do valor
do PAR (mínimo 0,01).

Divergências internas do próprio PDF, achadas na transcrição:

- Quadro 5: as taxas não batem com óbitos / população do próprio quadro (ex.: macro
  1.581 / 1.997.527 × 100.000 = 79,15, e o quadro mostra 82,2; 15ª RS 657 → 72,24, e o
  quadro mostra 75,9). O total de óbitos (1.581) também difere do Quadro 2 (1.587).
- IAM, macro, 2025: 41,85 no texto do Gráfico 2 e 41,80 na Tabela 4 e no Quadro 3.
- AVC, macro, 2025: 30,94 no texto do Gráfico 2, 30,89 na Tabela 5 e 32,08 no Quadro 4
  (pode ser diferença de CID: I64 × I63+I64).
- C34 em 2025: 323 óbitos na Tabela 6 e 324 na Tabela 7.
- O título do Quadro 2 fala em "mortalidade proporcional", mas o quadro mostra taxas.
- O texto do Quadro 4 cita "11,58/10.000", valor que não aparece no quadro.

## Histórico de alterações

| Data | Alteração |
|---|---|
| 12/11/2026 | Versão inicial: SIM 2015–2025, SIH 2025, Quadros 2–5, Tabelas 3–15, Gráficos 1–4 e 8, conferência com o PAR. |
| 12/11/2026 | Relatório HTML (`gerar_relatorio.R`) e prévia com os valores publicados no PAR. |
