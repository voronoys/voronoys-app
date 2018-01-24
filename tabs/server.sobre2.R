output$sobre <- renderUI({
  
  douglas <- wellPanel(
    HTML("<h1>DOUGLAS R. MESQUITA AZEVEDO<h1>
<h3>Possui graduação em Estatística pela Universidade Federal do Rio Grande do Sul - UFRGS (2013), 
Mestrado em Estatística pela Universidade Federal de Minas Gerais - UFMG (2015), 
atualmente é aluno de doutorado em Estatística na UFMG trabalhando com modelos espaciais de 
sobrevivência. Trabalhou no projeto InfoSAS cujo objetivo era a detecção de anomalias 
estatísticas na produção do SUS. É um dos fundadores do Grupo Stats4Good - UFMG. 
Atualmente trabalha no projeto ELSA Brasil cujo objetivo é investigar a incidência 
e os fatores de risco associados à doenças crônicas. 
Possui interesse em estatística computacional, visualização de dados, 
estatística espacial e em problemas que podem ser solucionados através de modelagem estatística.</h3>")
  )
  
  felipe <- wellPanel(
    HTML("<h1>FELIPE NUNES<h1>
<h3>Possui graduação em Ciências Sociais (2006) e mestrado em Ciência Política (2009) pela Universidade 
Federal de Minas Gerais (UFMG). É mestre em Ciência Política pela Universidade da California, 
Los Angeles (UCLA), onde cursou também mestrado em Estatística e o doutorado em Ciência Política. 
É bolsista CAPES/Fulbright e pesquisador do Centro de Estudos Legislativos da UFMG. 
Tem experiência na área de Ciência Política, com ênfase em instituições políticas comparadas e 
desenho de pesquisa, atuando principalmente nos seguintes temas: comportamento parlamentar, 
relação Executivo-Legislativo, política distributiva e partidos na América Latina.</h3>")
  )
  
  lucas <- wellPanel(
    HTML("<h1>LUCAS DA CUNHA GODOY<h1>
<h3>Bacharel em Estatística pela Universidade Federal do Rio Grande do Sul (2016) e atualmente é aluno
de mestrado em Estatística na Universidade Federal de Minas Gerais, onde atualmente trabalha com 
estatística espacial. Tem experiência com geoestatística, análise de dados públicos, análise de 
dados da web e modelos preditivos. Possui interesse em Estatística Aplicada, Estatístca Computacional,
Estatística Espacial e Estatística Bayesiana.</h3>")
  )
  
  luis <- wellPanel(
    HTML("<h1>LUÍS GUSTAVO SILVA E SILVA<h1>
<h3>Possui graduação em Estatística pela Universidade Federal de Juiz de Fora (2010), 
Mestrado em Estatística pela Universidade Federal de Minas Gerais (2012), 
Doutorado em Estatística pela Universidade Federal de Minas Gerais (2017) com ênfase em 
visualização de dados. 
Recebeu o Prêmio deIncentivo em Ciência e Tecnologia para o SUS na 
categoria mestrado (2013). É um dos fundadores do Grupo Stats4Good - 2016. 
Possui interesse em estatística computacional, visualização de dados, 
estatística espacial e em problemas que podem ser solucionados através de modelagem estatística.</h3>")
  )
  
  paste(douglas, br(), felipe, br(), lucas, br(), luis)
  
})