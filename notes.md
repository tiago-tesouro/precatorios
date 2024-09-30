tem um endpoint para cada tribunal
como localizar os precatórios? usando os códigos de assunto?

Alex:
> Estou com uma necessidade que creio que te auxiliará no seu trabalho com os precatorios tb. Que é conseguir acessar os dados do DATAJUD.

> Acho que no seu caso, vc vai precisar procurar os temas....no meu, eu precisaria buscar todas as partes (autores e réus) das ações...

Classes
1265	Precatório
1677	Precatório


Assuntos
10672	Precatório
14912	Cessão de Créditos
13097	Compensação de Reajustes Concedidos
10680	Crédito Complementar
10679	Fracionamento
11923	Fraude / Quebra de ordem cronológica
13216	Juros de Mora
10885	Liquidação Parcelada
11924	Pagamento
10869	Parcela Incontroversa
13253	Período de Graça
10678	Sequestro de Verbas Públicas

13506	Precatório


Processamento

Ceará, campo assunto inexistente, tratado.
Pernambuco, retornando sem hits.
São Paulo também.

Pernambuco é por causa do formato do código do processo. Estava como numérico, sem leading zeros. Tratado.

SP é porque tem um /XXXX no final que precisa ser ignorado. Tratado. Mas agora tá dando um erro com os códigos de assuntos.

O erro é porque em alguns processos não há o campo "Nome" do assunto.
Deu certo são paulo! Tinha um erro pq alguns processos estão com código de classe inválidos também.

Agora falta TJDFT.

