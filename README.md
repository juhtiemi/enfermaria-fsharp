# Enfermaria em F#

Sistema de controle de uma enfermaria hospitalar desenvolvido em **F#** com o
**paradigma funcional**, como trabalho da disciplina de Linguagens de
Programação (UNESPAR, Ciência da Computação).

Autoras: Júlia Tiemi Moribe Kato e Marielly Deise Rodrigues Tiago.

## Funcionalidades

**Operações**
1. Cadastrar paciente (cada campo é validado ao ser digitado)
2. Prescrever medicamento ou cuidado (com validade e indicação de horário crítico)
3. Aprazar / reaprazar prescrição (apenas enfermeiros e médicos)
4. Registrar administração (alerta quando sai da janela recomendada pelo ISMP)
5. Transferir de leito (registrada automaticamente no prontuário)
6. Registrar anotação de enfermagem (intercorrências)
7. Dar alta

**Consultas**
8. Listar profissionais (médicos e equipe de enfermagem)
9. Listar pacientes (internados e com alta)
10. Consultar prontuário
11. Consultar prescrições de um paciente (ativas, realizadas, pendentes e aguardando aprazamento)
12. Horários pendentes da enfermaria

Digite 0 em qualquer campo para voltar ao menu.

O sistema já inicia com 3 médicos, 4 profissionais de enfermagem e 10
pacientes de demonstração (5 internados e 5 com alta registrada). Os dados ficam em memória enquanto o programa está aberto.

## Como executar

1. Instale o [.NET SDK 10](https://dotnet.microsoft.com/download) ou superior.
2. Clone o repositório:
   ```
   git clone https://github.com/SEU-USUARIO/enfermaria-fsharp.git
   cd enfermaria-fsharp
   ```
3. Execute:
   ```
   dotnet run
   ```

## Organização do código

Os arquivos são compilados nesta ordem (definida em `Enfermaria.fsproj`):

| Arquivo | Conteúdo |
|---|---|
| `Dominio.fs` | Tipos imutáveis (records e uniões discriminadas) |
| `Regras.fs` | Regras de negócio: funções puras que devolvem `Result` |
| `Dados.fs` | Dados iniciais de demonstração |
| `Consultas.fs` | Consultas: transformações sobre listas |
| `Formatacao.fs` | Montagem dos textos e tabelas exibidos (funções puras) |
| `Entrada.fs` | Leitura do teclado (efeitos colaterais) |
| `Program.fs` | Menu e telas; laço principal recursivo |

A lógica do sistema (`Regras.fs` e `Consultas.fs`) não lê o teclado, não
escreve na tela e não consulta o relógio. Esses efeitos ficam apenas em
`Entrada.fs` e `Program.fs`.
