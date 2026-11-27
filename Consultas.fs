/// Consultas: obtém as informações exibidas nas telas de consulta (pacientes,
/// prescrições ativas, administrações realizadas e horários pendentes),
/// sem modificar nenhum dado.
module Consultas

open System
open Dominio

/// Um horário aprazado que ainda não foi realizado
type Pendencia =
    { Paciente: Paciente
      Prescricao: Prescricao
      Horario: DateTime
      Atrasado: bool }

// Aplica o paradigma funcional (composição de funções) por meio do operador >>:
// obtém o campo Alta e, em seguida, verifica se está vazio.
let pacientesInternados (enfermaria: Enfermaria) =
    enfermaria.Pacientes
    |> List.filter ((fun p -> p.Alta) >> Option.isNone)
    |> List.sortBy (fun p -> p.Leito)

let pacientesComAlta (enfermaria: Enfermaria) =
    enfermaria.Pacientes
    |> List.filter ((fun p -> p.Alta) >> Option.isSome)
    |> List.sortBy (fun p -> p.Prontuario)

// Aplica o paradigma funcional (composição de funções e aplicação parcial) por
// meio de Regras.situacao agora >> (=) Ativa, usada como critério do filtro.
let prescricoesAtivas agora (paciente: Paciente) =
    paciente.Prescricoes
    |> List.filter (Regras.situacao agora >> (=) Ativa)

let aguardandoAprazamento agora (paciente: Paciente) =
    prescricoesAtivas agora paciente
    |> List.filter (fun p -> p.Aprazamento.IsNone)

let administracoesRealizadas (paciente: Paciente) =
    paciente.Administracoes
    |> List.sortByDescending (fun a -> a.RealizadaEm)

// Aplica o paradigma funcional (tratamento de dados por transformações) por
// meio do pipeline |> com List.collect, List.map e List.sortBy: cada prescrição
// ativa é transformada na lista de seus horários pendentes.
let pendenciasDoPaciente (agora: DateTime) (paciente: Paciente) =
    prescricoesAtivas agora paciente
    |> List.collect (fun prescricao ->
        Regras.horariosPendentes prescricao paciente.Administracoes
        |> List.map (fun horario ->
            { Paciente = paciente
              Prescricao = prescricao
              Horario = horario
              Atrasado = horario < agora }))
    |> List.sortBy (fun p -> p.Horario)

// Aplica o paradigma funcional (funções de ordem superior) por meio de
// List.collect, que recebe a função pendenciasDoPaciente parcialmente aplicada.
let pendenciasDaEnfermaria agora enfermaria =
    pacientesInternados enfermaria
    |> List.collect (pendenciasDoPaciente agora)
    |> List.sortBy (fun p -> p.Horario)

let atrasoEmMinutos (administracao: Administracao) =
    (administracao.RealizadaEm - administracao.HorarioAprazado).TotalMinutes |> int
