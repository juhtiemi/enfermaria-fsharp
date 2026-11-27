/// Formatacao: monta os textos exibidos no terminal (menu, tabelas, prontuário
/// e alertas). As funções recebem dados e devolvem texto; quem imprime é o Program.fs.
module Formatacao

open System
open System.Globalization
open Dominio

let private cultura = CultureInfo.InvariantCulture
let dataHora (d: DateTime) = d.ToString("dd/MM/yyyy HH:mm", cultura)
let data (d: DateTime) = d.ToString("dd/MM/yyyy", cultura)
let diaHora (d: DateTime) = d.ToString("dd/MM HH:mm", cultura)

// Aplica o paradigma funcional (casamento de padrões) por meio do match, que
// converte cada caso das uniões discriminadas em texto (via, estadoCivil,
// categoria, item, situacao e classificacao).
let via v =
    match v with
    | Oral -> "Oral"
    | Sublingual -> "Sublingual"
    | Intravenosa -> "Intravenosa"
    | Intramuscular -> "Intramuscular"
    | Subcutanea -> "Subcutânea"
    | Topica -> "Tópica"
    | Inalatoria -> "Inalatória"

let estadoCivil e =
    match e with
    | Solteiro -> "Solteiro(a)"
    | Casado -> "Casado(a)"
    | UniaoEstavel -> "União estável"
    | Divorciado -> "Divorciado(a)"
    | Viuvo -> "Viúvo(a)"

let categoria c =
    match c with
    | Enfermeiro -> "Enfermeiro(a)"
    | Tecnico -> "Técnico(a) de enfermagem"
    | Auxiliar -> "Auxiliar de enfermagem"

let item i =
    match i with
    | Medicamento(nome, dose, v) -> sprintf "%s %s (%s)" nome dose (via v)
    | Cuidado nome -> sprintf "%s (cuidado)" nome

let situacao s =
    match s with
    | Ativa -> "ATIVA"
    | Vencida -> "VENCIDA"
    | Encerrada -> "ENCERRADA"

// Aplica o paradigma funcional (ausência de valor sem null) por meio de
// Option.defaultValue, que usa "-" quando o campo opcional está vazio.
let opcional (texto: string option) = Option.defaultValue "-" texto

/// Ex.: "Paula Mendes (COREN 100001)" ou "Dra. Ana Souza (CRM 12345)"
let profissional registro enfermaria =
    match Regras.buscarProfissional registro enfermaria with
    | Ok(ProfMedico m) -> sprintf "%s (CRM %s)" m.NomeMedico m.Crm
    | Ok(ProfEnfermagem e) -> sprintf "%s (COREN %s)" e.NomeMembro e.Coren
    | Error _ -> registro

let classificacao c =
    match c with
    | Critico -> "de horário crítico"
    | MaisDeUmaVezAoDia -> "administrados mais de uma vez ao dia"
    | DiarioOuMaior -> "diários, semanais ou mensais"

/// Ex.: "±30 min" para um medicamento de horário crítico
let janela (p: Prescricao) =
    match p.Item with
    | Cuidado _ -> "-"
    | Medicamento _ -> sprintf "±%d min" (Regras.toleranciaMinutos (Regras.classificar p))

let alertaJanela (alerta: AlertaJanela) =
    let quando =
        if alerta.DiferencaMinutos > 0 then
            sprintf "%d min DEPOIS" alerta.DiferencaMinutos
        else
            sprintf "%d min ANTES" (abs alerta.DiferencaMinutos)

    [ ""
      sprintf "  [ATENÇÃO] Medicamento administrado %s do horário aprazado." quando
      sprintf "  Para medicamentos %s, a janela recomendada pelo ISMP é de" (classificacao alerta.Classificacao)
      sprintf "  até %d min antes ou depois do horário." alerta.ToleranciaMinutos
      "  Recomenda-se REAPRAZAR os próximos horários (opção 3 do menu)." ]
    |> String.concat "\n"

// Aplica o paradigma funcional (funções de ordem superior) por meio de
// List.mapi, List.map e List.max, que calculam a largura de cada coluna
// e formatam as linhas sem nenhum laço ou variável mutável.
let tabela (cabecalho: string list) (linhas: string list list) =
    if List.isEmpty linhas then
        "  (nenhum registro)"
    else
        let todas = cabecalho :: linhas

        let larguras =
            cabecalho
            |> List.mapi (fun i _ -> todas |> List.map (fun linha -> (List.item i linha).Length) |> List.max)

        let formatarLinha linha =
            linha
            |> List.mapi (fun i celula -> (celula: string).PadRight(List.item i larguras))
            |> String.concat " | "

        let separador =
            larguras |> List.map (fun n -> String('-', n)) |> String.concat "-+-"

        [ formatarLinha cabecalho; separador ] @ List.map formatarLinha linhas
        |> List.map (fun l -> "  " + l)
        |> String.concat "\n"

let titulo (texto: string) =
    let linha = String('=', texto.Length + 4)
    sprintf "\n\n%s\n  %s\n%s\n" linha (texto.ToUpper()) linha

// ---------------------------------------------------------------------
// Telas de consulta
// ---------------------------------------------------------------------

let profissionais (enfermaria: Enfermaria) =
    let tabelaMedicos =
        enfermaria.Medicos
        |> List.map (fun m -> [ m.Crm; m.NomeMedico; m.Especialidade ])
        |> tabela [ "CRM"; "Nome"; "Especialidade" ]

    let tabelaEquipe =
        enfermaria.Equipe
        |> List.map (fun e -> [ e.Coren; e.NomeMembro; categoria e.Categoria ])
        |> tabela [ "COREN"; "Nome"; "Categoria" ]

    sprintf "%s\n%s\n%s\n%s" (titulo "Médicos") tabelaMedicos (titulo "Equipe de enfermagem") tabelaEquipe

// Aplica o paradigma funcional (tratamento de dados por transformações) por
// meio do pipeline |>: a lista de pacientes é transformada em linhas e depois em tabela.
let pacientes agora (enfermaria: Enfermaria) =
    let tabelaInternados =
        Consultas.pacientesInternados enfermaria
        |> List.map (fun p ->
            [ string p.Prontuario
              p.Leito
              p.Nome
              string (Regras.idade agora p.DataNascimento)
              dataHora p.InternadoEm
              p.Diagnostico ])
        |> tabela [ "Prontuário"; "Leito"; "Nome"; "Idade"; "Internado em"; "Diagnóstico" ]

    let tabelaAlta =
        Consultas.pacientesComAlta enfermaria
        |> List.map (fun p ->
            let alta = Option.get p.Alta // seguro: a lista só tem pacientes com alta

            [ string p.Prontuario
              p.Nome
              dataHora p.InternadoEm
              dataHora alta.DataAlta
              profissional alta.CrmAlta enfermaria ])
        |> tabela [ "Prontuário"; "Nome"; "Internado em"; "Alta em"; "Médico da alta" ]

    sprintf
        "%s%s\n%s%s"
        (titulo "Pacientes internados")
        tabelaInternados
        (titulo "Pacientes que receberam alta")
        tabelaAlta

let private linhaPrescricao agora enfermaria (paciente: Paciente) (p: Prescricao) =
    [ string p.Id
      item p.Item
      sprintf "a cada %d h" p.FrequenciaHoras
      janela p
      dataHora p.ValidaAte
      situacao (Regras.situacaoNoProntuario agora paciente p)
      (match p.Aprazamento with
       | Some _ -> "sim"
       | None -> "não")
      profissional p.CrmPrescritor enfermaria
      opcional p.Descricao ]

let private cabecalhoPrescricao =
    [ "Nº"; "Item"; "Frequência"; "Janela"; "Válida até"; "Situação"; "Aprazada"; "Prescritor"; "Descrição" ]

let private tabelaAdministracoes enfermaria (paciente: Paciente) (administracoes: Administracao list) =
    administracoes
    |> List.map (fun a ->
        let nomeItem, situacaoJanela =
            match Regras.buscarPrescricao a.PrescricaoId paciente with
            | Ok p ->
                let noPrazo =
                    match p.Item, Regras.verificarJanela p a.HorarioAprazado a.RealizadaEm with
                    | Cuidado _, _ -> "-"
                    | Medicamento _, None -> "dentro"
                    | Medicamento _, Some _ -> "FORA"

                item p.Item, noPrazo
            | Error _ -> string a.PrescricaoId, "-"

        [ dataHora a.RealizadaEm
          nomeItem
          diaHora a.HorarioAprazado
          sprintf "%+d min" (Consultas.atrasoEmMinutos a)
          situacaoJanela
          profissional a.RealizadaPor enfermaria
          opcional a.Observacao ])
    |> tabela [ "Realizada em"; "Item"; "Aprazado"; "Diferença"; "Janela"; "Responsável"; "Observação" ]

let private tabelaPendencias (pendencias: Consultas.Pendencia list) =
    pendencias
    |> List.map (fun p ->
        [ diaHora p.Horario
          p.Paciente.Leito
          p.Paciente.Nome
          item p.Prescricao.Item
          (if p.Atrasado then "ATRASADO" else "no prazo") ])
    |> tabela [ "Horário"; "Leito"; "Paciente"; "Item"; "Status" ]

let private tabelaTransferencias enfermaria (transferencias: TransferenciaLeito list) =
    transferencias
    |> List.sortBy (fun t -> t.TransferidoEm)
    |> List.map (fun t ->
        [ dataHora t.TransferidoEm
          t.LeitoAnterior
          t.LeitoNovo
          profissional t.TransferidoPor enfermaria ])
    |> tabela [ "Data/hora"; "Leito anterior"; "Novo leito"; "Responsável" ]

/// Exibidas em blocos, e não em tabela, pois o texto costuma ser longo
let private listaAnotacoes enfermaria (anotacoes: AnotacaoEnfermagem list) =
    match anotacoes with
    | [] -> "  (nenhum registro)"
    | _ ->
        anotacoes
        |> List.sortBy (fun a -> a.OcorridaEm)
        |> List.map (fun a ->
            sprintf
                "  Ocorrência: %s  |  Anotada em: %s  |  Por: %s\n    %s"
                (dataHora a.OcorridaEm)
                (dataHora a.AnotadaEm)
                (profissional a.AnotadaPor enfermaria)
                a.Texto)
        |> String.concat "\n\n"

let prontuario agora enfermaria (p: Paciente) =
    let situacaoInternacao =
        match p.Alta with
        | None -> "INTERNADO"
        | Some alta -> sprintf "ALTA em %s" (dataHora alta.DataAlta)

    let dados =
        [ sprintf "  Prontuário:     %d" p.Prontuario
          sprintf "  Nome:           %s" p.Nome
          sprintf "  Nascimento:     %s (%d anos)" (data p.DataNascimento) (Regras.idade agora p.DataNascimento)
          sprintf "  CPF:            %s" p.Cpf
          sprintf "  Estado civil:   %s" (estadoCivil p.EstadoCivil)
          sprintf "  Responsável:    %s" p.Responsavel
          sprintf "  Leito:          %s" p.Leito
          sprintf "  Internado em:   %s" (dataHora p.InternadoEm)
          sprintf "  Diagnóstico:    %s" p.Diagnostico
          sprintf "  Situação:       %s" situacaoInternacao ]
        |> String.concat "\n"

    let prescricoes =
        p.Prescricoes
        |> List.map (linhaPrescricao agora enfermaria p)
        |> tabela cabecalhoPrescricao

    let alta =
        match p.Alta with
        | None -> ""
        | Some a ->
            sprintf
                "%s\n  Data:     %s\n  Médico:   %s\n  Resumo:   %s"
                (titulo "Alta")
                (dataHora a.DataAlta)
                (profissional a.CrmAlta enfermaria)
                a.Resumo

    sprintf
        "%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s%s"
        (titulo "Prontuário")
        dados
        (titulo "Prescrições")
        prescricoes
        (titulo "Histórico de administrações")
        (tabelaAdministracoes enfermaria p (Consultas.administracoesRealizadas p))
        (titulo "Anotações de enfermagem")
        (listaAnotacoes enfermaria p.Anotacoes)
        (titulo "Transferências de leito")
        (tabelaTransferencias enfermaria p.Transferencias)
        (if alta = "" then "" else "\n" + alta)

let prescricoesDoPaciente agora enfermaria (p: Paciente) =
    let ativas =
        Consultas.prescricoesAtivas agora p
        |> List.map (linhaPrescricao agora enfermaria p)
        |> tabela cabecalhoPrescricao

    let semAprazamento =
        Consultas.aguardandoAprazamento agora p
        |> List.map (fun pr ->
            [ string pr.Id; item pr.Item; sprintf "a cada %d h" pr.FrequenciaHoras; dataHora pr.ValidaAte ])
        |> tabela [ "Nº"; "Item"; "Frequência"; "Válida até" ]

    sprintf
        "%s\n  Paciente: %s - leito %s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s"
        (titulo "Prescrições do paciente")
        p.Nome
        p.Leito
        (titulo "Prescrições ativas")
        ativas
        (titulo "Realizadas")
        (tabelaAdministracoes enfermaria p (Consultas.administracoesRealizadas p))
        (titulo "Pendentes")
        (tabelaPendencias (Consultas.pendenciasDoPaciente agora p))
        (titulo "Aguardando aprazamento")
        semAprazamento

let pendenciasEnfermaria agora enfermaria =
    sprintf
        "%s\n%s"
        (titulo "Horários pendentes da enfermaria")
        (tabelaPendencias (Consultas.pendenciasDaEnfermaria agora enfermaria))

let menu (agora: DateTime) =
    [ titulo "Enfermaria - Sistema de Controle"
      sprintf "  Data e hora: %s" (dataHora agora)
      ""
      "  OPERAÇÕES"
      "   1) Cadastrar paciente"
      "   2) Prescrever"
      "   3) Aprazar / reaprazar prescrição"
      "   4) Registrar administração"
      "   5) Transferir de leito"
      "   6) Registrar anotação de enfermagem"
      "   7) Dar alta"
      ""
      "  CONSULTAS"
      "   8) Listar profissionais"
      "   9) Listar pacientes"
      "  10) Consultar prontuário"
      "  11) Consultar prescrições de um paciente"
      "  12) Horários pendentes da enfermaria"
      ""
      "   0) Sair" ]
    |> String.concat "\n"
