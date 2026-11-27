/// Program: exibe o menu e as telas, coleta os dados digitados, chama as
/// regras de negócio e mostra os resultados. É o ponto de entrada do sistema.
module Program

open System
open Dominio
open Entrada

// Aplica o paradigma funcional (isolamento de efeitos colaterais) por meio
// desta função: o relógio só é consultado aqui, e as regras recebem o horário
// como parâmetro.
let agora () = DateTime.Now

// Aplica o paradigma funcional (casamento de padrões) por meio do match sobre
// Result: em caso de erro, o estado anterior continua valendo.
let concluir mensagemSucesso (anterior: Enfermaria) (resultado: Result<Enfermaria, Erro>) =
    match resultado with
    | Ok nova ->
        escrever ("\n  [OK] " + mensagemSucesso)
        nova
    | Error erro ->
        escrever ("\n  [ERRO] " + Regras.descreverErro erro)
        anterior

/// Se a tela foi cancelada (None), o estado anterior continua valendo
let seCancelada (anterior: Enfermaria) (resultado: Enfermaria option) =
    match resultado with
    | Some nova -> nova
    | None ->
        escrever "\n  Operação cancelada. Voltando ao menu."
        anterior

let limparTela () =
    try
        Console.Clear()
    with _ ->
        () // a saída não é um terminal interativo

let abrirTela nome =
    limparTela ()
    escrever (Formatacao.titulo nome)
    escrever "  (digite 0 em qualquer campo para voltar ao menu)\n"

/// Valida o campo logo após a leitura e, se for inválido, pergunta de novo
// Aplica o paradigma funcional (funções de ordem superior) por meio dos
// parâmetros "ler" e "validar", que são funções.
let rec lerValidado (ler: unit -> 'a option) (validar: 'a -> Result<'a, Erro>) : 'a option =
    match ler () with
    | None -> None
    | Some valor ->
        match validar valor with
        | Ok valido -> Some valido
        | Error erro ->
            escrever ("  [ERRO] " + Regras.descreverErro erro)
            lerValidado ler validar

// ---------------------------------------------------------------------
// Seleções reaproveitadas pelas telas
// ---------------------------------------------------------------------

let escolherPaciente (pacientes: Paciente list) =
    match pacientes with
    | [] ->
        escrever "  Nenhum paciente disponível."
        None
    | _ ->
        pacientes
        |> List.map (fun p -> p.Prontuario, sprintf "%s (leito %s)" p.Nome p.Leito, p)
        |> escolherPorCodigo "Prontuário"

let escolherMedico (enfermaria: Enfermaria) =
    enfermaria.Medicos
    |> List.map (fun m -> sprintf "%s - CRM %s (%s)" m.NomeMedico m.Crm m.Especialidade, m)
    |> escolher "Médico"

/// Devolve o registro (CRM ou COREN) do profissional escolhido
let escolherProfissional (enfermaria: Enfermaria) =
    let medicos =
        enfermaria.Medicos
        |> List.map (fun m -> sprintf "%s - médico(a), CRM %s" m.NomeMedico m.Crm, m.Crm)

    let equipe =
        enfermaria.Equipe
        |> List.map (fun e ->
            sprintf "%s - %s, COREN %s" e.NomeMembro (Formatacao.categoria e.Categoria) e.Coren, e.Coren)

    escolher "Profissional" (medicos @ equipe)

// ---------------------------------------------------------------------
// Telas de operação: cada uma recebe a enfermaria e devolve a nova enfermaria
// ---------------------------------------------------------------------

// Aplica o paradigma funcional (expressões de computação) por meio do bloco
// talvez { }, interrompido se o usuário digitar 0 em qualquer campo.
let telaCadastrarPaciente (enfermaria: Enfermaria) =
    abrirTela "Cadastrar paciente"

    let momento = agora ()

    talvez {
        let! prontuario =
            lerValidado (fun () -> lerInteiro "Número do prontuário") (Regras.validarProntuario enfermaria)

        let! nome = lerObrigatorio "Nome"

        let! nascimento =
            lerValidado (fun () -> lerData "Data de nascimento") (Regras.validarNascimento momento)

        // Aplica o paradigma funcional (composição de funções) por meio do operador >>:
        // lê o texto e, em seguida, remove pontos e traços do CPF.
        let lerCpf = lerObrigatorio >> Option.map Regras.somenteDigitos
        let! cpf = lerValidado (fun () -> lerCpf "CPF") (Regras.validarCpf enfermaria)

        let! estadoCivil =
            escolher
                "Estado civil"
                [ "Solteiro(a)", Solteiro
                  "Casado(a)", Casado
                  "União estável", UniaoEstavel
                  "Divorciado(a)", Divorciado
                  "Viúvo(a)", Viuvo ]

        let! responsavel = lerObrigatorio "Nome do responsável"
        let! leito = lerValidado (fun () -> lerObrigatorio "Leito") (Regras.validarLeito enfermaria)
        let! diagnostico = lerObrigatorio "Diagnóstico / motivo da internação"

        let novo =
            { Prontuario = prontuario
              Nome = nome
              DataNascimento = nascimento
              Cpf = cpf
              EstadoCivil = estadoCivil
              Responsavel = responsavel
              Leito = leito
              InternadoEm = momento
              Diagnostico = diagnostico
              Prescricoes = []
              Administracoes = []
              Transferencias = []
              Anotacoes = []
              Alta = None }

        return
            Regras.cadastrarPaciente momento novo enfermaria
            |> concluir (sprintf "Paciente %s internado no leito %s." nome leito) enfermaria
    }
    |> seCancelada enfermaria

let telaPrescrever (enfermaria: Enfermaria) =
    abrirTela "Prescrever"

    talvez {
        let! paciente = escolherPaciente (Consultas.pacientesInternados enfermaria)
        let! medico = escolherMedico enfermaria
        let! ehMedicamento = escolher "Tipo" [ "Medicamento", true; "Cuidado", false ]

        let! itemPrescrito, critico =
            if ehMedicamento then
                talvez {
                    let! nome = lerObrigatorio "Medicamento"
                    let! dose = lerObrigatorio "Dose (ex.: 500 mg)"

                    let! viaEscolhida =
                        escolher
                            "Via de administração"
                            [ "Oral", Oral
                              "Sublingual", Sublingual
                              "Intravenosa", Intravenosa
                              "Intramuscular", Intramuscular
                              "Subcutânea", Subcutanea
                              "Tópica", Topica
                              "Inalatória", Inalatoria ]

                    let! critico =
                        escolher
                            "Medicamento de horário crítico? (janela ISMP de 30 min)"
                            [ "Não", false; "Sim", true ]

                    return Medicamento(nome, dose, viaEscolhida), critico
                }
            else
                lerObrigatorio "Cuidado (ex.: Curativo, Mudança de decúbito)"
                |> Option.map (fun nome -> Cuidado nome, false)

        let! frequencia = lerInteiro "Frequência (de quantas em quantas horas)"
        let! descricao = lerOpcional "Descrição"
        let! horasValidade = lerInteiroPadrao "Validade em horas" 24
        let momento = agora ()

        return
            Regras.prescrever
                momento
                paciente.Prontuario
                itemPrescrito
                critico
                frequencia
                descricao
                medico.Crm
                (momento.AddHours(float horasValidade))
                enfermaria
            |> concluir "Prescrição registrada." enfermaria
    }
    |> seCancelada enfermaria

/// Apraza uma prescrição sem horários ou reapraza uma já aprazada
let telaAprazar (enfermaria: Enfermaria) =
    abrirTela "Aprazar / reaprazar prescrição"
    let momento = agora ()

    talvez {
        let! paciente = escolherPaciente (Consultas.pacientesInternados enfermaria)

        match Consultas.prescricoesAtivas momento paciente with
        | [] ->
            escrever "  Este paciente não possui prescrições ativas."
            return enfermaria
        | ativas ->
            let! prescricao =
                ativas
                |> List.map (fun p ->
                    let estado =
                        match p.Aprazamento with
                        | None -> "aguardando aprazamento"
                        | Some _ -> "já aprazada: será REAPRAZADA"

                    sprintf "Nº %d - %s, a cada %d h (%s)" p.Id (Formatacao.item p.Item) p.FrequenciaHoras estado, p)
                |> escolher "Prescrição"

            let! registro = escolherProfissional enfermaria
            let proximaHora = DateTime(momento.Year, momento.Month, momento.Day, momento.Hour, 0, 0).AddHours 1.0
            let! inicio = lerDataHora "Horário da próxima administração" proximaHora

            let horarios =
                Regras.gerarHorarios inicio prescricao.FrequenciaHoras prescricao.ValidaAte
                |> List.map Formatacao.diaHora
                |> String.concat ", "

            // Aplica o paradigma funcional (funções de primeira classe) por meio da
            // escolha de Regras.aprazar ou Regras.reaprazar como um valor.
            let operacao, mensagem =
                match prescricao.Aprazamento with
                | None -> Regras.aprazar, "Horários aprazados"
                | Some _ -> Regras.reaprazar, "Horários reaprazados"

            return
                operacao momento paciente.Prontuario prescricao.Id registro inicio enfermaria
                |> concluir (sprintf "%s: %s" mensagem horarios) enfermaria
    }
    |> seCancelada enfermaria

/// Se o registro deu certo e saiu da janela do ISMP, mostra o alerta
let mostrarAlerta (resultado: Result<Enfermaria, Erro>) (alerta: AlertaJanela option) =
    match resultado, alerta with
    | Ok _, Some a -> escrever (Formatacao.alertaJanela a)
    | _ -> ()

let telaRegistrarAdministracao (enfermaria: Enfermaria) =
    abrirTela "Registrar administração"
    let momento = agora ()

    talvez {
        let! paciente = escolherPaciente (Consultas.pacientesInternados enfermaria)

        match Consultas.pendenciasDoPaciente momento paciente with
        | [] ->
            escrever "  Este paciente não possui horários pendentes."
            return enfermaria
        | pendencias ->
            let! pendencia =
                pendencias
                |> List.map (fun p ->
                    let status = if p.Atrasado then " - ATRASADO" else ""
                    sprintf "%s - %s%s" (Formatacao.diaHora p.Horario) (Formatacao.item p.Prescricao.Item) status, p)
                |> escolher "Horário"

            let! registro = escolherProfissional enfermaria
            let! realizadaEm = lerDataHora "Realizado em" momento
            let! observacao = lerOpcional "Observação"

            let resultado =
                Regras.registrarAdministracao
                    momento
                    paciente.Prontuario
                    pendencia.Prescricao.Id
                    pendencia.Horario
                    registro
                    realizadaEm
                    observacao
                    enfermaria

            let novaEnfermaria = concluir "Administração registrada." enfermaria resultado

            mostrarAlerta resultado (Regras.verificarJanela pendencia.Prescricao pendencia.Horario realizadaEm)
            return novaEnfermaria
    }
    |> seCancelada enfermaria

let telaTransferirLeito (enfermaria: Enfermaria) =
    abrirTela "Transferir de leito"

    talvez {
        let! paciente = escolherPaciente (Consultas.pacientesInternados enfermaria)
        escrever (sprintf "\n  Leito atual: %s" paciente.Leito)

        let! novoLeito =
            lerValidado (fun () -> lerObrigatorio "Novo leito") (Regras.validarLeito enfermaria)

        let! registro = escolherProfissional enfermaria

        return
            Regras.transferirLeito (agora ()) paciente.Prontuario novoLeito registro enfermaria
            |> concluir
                (sprintf "%s transferido(a) do leito %s para o leito %s." paciente.Nome paciente.Leito novoLeito)
                enfermaria
    }
    |> seCancelada enfermaria

/// Devolve o COREN do membro da equipe de enfermagem escolhido
let escolherMembroEnfermagem (enfermaria: Enfermaria) =
    enfermaria.Equipe
    |> List.map (fun e -> sprintf "%s - %s, COREN %s" e.NomeMembro (Formatacao.categoria e.Categoria) e.Coren, e.Coren)
    |> escolher "Profissional de enfermagem"

let telaAnotacao (enfermaria: Enfermaria) =
    abrirTela "Registrar anotação de enfermagem"
    let momento = agora ()

    talvez {
        let! paciente = escolherPaciente (Consultas.pacientesInternados enfermaria)
        let! registro = escolherMembroEnfermagem enfermaria
        let! ocorridaEm = lerDataHora "Data e hora da intercorrência" momento
        let! texto = lerObrigatorio "Anotação"

        return
            Regras.registrarAnotacao momento paciente.Prontuario registro ocorridaEm texto enfermaria
            |> concluir "Anotação registrada no prontuário." enfermaria
    }
    |> seCancelada enfermaria

let telaDarAlta (enfermaria: Enfermaria) =
    abrirTela "Dar alta"

    talvez {
        let! paciente = escolherPaciente (Consultas.pacientesInternados enfermaria)
        let! medico = escolherMedico enfermaria
        let! resumo = lerObrigatorio "Resumo de alta"

        return
            Regras.darAlta (agora ()) paciente.Prontuario medico.Crm resumo enfermaria
            |> concluir (sprintf "Alta registrada para %s." paciente.Nome) enfermaria
    }
    |> seCancelada enfermaria

// ---------------------------------------------------------------------
// Telas de consulta
// ---------------------------------------------------------------------

let telaProntuario (enfermaria: Enfermaria) =
    abrirTela "Consultar prontuário"

    match escolherPaciente enfermaria.Pacientes with
    | Some paciente -> escrever (Formatacao.prontuario (agora ()) enfermaria paciente)
    | None -> escrever "\n  Voltando ao menu."

let telaPrescricoesDoPaciente (enfermaria: Enfermaria) =
    abrirTela "Prescrições de um paciente"

    match escolherPaciente (Consultas.pacientesInternados enfermaria) with
    | Some paciente -> escrever (Formatacao.prescricoesDoPaciente (agora ()) enfermaria paciente)
    | None -> escrever "\n  Voltando ao menu."

// ---------------------------------------------------------------------
// Laço principal
// ---------------------------------------------------------------------

// Aplica o paradigma funcional (recursão e imutabilidade) por meio da função
// recursiva executar: em vez de um while com uma variável mutável, cada volta
// do menu recebe o estado atual e chama a si mesma com o novo estado.
let rec executar (enfermaria: Enfermaria) =
    limparTela ()
    escrever (Formatacao.menu (agora ()))

    // Aplica o paradigma funcional (funções de ordem superior) por meio de
    // consultar e operar, que recebem como parâmetro a ação ou a tela a executar.
    let consultar acao =
        limparTela ()
        acao ()
        pausar ()
        executar enfermaria

    let operar tela =
        let nova = tela enfermaria
        pausar ()
        executar nova

    match lerLinha "\nOpção" with
    | "1" -> operar telaCadastrarPaciente
    | "2" -> operar telaPrescrever
    | "3" -> operar telaAprazar
    | "4" -> operar telaRegistrarAdministracao
    | "5" -> operar telaTransferirLeito
    | "6" -> operar telaAnotacao
    | "7" -> operar telaDarAlta
    | "8" -> consultar (fun () -> escrever (Formatacao.profissionais enfermaria))
    | "9" -> consultar (fun () -> escrever (Formatacao.pacientes (agora ()) enfermaria))
    | "10" -> consultar (fun () -> telaProntuario enfermaria)
    | "11" -> consultar (fun () -> telaPrescricoesDoPaciente enfermaria)
    | "12" -> consultar (fun () -> escrever (Formatacao.pendenciasEnfermaria (agora ()) enfermaria))
    | "0" -> escrever "\nSistema encerrado."
    | _ ->
        escrever "  Opção inválida."
        executar enfermaria

[<EntryPoint>]
let main _ =
    // Exibe os acentos corretamente no terminal do Windows
    Console.OutputEncoding <- Text.Encoding.UTF8

    try
        Console.InputEncoding <- Text.Encoding.UTF8
    with _ ->
        ()

    executar (Dados.dadosIniciais (agora ()))
    0
