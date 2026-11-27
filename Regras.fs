/// Regras: concentra as regras de negócio da enfermaria (cálculos, validações
/// e operações). Nenhuma função deste módulo lê o teclado, escreve na tela ou
/// consulta o relógio; cada operação recebe a enfermaria atual e devolve uma
/// nova enfermaria ou um erro.
module Regras

open System
open Dominio

// Aplica o paradigma funcional (expressões de computação) por meio de um
// builder sobre Result: cada let!/do! só continua se o passo anterior for Ok,
// e o primeiro Error interrompe a operação inteira.
type ResultadoBuilder() =
    member _.Bind(r: Result<'a, Erro>, f: 'a -> Result<'b, Erro>) = Result.bind f r
    member _.Return(x: 'a) : Result<'a, Erro> = Ok x

let resultado = ResultadoBuilder()

// Aplica o paradigma funcional (expressões e avaliação) por meio do if/else
// usado como expressão, que sempre produz um valor.
let exigir condicao erro : Result<unit, Erro> = if condicao then Ok() else Error erro

// Aplica o paradigma funcional (casamento de padrões) por meio do match,
// convertendo Option em Result.
let paraResultado erro opcao : Result<'a, Erro> =
    match opcao with
    | Some valor -> Ok valor
    | None -> Error erro

// ---------------------------------------------------------------------
// Cálculos
// ---------------------------------------------------------------------

// Aplica o paradigma funcional (funções puras) por meio do cálculo da idade
// a partir de valores recebidos: a idade nunca é armazenada.
let idade (hoje: DateTime) (nascimento: DateTime) =
    let anos = hoje.Year - nascimento.Year
    if hoje < nascimento.AddYears anos then anos - 1 else anos

// Aplica o paradigma funcional (funções puras) por meio do cálculo da
// situação no momento da consulta, em vez de guardar uma marcação mutável.
let situacao (agora: DateTime) (prescricao: Prescricao) =
    if agora > prescricao.ValidaAte then Vencida else Ativa

// Aplica o paradigma funcional (composição de funções) por meio do operador >>:
// calcula a situação e, em seguida, compara o resultado com Vencida.
let estaVencida agora = situacao agora >> (=) Vencida

// Aplica o paradigma funcional (casamento de padrões) por meio do match sobre a alta.
let situacaoNoProntuario agora (paciente: Paciente) prescricao =
    match paciente.Alta with
    | Some _ -> Encerrada
    | None -> situacao agora prescricao

/// Gera os horários do aprazamento até o fim da validade.
/// Ex.: início 08:00, a cada 8 h, válida até 07:00 do dia seguinte -> [08:00; 16:00; 00:00]
// Aplica o paradigma funcional (recursão) por meio da função "gerar", com
// recursão de cauda: o acumulador substitui a variável de um laço.
let gerarHorarios (inicio: DateTime) (frequenciaHoras: int) (limite: DateTime) : DateTime list =
    let rec gerar (atual: DateTime) acumulados =
        if atual > limite then
            List.rev acumulados
        else
            gerar (atual.AddHours(float frequenciaHoras)) (atual :: acumulados)

    gerar inicio []

// Aplica o paradigma funcional (casamento de padrões) por meio de match
// aninhados sobre as uniões Profissional e CategoriaEnfermagem.
let podeAprazar profissional =
    match profissional with
    | ProfMedico _ -> true
    | ProfEnfermagem membro ->
        match membro.Categoria with
        | Enfermeiro -> true
        | Tecnico
        | Auxiliar -> false

// ---------------------------------------------------------------------
// Janela de administração (ISMP, 2011)
// ---------------------------------------------------------------------

/// Doses mais frequentes que a cada 4 horas também são de horário crítico
let classificar (prescricao: Prescricao) =
    if prescricao.MarcadoComoCritico || prescricao.FrequenciaHoras < 4 then Critico
    elif prescricao.FrequenciaHoras < 24 then MaisDeUmaVezAoDia
    else DiarioOuMaior

let toleranciaMinutos classificacao =
    match classificacao with
    | Critico -> 30
    | MaisDeUmaVezAoDia -> 60
    | DiarioOuMaior -> 120

// Aplica o paradigma funcional (ausência de valor sem null) por meio do tipo
// Option: devolve Some alerta quando sai da janela e None quando está dentro.
let verificarJanela (prescricao: Prescricao) (horarioAprazado: DateTime) (realizadaEm: DateTime) =
    match prescricao.Item with
    | Cuidado _ -> None
    | Medicamento _ ->
        let classificacao = classificar prescricao
        let tolerancia = toleranciaMinutos classificacao
        let diferenca = int (realizadaEm - horarioAprazado).TotalMinutes

        if abs diferenca > tolerancia then
            Some
                { DiferencaMinutos = diferenca
                  ToleranciaMinutos = tolerancia
                  Classificacao = classificacao }
        else
            None

// Aplica o paradigma funcional (funções de ordem superior) por meio de
// Seq.forall e String.filter, que recebem a função Char.IsDigit.
let cpfValido (cpf: string) = cpf.Length = 11 && Seq.forall Char.IsDigit cpf

let somenteDigitos (texto: string) = String.filter Char.IsDigit texto

// Aplica o paradigma funcional (tratamento de dados por transformações) por
// meio do pipeline |> com List.filter, List.map e Set.ofList.
let horariosPendentes (prescricao: Prescricao) (administracoes: Administracao list) =
    match prescricao.Aprazamento with
    | None -> []
    | Some aprazamento ->
        let realizados =
            administracoes
            |> List.filter (fun a -> a.PrescricaoId = prescricao.Id)
            |> List.map (fun a -> a.HorarioAprazado)
            |> Set.ofList

        aprazamento.Horarios
        |> List.filter (fun horario -> not (Set.contains horario realizados))

// ---------------------------------------------------------------------
// Buscas
// ---------------------------------------------------------------------

// Aplica o paradigma funcional (funções de ordem superior) por meio de
// List.tryFind, que recebe uma função anônima como critério de busca.
let buscarPaciente prontuario (enfermaria: Enfermaria) =
    enfermaria.Pacientes
    |> List.tryFind (fun p -> p.Prontuario = prontuario)
    |> paraResultado (PacienteNaoEncontrado prontuario)

let buscarInternado prontuario enfermaria =
    resultado {
        let! paciente = buscarPaciente prontuario enfermaria
        do! exigir paciente.Alta.IsNone (PacienteJaTemAlta prontuario)
        return paciente
    }

let buscarMedico crm (enfermaria: Enfermaria) =
    enfermaria.Medicos
    |> List.tryFind (fun m -> m.Crm = crm)
    |> paraResultado (MedicoNaoEncontrado crm)

// Aplica o paradigma funcional (funções de primeira classe) por meio dos casos
// ProfMedico e ProfEnfermagem, passados como funções para Option.map.
let buscarProfissional registro (enfermaria: Enfermaria) =
    let medico =
        enfermaria.Medicos
        |> List.tryFind (fun m -> m.Crm = registro)
        |> Option.map ProfMedico

    let membro =
        enfermaria.Equipe
        |> List.tryFind (fun e -> e.Coren = registro)
        |> Option.map ProfEnfermagem

    Option.orElse membro medico
    |> paraResultado (ProfissionalNaoEncontrado registro)

let buscarPrescricao id (paciente: Paciente) =
    paciente.Prescricoes
    |> List.tryFind (fun p -> p.Id = id)
    |> paraResultado (PrescricaoNaoEncontrada id)

// ---------------------------------------------------------------------
// Substituição de valores
// ---------------------------------------------------------------------

// Aplica o paradigma funcional (imutabilidade) por meio da expressão
// { ... with ... } e de List.map: cria uma nova enfermaria com o paciente
// trocado, sem modificar a lista original.
let substituirPaciente (novo: Paciente) (enfermaria: Enfermaria) =
    { enfermaria with
        Pacientes =
            enfermaria.Pacientes
            |> List.map (fun p -> if p.Prontuario = novo.Prontuario then novo else p) }

let substituirPrescricao (nova: Prescricao) (paciente: Paciente) =
    { paciente with
        Prescricoes =
            paciente.Prescricoes
            |> List.map (fun p -> if p.Id = nova.Id then nova else p) }

// ---------------------------------------------------------------------
// Validações do cadastro, usadas campo a campo pela tela e no cadastro final.
// A enfermaria é o primeiro parâmetro para permitir aplicação parcial:
// (validarCpf enfermaria) é uma função que só espera o CPF.
// ---------------------------------------------------------------------

let private internados (enfermaria: Enfermaria) =
    enfermaria.Pacientes |> List.filter (fun p -> p.Alta.IsNone)

let mesmoLeito (a: string) (b: string) =
    String.Equals(a.Trim(), b.Trim(), StringComparison.OrdinalIgnoreCase)

// Aplica o paradigma funcional (expressões de computação) por meio do bloco
// resultado { }, que encadeia as validações e para no primeiro erro.
let validarProntuario (enfermaria: Enfermaria) prontuario =
    resultado {
        do! exigir (prontuario > 0) (DadoInvalido "o número do prontuário deve ser positivo")
        do! exigir
                (enfermaria.Pacientes |> List.forall (fun p -> p.Prontuario <> prontuario))
                (ProntuarioDuplicado prontuario)
        return prontuario
    }

/// Quem já recebeu alta pode ser internado novamente com o mesmo CPF
let validarCpf (enfermaria: Enfermaria) cpf =
    resultado {
        do! exigir (cpfValido cpf) (DadoInvalido "o CPF deve ter 11 dígitos numéricos")
        do! exigir (internados enfermaria |> List.forall (fun p -> p.Cpf <> cpf)) (CpfJaInternado cpf)
        return cpf
    }

let validarLeito (enfermaria: Enfermaria) leito =
    resultado {
        do! exigir (internados enfermaria |> List.forall (fun p -> not (mesmoLeito p.Leito leito))) (LeitoOcupado leito)
        return leito
    }

let validarNascimento (agora: DateTime) nascimento =
    resultado {
        do! exigir (nascimento <= agora) (DadoInvalido "a data de nascimento não pode estar no futuro")
        return nascimento
    }

// ---------------------------------------------------------------------
// Operações. Todas devolvem Result<Enfermaria, Erro>.
// ---------------------------------------------------------------------

// Aplica o paradigma funcional (imutabilidade) por meio do operador @, que
// cria uma nova lista com o paciente acrescentado, sem alterar a anterior.
let cadastrarPaciente (agora: DateTime) (novo: Paciente) (enfermaria: Enfermaria) =
    resultado {
        let! _ = validarProntuario enfermaria novo.Prontuario
        do! exigir (not (String.IsNullOrWhiteSpace novo.Nome)) (DadoInvalido "o nome é obrigatório")
        let! _ = validarNascimento agora novo.DataNascimento
        let! _ = validarCpf enfermaria novo.Cpf
        let! _ = validarLeito enfermaria novo.Leito
        return { enfermaria with Pacientes = enfermaria.Pacientes @ [ novo ] }
    }

// Aplica o paradigma funcional (imutabilidade) por meio de { paciente with ... }:
// a prescrição é acrescentada a uma cópia do paciente.
let prescrever (agora: DateTime) prontuario item marcadoComoCritico frequenciaHoras descricao crm (validaAte: DateTime) enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! _ = buscarMedico crm enfermaria
        do! exigir (frequenciaHoras > 0) (DadoInvalido "a frequência deve ser maior que zero")
        do! exigir (validaAte > agora) (DadoInvalido "a validade deve ser posterior ao momento da prescrição")

        let nova =
            { Id = List.length paciente.Prescricoes + 1
              Item = item
              MarcadoComoCritico = marcadoComoCritico
              FrequenciaHoras = frequenciaHoras
              Descricao = descricao
              CrmPrescritor = crm
              PrescritaEm = agora
              ValidaAte = validaAte
              Aprazamento = None }

        let pacienteAtualizado = { paciente with Prescricoes = paciente.Prescricoes @ [ nova ] }
        return substituirPaciente pacienteAtualizado enfermaria
    }

let aprazar (agora: DateTime) prontuario idPrescricao registro (inicio: DateTime) enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! prescricao = buscarPrescricao idPrescricao paciente
        do! exigir (not (estaVencida agora prescricao)) (PrescricaoVencida idPrescricao)
        do! exigir prescricao.Aprazamento.IsNone (PrescricaoJaAprazada idPrescricao)
        let! profissional = buscarProfissional registro enfermaria
        do! exigir (podeAprazar profissional) (SemPermissao "apenas enfermeiros e médicos podem aprazar")
        do! exigir (inicio <= prescricao.ValidaAte) (DadoInvalido "o horário inicial está fora da validade da prescrição")

        let horarios = gerarHorarios inicio prescricao.FrequenciaHoras prescricao.ValidaAte

        let aprazada =
            { prescricao with
                Aprazamento =
                    Some
                        { Horarios = horarios
                          AprazadoPor = registro
                          AprazadoEm = agora } }

        return substituirPaciente (substituirPrescricao aprazada paciente) enfermaria
    }

/// Os horários já realizados são mantidos; os pendentes são substituídos pelos novos
// Aplica o paradigma funcional (tratamento de dados por transformações) por
// meio do pipeline |> com List.distinct e List.sort sobre os horários.
let reaprazar (agora: DateTime) prontuario idPrescricao registro (inicio: DateTime) enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! prescricao = buscarPrescricao idPrescricao paciente
        do! exigir (not (estaVencida agora prescricao)) (PrescricaoVencida idPrescricao)
        let! aprazamentoAtual = prescricao.Aprazamento |> paraResultado (PrescricaoNaoAprazada idPrescricao)
        let! profissional = buscarProfissional registro enfermaria
        do! exigir (podeAprazar profissional) (SemPermissao "apenas enfermeiros e médicos podem aprazar")
        do! exigir (inicio <= prescricao.ValidaAte) (DadoInvalido "o horário inicial está fora da validade da prescrição")

        let realizados =
            paciente.Administracoes
            |> List.filter (fun a -> a.PrescricaoId = idPrescricao)
            |> List.map (fun a -> a.HorarioAprazado)

        let ultimaAdministracao =
            paciente.Administracoes
            |> List.filter (fun a -> a.PrescricaoId = idPrescricao)
            |> List.map (fun a -> a.RealizadaEm)
            |> List.sortDescending
            |> List.tryHead

        do! exigir
                (ultimaAdministracao |> Option.forall (fun ultima -> inicio > ultima))
                (DadoInvalido "o novo horário deve ser posterior à última administração")

        let novosHorarios =
            realizados @ gerarHorarios inicio prescricao.FrequenciaHoras prescricao.ValidaAte
            |> List.distinct
            |> List.sort

        let reaprazada =
            { prescricao with
                Aprazamento =
                    Some
                        { aprazamentoAtual with
                            Horarios = novosHorarios
                            AprazadoPor = registro
                            AprazadoEm = agora } }

        return substituirPaciente (substituirPrescricao reaprazada paciente) enfermaria
    }

// Aplica o paradigma funcional (imutabilidade) por meio do operador @: o novo
// registro é acrescentado e os anteriores nunca são alterados ou removidos.
let registrarAdministracao (agora: DateTime) prontuario idPrescricao horarioAprazado registro (realizadaEm: DateTime) observacao enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! prescricao = buscarPrescricao idPrescricao paciente
        do! exigir (not (estaVencida agora prescricao)) (PrescricaoVencida idPrescricao)
        do! exigir prescricao.Aprazamento.IsSome (PrescricaoNaoAprazada idPrescricao)
        do! exigir
                (horariosPendentes prescricao paciente.Administracoes |> List.contains horarioAprazado)
                (HorarioNaoPendente horarioAprazado)
        let! _ = buscarProfissional registro enfermaria
        do! exigir (realizadaEm <= agora) (DadoInvalido "o horário de realização não pode estar no futuro")

        let registroNovo =
            { PrescricaoId = idPrescricao
              HorarioAprazado = horarioAprazado
              RealizadaEm = realizadaEm
              RealizadaPor = registro
              Observacao = observacao }

        let pacienteAtualizado =
            { paciente with Administracoes = paciente.Administracoes @ [ registroNovo ] }

        return substituirPaciente pacienteAtualizado enfermaria
    }

// Aplica o paradigma funcional (imutabilidade) por meio de { paciente with ... }:
// o leito muda em uma cópia do paciente e a transferência é acrescentada ao histórico.
let transferirLeito (agora: DateTime) prontuario (novoLeito: string) registro enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! _ = buscarProfissional registro enfermaria
        do! exigir (not (String.IsNullOrWhiteSpace novoLeito)) (DadoInvalido "o leito é obrigatório")
        do! exigir (not (mesmoLeito paciente.Leito novoLeito)) (DadoInvalido "o paciente já está nesse leito")
        let! _ = validarLeito enfermaria novoLeito

        let transferencia =
            { LeitoAnterior = paciente.Leito
              LeitoNovo = novoLeito.Trim()
              TransferidoEm = agora
              TransferidoPor = registro }

        let pacienteAtualizado =
            { paciente with
                Leito = novoLeito.Trim()
                Transferencias = paciente.Transferencias @ [ transferencia ] }

        return substituirPaciente pacienteAtualizado enfermaria
    }

// Aplica o paradigma funcional (casamento de padrões) por meio do match que
// permite a anotação apenas para a equipe de enfermagem.
let registrarAnotacao (agora: DateTime) prontuario registro (ocorridaEm: DateTime) (texto: string) enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! profissional = buscarProfissional registro enfermaria

        do! exigir
                (match profissional with
                 | ProfEnfermagem _ -> true
                 | ProfMedico _ -> false)
                (SemPermissao "anotações de enfermagem são feitas pela equipe de enfermagem")

        do! exigir (not (String.IsNullOrWhiteSpace texto)) (DadoInvalido "a anotação não pode ficar vazia")
        do! exigir (ocorridaEm <= agora) (DadoInvalido "a intercorrência não pode estar no futuro")

        let anotacao =
            { OcorridaEm = ocorridaEm
              AnotadaEm = agora
              AnotadaPor = registro
              Texto = texto.Trim() }

        return substituirPaciente { paciente with Anotacoes = paciente.Anotacoes @ [ anotacao ] } enfermaria
    }

let darAlta (agora: DateTime) prontuario crm resumo enfermaria =
    resultado {
        let! paciente = buscarInternado prontuario enfermaria
        let! _ = buscarMedico crm enfermaria
        do! exigir (not (String.IsNullOrWhiteSpace resumo)) (DadoInvalido "o resumo de alta é obrigatório")

        let alta =
            { DataAlta = agora
              CrmAlta = crm
              Resumo = resumo }

        return substituirPaciente { paciente with Alta = Some alta } enfermaria
    }

// Aplica o paradigma funcional (casamento de padrões) por meio de um match
// exaustivo: se um novo erro for criado e não tratado aqui, o compilador avisa.
let descreverErro erro =
    match erro with
    | ProntuarioDuplicado n -> sprintf "já existe um paciente com o prontuário %d" n
    | CpfJaInternado cpf -> sprintf "já existe um paciente internado com o CPF %s" cpf
    | LeitoOcupado leito -> sprintf "o leito %s já está ocupado" leito
    | PacienteNaoEncontrado n -> sprintf "paciente com prontuário %d não encontrado" n
    | PacienteJaTemAlta n -> sprintf "o paciente %d já recebeu alta" n
    | PrescricaoNaoEncontrada n -> sprintf "prescrição %d não encontrada" n
    | PrescricaoVencida n -> sprintf "a prescrição %d está vencida e precisa ser renovada" n
    | PrescricaoJaAprazada n -> sprintf "a prescrição %d já foi aprazada" n
    | PrescricaoNaoAprazada n -> sprintf "a prescrição %d ainda não foi aprazada" n
    | HorarioNaoPendente h -> sprintf "o horário %s não está pendente" (h.ToString "dd/MM HH:mm")
    | MedicoNaoEncontrado crm -> sprintf "médico com CRM %s não encontrado" crm
    | ProfissionalNaoEncontrado r -> sprintf "profissional com registro %s não encontrado" r
    | SemPermissao motivo -> sprintf "sem permissão: %s" motivo
    | DadoInvalido motivo -> sprintf "dado inválido: %s" motivo
