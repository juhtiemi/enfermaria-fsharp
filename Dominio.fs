/// Dominio: define os tipos que representam a enfermaria (profissionais,
/// pacientes, prescrições, administrações, transferências, anotações e erros).
module Dominio

open System

// Aplica o paradigma funcional (tipos algébricos) por meio de uniões
// discriminadas: cada tipo abaixo é um conjunto fechado de casos, e o
// compilador avisa quando algum caso não é tratado em um match.

type Via =
    | Oral
    | Sublingual
    | Intravenosa
    | Intramuscular
    | Subcutanea
    | Topica
    | Inalatoria

type EstadoCivil =
    | Solteiro
    | Casado
    | UniaoEstavel
    | Divorciado
    | Viuvo

type CategoriaEnfermagem =
    | Enfermeiro
    | Tecnico
    | Auxiliar

type Medico =
    { NomeMedico: string
      Crm: string
      Especialidade: string }

type MembroEnfermagem =
    { NomeMembro: string
      Coren: string
      Categoria: CategoriaEnfermagem }

type Profissional =
    | ProfMedico of Medico
    | ProfEnfermagem of MembroEnfermagem

type ItemPrescrito =
    | Medicamento of nome: string * dose: string * via: Via
    | Cuidado of nome: string

// Aplica o paradigma funcional (imutabilidade) por meio de records:
// nenhum campo pode ser alterado depois de criado. Para "alterar" um valor,
// o sistema cria uma cópia com a expressão { valor with Campo = ... }.

type Aprazamento =
    { Horarios: DateTime list
      AprazadoPor: string // CRM ou COREN
      AprazadoEm: DateTime }

// Aplica o paradigma funcional (ausência de valor sem null) por meio do
// tipo Option: Descricao e Aprazamento podem ser Some valor ou None.
type Prescricao =
    { Id: int
      Item: ItemPrescrito
      MarcadoComoCritico: bool // horário crítico segundo o ISMP
      FrequenciaHoras: int
      Descricao: string option
      CrmPrescritor: string
      PrescritaEm: DateTime
      ValidaAte: DateTime
      Aprazamento: Aprazamento option }

type Administracao =
    { PrescricaoId: int
      HorarioAprazado: DateTime
      RealizadaEm: DateTime
      RealizadaPor: string // COREN ou CRM
      Observacao: string option }

/// Janela de administração do ISMP: ±30 min, ±1 h ou ±2 h
type ClassificacaoIsmp =
    | Critico
    | MaisDeUmaVezAoDia
    | DiarioOuMaior

type AlertaJanela =
    { DiferencaMinutos: int // negativo = antes do horário; positivo = depois
      ToleranciaMinutos: int
      Classificacao: ClassificacaoIsmp }

/// A situação não é armazenada: é calculada em Regras.situacao
type SituacaoPrescricao =
    | Ativa
    | Vencida
    | Encerrada

type TransferenciaLeito =
    { LeitoAnterior: string
      LeitoNovo: string
      TransferidoEm: DateTime
      TransferidoPor: string }

type AnotacaoEnfermagem =
    { OcorridaEm: DateTime
      AnotadaEm: DateTime
      AnotadaPor: string // COREN
      Texto: string }

type Alta =
    { DataAlta: DateTime
      CrmAlta: string
      Resumo: string }

type Paciente =
    { Prontuario: int
      Nome: string
      DataNascimento: DateTime
      Cpf: string
      EstadoCivil: EstadoCivil
      Responsavel: string
      Leito: string
      InternadoEm: DateTime
      Diagnostico: string
      Prescricoes: Prescricao list
      Administracoes: Administracao list
      Transferencias: TransferenciaLeito list
      Anotacoes: AnotacaoEnfermagem list
      Alta: Alta option } // None = paciente internado

/// Estado completo do sistema em um determinado momento
type Enfermaria =
    { Medicos: Medico list
      Equipe: MembroEnfermagem list
      Pacientes: Paciente list }

// Aplica o paradigma funcional (tratamento de erros sem exceções) por meio
// de uma união discriminada usada dentro do tipo Result<_, Erro>.
type Erro =
    | ProntuarioDuplicado of int
    | CpfJaInternado of string
    | LeitoOcupado of string
    | PacienteNaoEncontrado of int
    | PacienteJaTemAlta of int
    | PrescricaoNaoEncontrada of int
    | PrescricaoVencida of int
    | PrescricaoJaAprazada of int
    | PrescricaoNaoAprazada of int
    | HorarioNaoPendente of DateTime
    | MedicoNaoEncontrado of string
    | ProfissionalNaoEncontrado of string
    | SemPermissao of string
    | DadoInvalido of string
