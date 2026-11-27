/// Dados: fornece os profissionais previamente cadastrados e os pacientes
/// de demonstração (5 internados e 5 com alta registrada).
module Dados

open System
open Dominio

let medicos =
    [ { NomeMedico = "Dra. Ana Souza"; Crm = "12345"; Especialidade = "Clínica Médica" }
      { NomeMedico = "Dr. Carlos Lima"; Crm = "23456"; Especialidade = "Cardiologia" }
      { NomeMedico = "Dra. Beatriz Rocha"; Crm = "34567"; Especialidade = "Infectologia" } ]

let equipe =
    [ { NomeMembro = "Paula Mendes"; Coren = "100001"; Categoria = Enfermeiro }
      { NomeMembro = "Rafael Costa"; Coren = "100002"; Categoria = Enfermeiro }
      { NomeMembro = "Juliana Alves"; Coren = "200001"; Categoria = Tecnico }
      { NomeMembro = "Marcos Silva"; Coren = "300001"; Categoria = Auxiliar } ]

/// Cria uma prescrição. Se houver horário inicial, ela já é criada aprazada.
// Aplica o paradigma funcional (funções de ordem superior) por meio de
// Option.map, que só gera o aprazamento quando há um horário inicial.
let private prescricao id item critico frequencia crm (prescritaEm: DateTime) horasValidade descricao inicio aprazadoPor =
    let validaAte = prescritaEm.AddHours(float horasValidade)

    { Id = id
      Item = item
      MarcadoComoCritico = critico
      FrequenciaHoras = frequencia
      Descricao = descricao
      CrmPrescritor = crm
      PrescritaEm = prescritaEm
      ValidaAte = validaAte
      Aprazamento =
        inicio
        |> Option.map (fun (primeiro: DateTime) ->
            { Horarios = Regras.gerarHorarios primeiro frequencia validaAte
              AprazadoPor = aprazadoPor
              AprazadoEm = prescritaEm.AddMinutes 30.0 }) }

/// Cria o registro de uma administração realizada alguns minutos após o horário aprazado
let private administracao prescricaoId (aprazado: DateTime) minutos registro observacao =
    { PrescricaoId = prescricaoId
      HorarioAprazado = aprazado
      RealizadaEm = aprazado.AddMinutes(float minutos)
      RealizadaPor = registro
      Observacao = observacao }

/// Monta a enfermaria inicial com horários relativos ao momento atual,
/// para que a demonstração sempre tenha itens realizados, atrasados e pendentes.
// Aplica o paradigma funcional (funções puras) por meio do parâmetro
// "agora": a função não consulta o relógio, apenas usa o valor recebido.
let dadosIniciais (agora: DateTime) : Enfermaria =
    let horaCheia = DateTime(agora.Year, agora.Month, agora.Day, agora.Hour, 0, 0)
    let h (horas: float) = horaCheia.AddHours horas

    // ----------------------------- INTERNADOS -----------------------------

    let maria =
        { Prontuario = 1001
          Nome = "Maria Aparecida Oliveira"
          DataNascimento = DateTime(1958, 3, 14)
          Cpf = "12345678901"
          EstadoCivil = Viuvo
          Responsavel = "Carlos Oliveira (filho)"
          Leito = "101"
          InternadoEm = h -50.0
          Diagnostico = "Pneumonia adquirida na comunidade"
          Prescricoes =
            [ prescricao 1 (Medicamento("Ceftriaxona", "1 g", Intravenosa)) true 12 "34567" (h -7.0) 24
                  (Some "Diluir em 100 mL de SF 0,9% e infundir em 30 min") (Some(h -6.0)) "100001"
              prescricao 2 (Medicamento("Paracetamol", "750 mg", Oral)) false 6 "12345" (h -7.0) 24
                  None (Some(h -6.0)) "100001"
              prescricao 3 (Cuidado "Mudança de decúbito") false 2 "12345" (h -7.0) 24
                  (Some "Alternar decúbito lateral direito, esquerdo e dorsal") None ""
              prescricao 4 (Medicamento("Omeprazol", "40 mg", Intravenosa)) false 24 "12345" (h -50.0) 24
                  None (Some(h -49.0)) "100002" ]
          Administracoes =
            [ administracao 4 (h -49.0) 10 "100002" None
              administracao 1 (h -6.0) 15 "200001" (Some "Sem intercorrências")
              administracao 2 (h -6.0) 5 "200001" None ]
          Transferencias = []
          Anotacoes =
            [ { OcorridaEm = h -5.0
                AnotadaEm = h -3.5
                AnotadaPor = "100001"
                Texto = "Paciente apresentou episódio de dispneia leve, saturação de 91% em ar ambiente. Instalado cateter nasal de O2 a 2 L/min, com melhora. Médico plantonista comunicado." } ]
          Alta = None }

    let joao =
        { Prontuario = 1002
          Nome = "João Batista Pereira"
          DataNascimento = DateTime(1971, 11, 2)
          Cpf = "98765432100"
          EstadoCivil = Casado
          Responsavel = "Rosa Pereira (esposa)"
          Leito = "102"
          InternadoEm = h -20.0
          Diagnostico = "Insuficiência cardíaca descompensada"
          Prescricoes =
            [ prescricao 1 (Medicamento("Furosemida", "40 mg", Intravenosa)) false 12 "23456" (h -3.0) 24
                  None (Some(h -2.0)) "100002"
              prescricao 2 (Cuidado "Curativo em membro inferior direito") false 24 "23456" (h -3.0) 24
                  (Some "Limpar com SF 0,9% e cobrir com gaze estéril") None "" ]
          Administracoes = []
          Transferencias =
            [ { LeitoAnterior = "105"; LeitoNovo = "102"; TransferidoEm = h -18.0; TransferidoPor = "100002" } ]
          Anotacoes = []
          Alta = None }

    let antonio =
        { Prontuario = 1004
          Nome = "Antônio Gomes"
          DataNascimento = DateTime(1980, 5, 10)
          Cpf = "32165498700"
          EstadoCivil = Casado
          Responsavel = "Isabel Gomes (esposa)"
          Leito = "104"
          InternadoEm = h -30.0
          Diagnostico = "Pós-operatório de apendicectomia"
          Prescricoes =
            [ prescricao 1 (Medicamento("Cefazolina", "1 g", Intravenosa)) true 8 "34567" (h -10.0) 24
                  None (Some(h -9.0)) "100002"
              prescricao 2 (Medicamento("Dipirona", "1 g", Intravenosa)) false 6 "12345" (h -10.0) 24
                  None (Some(h -9.0)) "100002"
              prescricao 3 (Cuidado "Curativo em ferida operatória") false 24 "12345" (h -10.0) 24
                  (Some "Manter curativo seco e observar sinais de infecção") (Some(h -8.0)) "100002" ]
          Administracoes =
            [ administracao 1 (h -9.0) 10 "200001" None
              administracao 2 (h -9.0) 5 "200001" None
              administracao 3 (h -8.0) 30 "100002" (Some "Ferida limpa, sem secreção")
              administracao 2 (h -3.0) 20 "200001" None
              administracao 1 (h -1.0) 50 "300001" (Some "Acesso venoso perdido; necessária nova punção") ]
          Transferencias = []
          Anotacoes =
            [ { OcorridaEm = h -12.0
                AnotadaEm = h -11.0
                AnotadaPor = "100002"
                Texto = "Paciente refere dor 6/10 em ferida operatória. Medicado conforme prescrição e reavaliado após 1 h, com melhora (dor 2/10)." } ]
          Alta = None }

    let francisca =
        { Prontuario = 1005
          Nome = "Francisca Lima"
          DataNascimento = DateTime(1966, 9, 30)
          Cpf = "74185296300"
          EstadoCivil = Viuvo
          Responsavel = "Débora Lima (filha)"
          Leito = "105"
          InternadoEm = h -40.0
          Diagnostico = "Diabetes mellitus descompensado"
          Prescricoes =
            [ prescricao 1 (Medicamento("Insulina regular", "10 UI", Subcutanea)) true 6 "12345" (h -8.0) 24
                  (Some "Aplicar somente após glicemia capilar") (Some(h -6.0)) "100001"
              prescricao 2 (Cuidado "Glicemia capilar") false 6 "12345" (h -8.0) 24
                  (Some "Registrar o valor e comunicar se acima de 250 mg/dL") (Some(h -6.0)) "100001"
              prescricao 3 (Medicamento("Enalapril", "10 mg", Oral)) false 12 "23456" (h -8.0) 24
                  None (Some(h -4.0)) "100001" ]
          Administracoes =
            [ administracao 2 (h -6.0) 0 "200001" (Some "Glicemia de 212 mg/dL")
              administracao 1 (h -6.0) 10 "200001" None
              administracao 3 (h -4.0) 40 "300001" None ]
          Transferencias = []
          Anotacoes = []
          Alta = None }

    let pedro =
        { Prontuario = 1006
          Nome = "Pedro Henrique Souza"
          DataNascimento = DateTime(1995, 2, 18)
          Cpf = "85296374100"
          EstadoCivil = Solteiro
          Responsavel = "Márcia Souza (mãe)"
          Leito = "106"
          InternadoEm = h -10.0
          Diagnostico = "Celulite em membro inferior esquerdo"
          Prescricoes =
            [ prescricao 1 (Medicamento("Oxacilina", "2 g", Intravenosa)) true 4 "34567" (h -5.0) 24
                  None (Some(h -4.0)) "100001"
              prescricao 2 (Cuidado "Demarcar a área de eritema") false 12 "34567" (h -5.0) 24
                  (Some "Comparar com a marcação anterior e comunicar se houver progressão") None "" ]
          Administracoes = [ administracao 1 (h -4.0) 15 "200001" None ]
          Transferencias = []
          Anotacoes =
            [ { OcorridaEm = h -2.0
                AnotadaEm = h -1.5
                AnotadaPor = "100001"
                Texto = "Paciente com febre de 38,4 °C. Médico plantonista comunicado; aguardando conduta." } ]
          Alta = None }

    // --------------------------- COM ALTA REGISTRADA ---------------------------

    let lucia =
        { Prontuario = 1003
          Nome = "Lúcia Ferreira"
          DataNascimento = DateTime(1990, 6, 25)
          Cpf = "45678912300"
          EstadoCivil = Solteiro
          Responsavel = "Helena Ferreira (mãe)"
          Leito = "103"
          InternadoEm = h -72.0
          Diagnostico = "Infecção do trato urinário"
          Prescricoes =
            [ prescricao 1 (Medicamento("Ciprofloxacino", "400 mg", Intravenosa)) false 12 "34567" (h -71.0) 24
                  None (Some(h -70.0)) "100001" ]
          Administracoes =
            [ administracao 1 (h -70.0) 0 "200001" None
              administracao 1 (h -58.0) 20 "300001" (Some "Paciente em banho no horário") ]
          Transferencias = []
          Anotacoes = []
          Alta =
            Some
                { DataAlta = h -24.0
                  CrmAlta = "12345"
                  Resumo = "Paciente estável, afebril. Concluir antibiótico por via oral em domicílio." } }

    let roberto =
        { Prontuario = 1007
          Nome = "Roberto Almeida"
          DataNascimento = DateTime(1952, 8, 9)
          Cpf = "15975345600"
          EstadoCivil = Casado
          Responsavel = "Marta Almeida (esposa)"
          Leito = "107"
          InternadoEm = h -150.0
          Diagnostico = "DPOC exacerbada"
          Prescricoes =
            [ prescricao 1 (Medicamento("Salbutamol", "100 mcg (2 jatos)", Inalatoria)) false 6 "12345" (h -60.0) 24
                  None (Some(h -59.0)) "100002" ]
          Administracoes =
            [ administracao 1 (h -59.0) 5 "200001" None
              administracao 1 (h -53.0) 10 "200001" None
              administracao 1 (h -47.0) 0 "300001" None
              administracao 1 (h -41.0) 15 "300001" None ]
          Transferencias = []
          Anotacoes = []
          Alta =
            Some
                { DataAlta = h -30.0
                  CrmAlta = "12345"
                  Resumo = "Melhora do padrão respiratório. Alta com broncodilatador inalatório e retorno ambulatorial em 15 dias." } }

    let sandra =
        { Prontuario = 1008
          Nome = "Sandra Martins"
          DataNascimento = DateTime(1985, 1, 17)
          Cpf = "35715925800"
          EstadoCivil = Divorciado
          Responsavel = "Paulo Martins (irmão)"
          Leito = "108"
          InternadoEm = h -100.0
          Diagnostico = "Gastroenterite aguda com desidratação"
          Prescricoes =
            [ prescricao 1 (Medicamento("Soro fisiológico 0,9%", "1000 mL", Intravenosa)) false 8 "12345" (h -99.0) 24
                  None (Some(h -98.0)) "100001"
              prescricao 2 (Medicamento("Ondansetrona", "4 mg", Intravenosa)) false 8 "12345" (h -99.0) 24
                  (Some "Em caso de náusea persistente, comunicar o médico") (Some(h -98.0)) "100001" ]
          Administracoes =
            [ administracao 1 (h -98.0) 0 "200001" None
              administracao 2 (h -98.0) 10 "200001" None
              administracao 1 (h -90.0) 20 "300001" None
              administracao 2 (h -90.0) 20 "300001" None
              administracao 1 (h -82.0) 5 "200001" None ]
          Transferencias = []
          Anotacoes = []
          Alta =
            Some
                { DataAlta = h -60.0
                  CrmAlta = "12345"
                  Resumo = "Tolerando dieta oral e hidratada. Orientada quanto à hidratação e aos sinais de alerta." } }

    let jose =
        { Prontuario = 1009
          Nome = "José Carlos Nunes"
          DataNascimento = DateTime(1960, 12, 3)
          Cpf = "95135745600"
          EstadoCivil = Casado
          Responsavel = "Lúcia Nunes (esposa)"
          Leito = "101"
          InternadoEm = h -200.0
          Diagnostico = "Crise hipertensiva"
          Prescricoes =
            [ prescricao 1 (Medicamento("Captopril", "25 mg", Oral)) false 8 "23456" (h -199.0) 24
                  None (Some(h -198.0)) "100002" ]
          Administracoes =
            [ administracao 1 (h -198.0) 0 "200001" None
              administracao 1 (h -190.0) 30 "200001" None
              administracao 1 (h -182.0) 10 "300001" None ]
          Transferencias = []
          Anotacoes =
            [ { OcorridaEm = h -199.5
                AnotadaEm = h -198.0
                AnotadaPor = "100002"
                Texto = "Pressão arterial de 190x110 mmHg na admissão, paciente com cefaleia. Médico comunicado." } ]
          Alta =
            Some
                { DataAlta = h -120.0
                  CrmAlta = "23456"
                  Resumo = "Pressão arterial controlada com medicação oral. Encaminhado ao ambulatório de cardiologia." } }

    let beatriz =
        { Prontuario = 1010
          Nome = "Beatriz Ramos"
          DataNascimento = DateTime(1979, 4, 22)
          Cpf = "75315985200"
          EstadoCivil = UniaoEstavel
          Responsavel = "Marcelo Dias (companheiro)"
          Leito = "104"
          InternadoEm = h -96.0
          Diagnostico = "Pós-operatório de colecistectomia"
          Prescricoes =
            [ prescricao 1 (Medicamento("Cetoprofeno", "100 mg", Intravenosa)) false 12 "12345" (h -95.0) 24
                  None (Some(h -94.0)) "100001" ]
          Administracoes =
            [ administracao 1 (h -94.0) 10 "200001" None
              administracao 1 (h -82.0) 15 "200001" None ]
          Transferencias =
            [ { LeitoAnterior = "109"; LeitoNovo = "104"; TransferidoEm = h -90.0; TransferidoPor = "100001" } ]
          Anotacoes = []
          Alta =
            Some
                { DataAlta = h -48.0
                  CrmAlta = "12345"
                  Resumo = "Boa evolução pós-operatória, ferida limpa. Retirada de pontos em 10 dias na UBS." } }

    { Medicos = medicos
      Equipe = equipe
      Pacientes = [ maria; joao; antonio; francisca; pedro; lucia; roberto; sandra; jose; beatriz ] }
