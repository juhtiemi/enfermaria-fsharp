/// Entrada: lê e converte os dados digitados no teclado. Cada leitura devolve
/// Some valor, ou None quando o usuário digita 0 para voltar ao menu.
// Aplica o paradigma funcional (isolamento de efeitos colaterais) por meio da
// separação em módulo: só este arquivo e o Program.fs acessam o terminal.
module Entrada

open System
open System.Globalization

let escrever (texto: string) = Console.WriteLine texto

// Aplica o paradigma funcional (expressões de computação) por meio de um
// builder sobre Option: cada let! só continua se houver valor, e um None
// (usuário digitou 0) interrompe a tela inteira.
type TalvezBuilder() =
    member _.Bind(valor: 'a option, f: 'a -> 'b option) = Option.bind f valor
    member _.Return(x: 'a) = Some x
    member _.ReturnFrom(x: 'a option) = x

let talvez = TalvezBuilder()

/// Encerra o programa se a entrada terminar (fim do arquivo)
let lerLinha (rotulo: string) =
    Console.Write(rotulo + ": ")
    let linha = Console.ReadLine()
    if isNull linha then exit 0 else linha.Trim()

/// "0" significa voltar ao menu
let private verificarVoltar texto = if texto = "0" then None else Some texto

let lerTexto rotulo = lerLinha rotulo |> verificarVoltar

// Aplica o paradigma funcional (recursão) por meio da chamada da própria
// função quando o campo fica vazio, no lugar de um laço while.
let rec lerObrigatorio rotulo =
    match lerTexto rotulo with
    | Some "" ->
        escrever "  Campo obrigatório."
        lerObrigatorio rotulo
    | resultado -> resultado

/// Some None = campo deixado em branco; None = voltar ao menu
// Aplica o paradigma funcional (ausência de valor sem null) por meio de
// Option aninhado, que distingue "campo vazio" de "voltar ao menu".
let lerOpcional rotulo : string option option =
    match lerTexto (rotulo + " (opcional, Enter para pular)") with
    | None -> None
    | Some "" -> Some None
    | Some texto -> Some(Some texto)

let rec lerInteiro rotulo =
    match lerTexto rotulo with
    | None -> None
    | Some texto ->
        match Int32.TryParse texto with
        | true, n -> Some n
        | _ ->
            escrever "  Digite um número inteiro."
            lerInteiro rotulo

/// Enter = valor padrão
let rec lerInteiroPadrao rotulo (padrao: int) =
    match lerTexto (sprintf "%s [Enter = %d]" rotulo padrao) with
    | None -> None
    | Some "" -> Some padrao
    | Some texto ->
        match Int32.TryParse texto with
        | true, n -> Some n
        | _ ->
            escrever "  Digite um número inteiro."
            lerInteiroPadrao rotulo padrao

let private converterData (formato: string) (texto: string) =
    match DateTime.TryParseExact(texto, formato, CultureInfo.InvariantCulture, DateTimeStyles.None) with
    | true, d -> Some d
    | _ -> None

let rec lerData rotulo =
    match lerTexto (rotulo + " (dd/mm/aaaa)") with
    | None -> None
    | Some texto ->
        match converterData "dd/MM/yyyy" texto with
        | Some d -> Some d
        | None ->
            escrever "  Data inválida. Use o formato dd/mm/aaaa."
            lerData rotulo

/// Enter = valor padrão
let rec lerDataHora rotulo (padrao: DateTime) =
    let dica = sprintf " (dd/mm/aaaa hh:mm) [Enter = %s]" (Formatacao.dataHora padrao)

    match lerTexto (rotulo + dica) with
    | None -> None
    | Some "" -> Some padrao
    | Some texto ->
        match converterData "dd/MM/yyyy HH:mm" texto with
        | Some d -> Some d
        | None ->
            escrever "  Data/hora inválida. Use o formato dd/mm/aaaa hh:mm."
            lerDataHora rotulo padrao

// Aplica o paradigma funcional (polimorfismo paramétrico) por meio do tipo
// genérico 'a: a mesma função escolhe médicos, vias, pacientes ou qualquer valor.
let rec escolher (rotulo: string) (opcoes: (string * 'a) list) : 'a option =
    opcoes |> List.iteri (fun i (texto, _) -> escrever (sprintf "  %d) %s" (i + 1) texto))
    escrever "  0) Voltar ao menu"

    match lerInteiro rotulo with
    | None -> None
    | Some n when n >= 1 && n <= List.length opcoes -> Some(snd (List.item (n - 1) opcoes))
    | Some _ ->
        escrever "  Opção inválida."
        escolher rotulo opcoes

/// Escolha digitando um código, como o número do prontuário
// Aplica o paradigma funcional (desestruturação de tuplas) por meio do
// padrão (c, _, _), que extrai apenas o código de cada opção.
let rec escolherPorCodigo (rotulo: string) (opcoes: (int * string * 'a) list) : 'a option =
    opcoes |> List.iter (fun (codigo, texto, _) -> escrever (sprintf "  %d - %s" codigo texto))

    match lerInteiro (rotulo + " (0 = voltar)") with
    | None -> None
    | Some codigo ->
        match opcoes |> List.tryFind (fun (c, _, _) -> c = codigo) with
        | Some(_, _, valor) -> Some valor
        | None ->
            escrever (sprintf "  %s %d não encontrado na lista." rotulo codigo)
            escolherPorCodigo rotulo opcoes

let pausar () =
    Console.Write "\nPressione Enter para continuar..."
    Console.ReadLine() |> ignore
