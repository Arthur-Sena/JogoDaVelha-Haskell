module Jogada where

import Tipos

{-
    Manipulação e retorno de tabuleiro
-}
tabuleiroInicial :: Tabuleiro
tabuleiroInicial = replicate tamanhoTabuleiro (replicate tamanhoTabuleiro vazio)

imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro tabuleiro = do
    putStrLn "Estado atual do tabuleiro:"
    putStrLn $ unlines [unwords [ [c] | c <- linha] | linha <- tabuleiro]

{-
    Funções para obter, validar, e passar jogada para o tabuleiro
-}

-- Função para obter a jogada do usuário - looping até ter uma jogada válida do usuário
obterJogadaValida :: Tabuleiro -> IO Coord
obterJogadaValida tabuleiro = do
    putStrLn "Digite a linha (0-2) e a coluna (0-2) para sua jogada, separados por espaço:"
    input <- getLine
    let [linhaStr, colunaStr] = words input
    let jogada = (read linhaStr, read colunaStr)
    if posicaoDisponivel jogada tabuleiro
        then return jogada
        else do
            putStrLn "Essa posição já está ocupada. Por favor, escolha outra."
            obterJogadaValida tabuleiro

-- Função para verificar se uma posição no tabuleiro está disponível (vazia)
posicaoDisponivel :: Coord -> Tabuleiro -> Bool
posicaoDisponivel (linha, coluna) tabuleiro =
    tabuleiro !! linha !! coluna == vazio

-- Função para fazer uma jogada
fazJogada :: Coord -> Tabuleiro -> Char -> Tabuleiro
fazJogada (linha, coluna) tabuleiro jogador
    | tabuleiro !! linha !! coluna == vazio = 
        let novoTabuleiro = take linha tabuleiro ++
                             [take coluna (tabuleiro !! linha) ++
                              [jogador] ++
                              drop (coluna + 1) (tabuleiro !! linha)] ++
                             drop (linha + 1) tabuleiro
        in novoTabuleiro
    | otherwise = tabuleiro

