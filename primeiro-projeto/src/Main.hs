module Main where

import System.Random (randomRIO)
import Data.Char (toUpper)

-- Definição do tipo de tabuleiro e constantes
type Tabuleiro = [[Char]]
vazio, jogadorX, jogadorO :: Char
vazio = ' '
jogadorX = 'X'
jogadorO = 'O'

-- Tamanho do tabuleiro
tamanhoTabuleiro :: Int
tamanhoTabuleiro = 3

-- Função para verificar se alguém ganhou
ganhou :: Tabuleiro -> Char -> Bool
ganhou tabuleiro jogador = 
    -- Verificar linhas, colunas e diagonais
    any (all (== jogador)) tabuleiro ||
    any (all (== jogador)) (transpose tabuleiro) ||
    all (== jogador) (map (!! 0) tabuleiro) ||
    all (== jogador) (map (!! 2) tabuleiro) ||
    all (== jogador) [tabuleiro !! i !! i | i <- [0..2]] ||
    all (== jogador) [tabuleiro !! i !! (2 - i) | i <- [0..2]]

-- Função para verificar se o jogo empatou
empatou :: Tabuleiro -> Bool
empatou tabuleiro = all ((/= vazio) . head) tabuleiro

-- Função para imprimir o tabuleiro
imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro tabuleiro = do
    putStrLn "Estado atual do tabuleiro:"
    putStrLn $ unlines [unwords [ [c] | c <- linha] | linha <- tabuleiro]

-- Função para fazer uma jogada
fazJogada :: (Int, Int) -> Tabuleiro -> Char -> Tabuleiro
fazJogada (linha, coluna) tabuleiro jogador
    | tabuleiro !! linha !! coluna == vazio = 
        let novoTabuleiro = take linha tabuleiro ++
                             [take coluna (tabuleiro !! linha) ++
                              [jogador] ++
                              drop (coluna + 1) (tabuleiro !! linha)] ++
                             drop (linha + 1) tabuleiro
        in novoTabuleiro
    | otherwise = tabuleiro

-- Função para gerar um tabuleiro inicial vazio
tabuleiroInicial :: Tabuleiro
tabuleiroInicial = replicate tamanhoTabuleiro (replicate tamanhoTabuleiro vazio)

-- Função para obter a jogada do usuário
obterJogada :: IO (Int, Int)
obterJogada = do
    putStrLn "Digite a linha (0-2) e a coluna (0-2) para sua jogada, separados por espaço:"
    input <- getLine
    let [linhaStr, colunaStr] = words input
    return (read linhaStr, read colunaStr)

-- Função principal do jogo
jogoDaVelha :: IO ()
jogoDaVelha = do
    let tabuleiro = tabuleiroInicial
    imprimeTabuleiro tabuleiro
    loop tabuleiro jogadorX
  where
    loop :: Tabuleiro -> Char -> IO ()
    loop tabuleiro jogadorAtual = do
        putStrLn $ "É a vez do jogador " ++ [jogadorAtual]
        jogada <- obterJogada
        let novoTabuleiro = fazJogada jogada tabuleiro jogadorAtual
        imprimeTabuleiro novoTabuleiro
        if ganhou novoTabuleiro jogadorAtual
            then putStrLn $ "O jogador " ++ [jogadorAtual] ++ " ganhou!"
            else if empatou novoTabuleiro
                then putStrLn "O jogo empatou!"
                else loop novoTabuleiro (if jogadorAtual == jogadorX then jogadorO else jogadorX)

-- Função para iniciar o jogo
main :: IO ()
main = jogoDaVelha
