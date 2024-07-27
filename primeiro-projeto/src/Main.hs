module Main where

import System.Random (randomRIO)
import Resultados
import Tipos
import Jogada

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
        jogada <- obterJogadaValida tabuleiro
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