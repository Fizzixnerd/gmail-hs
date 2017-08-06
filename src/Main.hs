{-# LANGUAGE OverloadedStrings #-}

module Main where

import Gmail.Auth

main :: IO ()
main = do
  x <- defaultGetToken
  print x
