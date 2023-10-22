{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Diseases where

data Disease = Disease 
    { id :: Int
    , russian_name :: String
    , latin_name :: String
    , reciept :: [(Int, Float)]
    , duration :: Int    
    } deriving (Show, Generic)
