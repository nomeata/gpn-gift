module DatT where

type Option = [(String, String)] 

type Fahrplan = [(ID,Name,Room,Day,(Hour,Min),(Hour,Min))] 
type ID = Integer
type Name = String
type Room = String
type Day = Integer
type Hour = Integer
type Min = Integer

type Passwd = [((String,String),[String])]

