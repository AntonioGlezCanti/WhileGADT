{-# LANGUAGE ViewPatterns #-}

module ViewPattern where

type Grado = String
type Nombre = String
type Id = Integer
type Dep = String

data Uma =  Alum Nombre Id Grado
          | Prof Nombre Id Dep
          | Admin Nombre Id 

getGrado :: Uma -> Maybe Grado
getGrado (Alum _ _ g) = Just g
getGrado _ = Nothing

getDep :: Uma -> Maybe Dep
getDep (Prof _ _ d) = Just d
getDep _  = Nothing


perteneceDep :: Uma -> Dep -> Bool
perteneceDep (getDep -> Just d) dep = d == dep
perteneceDep _ _ = False

esInf :: Uma -> Bool
esInf (getGrado -> Just "Inf") = True
esInf _ = False 

al1 = Alum "Antonio" 1 "Inf"
prof1 = Prof "Pablo" 1 "LCC"