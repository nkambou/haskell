{-|
Module      : TP1DataTypes
Description : Définition des types pour SmartMail
Copyright   : (c) Nkambou & Tato
License     : GPL-3
Maintainer  : nkambou.roger@uqam.ca
Stability   : experimental

Ce module définit des types et synonimes de types nécessaires pour la création et la gestion 
du service de messagerie "SmartMail"
 -}

module TP1DataTypes where 
import qualified Data.Map as Map

-- Structure de la date ---
type Annee = Integer
type Mois = Int
type Jour = Int
data Date = Date Annee Mois Jour deriving (Show, Eq, Read)

-- Structure des données d'identification des participants ---
type Nom = String
type Prenom = String
type Courriel = String
type Signature = (Prenom, Nom)
data Personne = Personne Courriel Signature deriving (Show, Read) -- Courriel unique
instance Eq Personne where
    (==) (Personne cour1 _ ) (Personne cour2 _ ) = cour1 == cour2

-- Représentation des éléments du système de messagerie
-- //Caractéristique de messages et boite de messagerie
type Contenu = String 
type Objet = [Char]
type Explications = String 
data Priorite = Important | Normal | Faible deriving (Show, Eq, Read)
data TypeMessage = Spam | NonSpam deriving (Show, Eq, Read)

-- //Structure message
data Entete = Entete Date Objet Courriel Courriel deriving  (Show,Eq, Read)
data Trame  = Trame Entete Contenu Signature deriving  (Show, Read, Eq)

-- //Éléments d'un compte smail
type Contact = (Personne, Etat)
data Etat = Noir | Blanc deriving (Show, Eq, Read) -- Noir = contact dans liste noire (bloqué) , Blanc contact non blocqué
type Reception = [Trame] -- boîte de reception
type Envoi = [Trame] -- boîte d'envoi
type Spams = [(Trame, Explications)] -- boîte des spams
--type Preferences = [Trame -> Bool]-- certains spams peuvent se retrouver dans la boîte de reception si c'est ce que veut la personne
type Preferences = [Bool]
type CompteSmail = (Personne, Reception, Envoi, Spams, Preferences, [Contact]) -- Compte smail 
type SmartMail = Map.Map Courriel CompteSmail -- Dictionnaire de comptes smail
--type Heuristique = Trame -> Bool  -- La trame satisfait l'heuristique

-- Quelques fonctions d'accès aux structures ---
-- fonctions d'accès
-- // Les boîtes
spams :: CompteSmail -> Spams  -- boîte des spams
spams (_,_,_,s,_,_) = s
envoi :: CompteSmail -> Envoi  -- boîte des messages envoyés
envoi  (_,_,e,_,_,_)= e  
reception :: CompteSmail -> Reception  -- boîte des messages reçus
reception (_,r,_,_,_,_) = r
preferences :: CompteSmail -> Preferences 
-- filtres ou contraintes imposés par le titulaire d'un compte smail 
-- exemple: je ne veux aucun message dont le courriel de l'expéditer se termine pas ".zz"
--          si la préférence n'est pas satisfaite, le message est redirigé dans la boîte des spams
preferences (_,_,_,_,pr,_) = pr
contacts :: CompteSmail -> [Contact] -- liste de tous les contacts du compte (y compris les douteux)
contacts (_,_,_,_,_,c) = c 

-- // Identité
personnes :: CompteSmail -> (String, String, String)
personnes ((Personne c (p,n)),_,_,_,_,_) = (c, n, p)
courriel :: CompteSmail -> String
courriel ((Personne c _),_,_,_,_,_) = c
signature :: CompteSmail -> Signature
signature ((Personne _ (p,n)),_,_,_,_,_) = (p,n)
personne :: CompteSmail -> Personne
personne (p,_,_,_,_,_) = p

-- // Messages
date :: Trame -> Date
date (Trame (Entete d _ _ _) _ _) = d
emetteur (Trame (Entete _ _ e _) _ _) = e -- son courriel seulement
receveur (Trame (Entete _ _ _ r) _ _) = r -- son courriel seulementcontenu :: Trame -> String
objet :: Trame -> String
objet (Trame (Entete _ o _ _) _ _) = o
contenu :: Trame -> String
contenu (Trame _ c _) = c

--courriels à partir de la liste de contacts
courriels [] = []
courriels (((Personne courr _),etat):cs) = courr:courriels cs

-- retrouver et retourner le contact (la personne) correspondant à un courriel donné
courriel2Personne :: [Contact] -> Courriel -> Maybe Personne
courriel2Personne [] _ = Nothing
courriel2Personne ((p@(Personne courr _),etat):cs)  c 
                  | c== courr = Just p
                  | otherwise = courriel2Personne cs c

-- retrouver et retourner le compte smail d'une personne de courriel donné                                                     
courriel2CompteSmail :: SmartMail -> Courriel ->  CompteSmail
courriel2CompteSmail ssm email = case (Map.lookup email ssm) of
                                 Nothing -> error  "pas un courriel valide"
                                 Just csm -> csm

