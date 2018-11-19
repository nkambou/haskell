{-|
Module      : TP1DataTypes
Description : gestion du service de messagerie "SmartMail"
Copyright   : (c) Nkambou & Tato
License     : GPL-3
Maintainer  : nkambou.roger@uqam.ca
Stability   : experimental

Ce module implémente les fonctions nécessaires pour la gestion du service de messagerie 
"SmartMail". Afin de faciliter votre taille, nous avons implémenté une fonction main qui 
exécute un menu offrant les possibilités de tester vos fonctions. Vous n'avez pas à retoucher
ce menu. Vous de uniquement implémenter les fonctions indiquées. Haskell étant un langage 
fortement typé, nous vous suggerons de vous assurer de la conformité de la signature de 
vos fonctions. Ainsi, chaque fonction doit être précédée de la définition de son type.
 -}
 
module TP1 where 

import TP1DataTypes -- importation des structures et types de données définis 
import Data.List
import Data.Char
import qualified Data.Map as Map
import System.IO.Unsafe
import System.IO
import Data.Time.Clock
import Data.Time.Calendar
import System.Directory
import Control.DeepSeq
import Control.Exception


-------------------------------------------------------------------
------------- QUELQUES DONNEES À TITRE D'EXEMPLE-------------------   
-------------------------------------------------------------------
-- Exemples de trame de message
trameBienvenue1 = (Trame (Entete (Date 2018 10 02) "Bienvenue" "equipesmartmail@smail.ca" "nkambou.roger@smail.ca") "Bienvenue dans votre boite smartMail !" ("Equipe", "SmartMail"))
trameBienvenue2 = (Trame (Entete (Date 2018 10 02) "Bienvenue" "equipesmartmail@smail.ca" "tato.ange@smail.ca") "Bienvenue dans votre boite smartMail !" ("Equipe", "SmartMail")) 

-- Exemples de compte smail
csmail1 =((Personne "nkambou.roger@smail.ca" ("roger","nkambou")), [trameBienvenue1], [], [],[],[]) :: CompteSmail
csmail2 =((Personne "tato.ange@smail.ca" ("ange","tato")), [trameBienvenue2], [], [],[],[]) :: CompteSmail

-- Exemple d'ajout de nouveaux comptes dans le système
ssmail = Map.insert (courriel csmail2) csmail2 (Map.singleton (courriel csmail1) csmail1) 

-------------------------------------------------------------------
-------------------------FONCTIONS UTILITAIRES --------------------   
-------------------------------------------------------------------

-- Récupère la date d'aujourdhui au format : (Année, Mois, Jour)
dateAuj = unsafeDupablePerformIO (getCurrentTime >>= return . toGregorian . utctDay)
-- Insère ou met à jour un compte smail (csm) dans le système SmartMail (ssm)
insert' csm ssm = do 
    return (Map.insert (courriel csm) csm ssm)
-- Vérifie si le courriel passé en paramètre est conforme. Un courriel smail conforme
-- est de la forme <nom.prénom@smail.ca>
courrielValide :: [Char] -> Bool
courrielValide [] = False
courrielValide xs = (snd $ span (/= '@') xs) == "@smail.ca"
-- Convertir la date en une chaine de caractère au format jj/mm/aaaa
date2String (Date a m j) = show j ++ "/" ++ show m ++ "/" ++ show a

-- Extrait le nom à partir du courriel
nom email = takeWhile (/= '.') email 
-- Extrait le prenom à partir du courriel
prenom email = tail $ takeWhile (/= '@') $ dropWhile (/='.') email 

-- Compte le nombre d'occurences d'un élément dans une liste
occurence   :: Eq a => a -> [a] -> Int
occurence x =  length . filter (==x)

-- Retourne le contenu du message saisie par l'usager (de type IO String) jusqu'à ce qu'une ligne 
-- corresponde à la chaîne "&", ce qui marque la fin (et donc le retour) du texte saisie.
prendreContenu:: IO String
prendreContenu = do
        x <- getLine
        if x == "&" then
           return []
        else do
           xs <- prendreContenu
           return ( x ++ xs )

-- Permet la saisie et la récupération de tous les éléments d'une trame de message
ecrireMessage :: String -> IO [String]
ecrireMessage cour = do
         hSetBuffering stdout NoBuffering
         putStr "A > "
         receveur <- getLine
         putStr "Objet > "
         objet <- getLine
         putStr "Contenu > "
         contenu <- prendreContenu
         print $ words cour
         print $ words receveur
         print $ words objet
         print $ lines contenu
         return [cour, receveur, objet, contenu]

--------------------------------------------------------------
--------------- CREATION DE COMPTE SMAIL --------------------- 
--------------------------------------------------------------
-- Creer une nouvelle boîte de messagerie et l'ajouter 
-- dans le système SmartMail  
--------------------------------------------------------------
-- 1) Avant la création d'un compte, il faut que l'usager s'inscrive
-- La fonction suivante vous est offerte. Elle permet la saisie 
-- et la récupération des informations et 'fabrique' un compte smail.
-- Elle retourne donc une monade IO contenant un compte smail non 
-- encore enregistré dans SmartMail
inscrireUnePersonne :: IO CompteSmail
inscrireUnePersonne = do
        hSetBuffering stdout NoBuffering
        putStr "Nom > "
        nom <- getLine
        putStr "Prenom > "
        prenom <- getLine
        print $ "Bonjour " ++ prenom
        print $ "Vous avez maintenant une boite smartMail." 
        print $ "Votre adresse courriel est: " ++ nom ++ "." ++ prenom ++ "@smail.ca"
        return ((Personne (nom ++ "." ++ prenom ++ "@smail.ca") (prenom, nom)),[], [], [], [], [])

-- 2) Ensuite, il faute enregistrer le compte comme un service SmartMail
-- Pour cela, nous nous intéressons à quelques fonctions utiles qui permettent
-- d'ajouter de nouveaux comptes dans SmartMail
-- 2-1)(0.5) Ajouter un compte au système de messagerie SmartMail
ajouterCompte :: SmartMail -> CompteSmail -> SmartMail
ajouterCompte ssm csm = error "À Implementer"



-- 2-2)(0.75) Ajouter une liste donnée de comptes au système SmartMail
ajouterComptes:: SmartMail -> [CompteSmail] -> SmartMail
ajouterComptes ssm lcsm = error "À Implementer"



-- 3)(0.75) Il faut maintenant allier l'inscription et l'enregistrement dans une même
-- fonction. La fonction qui suit doit pouvoir permettre la création et l'enregistrement
-- d'un compte smail interactivemment. 
-- Input : Une instance du systeme de messagerie SmartMail 
-- Output: Une paire comportant le compte créé et le sytème de messagerie ajusté, c-à-d
--         comportant une entrée pour le nouveau compte.
-- Remarques: Le choix de retourner une paire est stratégique. Il évitera des accès inutiles
--            au système, puisque des manupulations du compte peuvent suivre immédiatement sa création.
creerUnCompteIO :: SmartMail -> IO (CompteSmail, SmartMail)  
creerUnCompteIO ssm = do 
                csm <-  inscrireUnePersonne
                return $ enregistrerUnCompte csm ssm

enregistrerUnCompte :: CompteSmail -> SmartMail -> (CompteSmail, SmartMail)
enregistrerUnCompte csm ssm = error "À compléter"
  
-------------------------------------------------------------------
---------- FILTRES ANTI SPAM ET ANTI HAMEÇONNAGE ------------------ 
-------------------------------------------------------------------
{- Il est prévu deux classes de filtres: les filtres d'enveloppe et de contenu.
   4)(1)  Filtrage de l'enveloppe. Ce filtre s’exécute uniquement sur l'entête de la trame et non sur son contenu.
       Une trame est détectée comme spam par le filtrageEnveloppe si au moins l'une des conditions suivantes est vraie: 
         (1) tous les caractères de l'objet sont en majiscules
         (2) l'objet contient au moins 2 points d'exclamation "!"
         (3) l'objet contient au moins 2 points d'interrogation "?"
         (4) l'objet est vide
         (5) l'objet contient le caractère "$"
         (6) l'emetteur a été bloqué (Etat = Noir) par le destinataire     
-}
filtrageEnveloppe :: Trame -> SmartMail -> (TypeMessage, Trame, Explications) 
filtrageEnveloppe trame ssm = error "À Implementer"



 
{- 5)(2) Filtre de contenu et mots clés. Les filtres de contenu analysent le contenu des 
   messages et détectent les spams qui ont réussi à passer à travers le filtre d'enveloppe.
   Le principe consiste généralement à détecter des mots précis ou des mots de formes 
   particulières, par exemple des mots qui contiennent des chiffres ou certains symboles (C|AL|S, -1AGRA, 
   PR0ZAC, ZYBAN and C3LEBREX cred1ted. Il y a donc deux sous catégories d'heuristiques: la détection 
   des mots clés (heuristiqueMotsCles) et la recherche de mots comportant des caractères spéciaux 
   (heuristiqueCaracteresSpeciaux). Un poids (proportion de mots clés ou de mots comportant 
   les caractères spéciaux) de l'ordre de 10% suffit pour déclarer le message comme spam.
   Vous devez définir les 3 filtres suivants:
   filtreSpecifiqueHams pour les mots-clés suivants: "sexe", "sexy", "viagra", "argent", "drogue",
                        "money","credit", "$","chaud", "nu", "click", "amateur", "pics","videos",
                        "gagner","lotterie","heritage".
                                   
   filtreSpecifiquePub pour les mots-clés "offre", "commande", "click", "videos","gratuit","publicité",
                        "special","voyage"
   filtreCaractèresSpeciaux pour mots contenant un ou l'autre des caractères suivant '|','0'..'9'.
   
   On supposera qu'un message, s'il est un spam, l'est à cause de l'un des filtres exclusivement
   L'explication donnera les indices sur la nature du spam: classique_enveloppe (pour le filtrage d'enveloppe),
   classique_contenu(caractères spéciaux dans les mots), hameçonnage, publicitaire. L'information sur la
   proportion des mots suspects dans le contenu du message est requis pour les filtres de contenu.
   Remarque: Pour cette question, vous devez écrire des sous-fonctions nécessaires.   
-}
filtrageContenu :: Trame -> (TypeMessage, Trame, Explications)
filtrageContenu tr = error "À compléter"


-------------------------------------------------------------------
------------------ ENVOI ET RECEPTION DE MESSAGE  ----------------- 
-------------------------------------------------------------------
{- 6)(2.5) Envoyer un message d'un emetteur vers un destinataire
   Input : Le système de messagerie au complet et le compte smail de l'emetteur
   Output : un couple formé du système SmartMail mis à jour, et du compte smail de l'emetteur.
   REMARQUE: le message doit être enregistré dans la boîte d'envoi de l'emetteur mais doit
   également être enregistré dans la boîte de reception du destinataire (si le message n'est 
   pas un spam c-à-d que filtreEnvelope ou filtreContenu ne retourne pas "Spam") ou dans 
   la boîte de spams le cas échéant. Si le destinataire n'a pas de compte "SmartMail", 
   ne rien faire et ne garder le message que dans la boite de l'émétteur 
-}
envoyerMessage :: SmartMail -> CompteSmail -> IO (SmartMail, CompteSmail)
envoyerMessage ssm csm = do
         xs <- ecrireMessage (courriel csm)-- utiliser le retour de cette fonction pour créer le message 
         return (error "À compléter selon le même modèle que dans la question 3")

envoyerMessage' :: SmartMail -> CompteSmail -> Trame -> (SmartMail, CompteSmail)
envoyerMessage' = error ""
-------------------------------------------------------------------
----------GESTION BOÎTES DE MESSAGERIES ET DES CONTACTS  ----------   
-------------------------------------------------------------------
-- 7-1)(0.75) AjouterContact
-- Description: Ajoute un contact à la liste de contacts d'une boîte de messagerie.
-- Input : Le courriel, le prenom, le nom du contact à ajouter ainsi que la boite de destination
-- Output : La boîte modifié : le nouveau contact est ajouté à la liste de contacts
ajouterContact :: Courriel -> Prenom -> Nom ->CompteSmail -> IO CompteSmail
ajouterContact c p n csm = do 
    return (error "À compléter selon le même modèle que dans la question 3")

-- 7-2)(0.75) BloquerContact
-- Description: Bloque un contact (met son Etat à Noir).
-- Input : La boîte ou se trouve le contact et la personne à bloquer
-- Output : La boîte modifié (le contact a été ajouté à la liste noir)
bloquerContact ::CompteSmail -> Personne -> IO (CompteSmail)
bloquerContact csm p = do
    return (error "À compléter selon le même modèle que dans la question 3") 


-- 8)(0.75) viderBoite
-- Description: Vider les messages d'une boîte donnée
-- Input : La boîte à vider et l'option = "Spams" pour vider les spams, "Envoi" pour 
--         vider la boîte d'envoi et "Reception" pour vider la boîte de recption"
-- Output: Le compte résultant avec la boîte indiquée vidée.
viderBoite :: CompteSmail -> String -> IO (CompteSmail)
viderBoite csm o = do 
    return (error "À compléter selon le même modèle que dans la question 3")


--- Afin de faciliter la gestion des messages, l'administrateur voudrait pouvoir
--  reorganiser toutes les boîtes (reception, spams, envoi) de la manière suivante:
--- chaque boîte doit devenir un dictionnaire avec le courriel de l'emetteur comme clé 
--- et comme valeur, une liste des messages reçus de cette personne. Les messages de cette 
--- liste doivent comporter uniquement la date (au format "jj/mm/aaaa"), l'objet et le contenu 
--  du message (sous forme d'un triplet). 
--  9-1)(1) Écrire la fonction reformaterBoite qui reformate une boîte selon le principe décrit.
--  --  Donnée: Une boîte de messages (une liste de trames)
--  --  Sortie: Le dictionnaire correspondant. 
reformaterBoite :: [Trame] -> Map.Map String [(String, String, String)]
reformaterBoite = error ""

--  9-2)(0.75) Utiliser la fonction reformaterBoite pour écrire la fonction reformaterCompte qui reconstruit 
--  la structure d'un compte smail. Le résultat doit comporter l'ensemble des messages provenant des 
--  trois boîtes y compris celle des spams. Pour la boîte des spams, on ne s'interessera 
--  qu'aux Trames de message sans explications.
--  --  Donnée:  Un compte smail
--  --  Sortie:  Un triplet comportant les trois boîtes dans l'ordre et dans leur nouvelle format.

reformaterCompte :: Ord k => k
     -> Map.Map k CompteSmail
     -> (Map.Map String [(String, String, String)],
         Map.Map String [(String, String, String)],
         Map.Map String [(String, String, String)])
reformaterCompte = error ""

-------------------------------------------------------------------
--------------------   STATISTIQUES ET AFFICHAGES   ---------------   
-------------------------------------------------------------------
-- 10-1)(0,75) Nombre de spams recus dans le systeme de messagerie au complet.
-- Input : systeme de messagerie global
-- Output : le nombre total de spams
nbTotalSpams :: SmartMail -> Int
nbTotalSpams ssm = error "À implementer"

-- 10-2)(0.75) Produire l'ensemble de tous les spams du système dans une même liste
-- Input : systeme de messagerie global
-- Output : liste de spams
tousLesSpams:: SmartMail -> [(Trame, Explications)]
tousLesSpams ssm = error "À implementer"

-- 10-3)(1) Produire une liste qui associe à chaque inscrit (son courriel seulement), 
-- le nombre de spams recus.
-- Input : systeme de messagerie global
-- Output : liste de (Courriel, nombre de spams recus)
statSpamsRecus :: SmartMail -> [(String, Int)]
statSpamsRecus ssm =  error "À implementer"

-- 10-4)(1) Produire une liste qui associe à chaque inscrit ayant envoyé au moins un spam 
-- (son courriel seulement), le nombre de spams produit
-- Input : systeme de messagerie global
-- Output : liste de (courriel emetteur spam, nombre de spams émis)
statSpamsEnvoyes :: SmartMail -> [(Courriel, Int)]
statSpamsEnvoyes ssm = error "À implementer"

-------------------------------------------------------------------
--------------------------MENU PRINCIPAL--------------------------- 
-------------------------------------------------------------------
-- Sauvegarde du système dans un fichier texte
sauver ssm = do
    fileExists <- doesFileExist "ssmail.txt"
    if fileExists
    then do 
            writeFile "ssmail.txt" (show ssm)
            putStr "Systeme Smail sauvegarde avec succes"
    else do putStrLn ("Erreur de sauvegarde du fichier ")

-- Charger le service smartMail
lire :: IO (SmartMail)
lire = do
    fileExists <- doesFileExist "ssmail.txt"
    if fileExists
            then do 
                    ssm <- readFile "ssmail.txt"
                    evaluate (force ssm)
                    putStr "Service Smail charge avec succes" 
                    return (read ssm)          
            else do putStr "Fichier non existant"
                    writeFile "ssmail.txt" (show ssmail)
                    return ssmail

sMail = do
    ssm <- lire
    menu ssm

menu ssm = do
    hSetBuffering stdout NoBuffering
    putStrLn "**********Bienvenue sur SmartMail**********"
    putStrLn "1- Se connecter"
    putStrLn "2- S'inscrire"
    putStrLn "0 - Quitter"
    putStr "Option : "
    option <- getLine
    traiterChoix1 ssm option  
    
traiterChoix1 ssm "1" = do 
                hSetBuffering stdout NoBuffering
                putStr "Votre adresse courriel :\n"
                courriel <- getLine 
                if courrielValide courriel then do 
                    case (Map.lookup courriel ssm) of
                        Nothing -> do
                            putStrLn "Cet adresse n'a pas de messagerie associée"
                            menu ssm 
                        Just csm -> do
                            menu' ssm csm
                else do 
                    putStrLn "Adresse courriel non valide\n"
                    menu ssm
traiterChoix1 ssm "2" = do 
                ssm <- (creerUnCompteIO  ssm)
                menu' (snd ssm) (fst ssm)
traiterChoix1 _ "0" = do
                putStrLn "Bye Bye ! nous esperons vous revoir bientot :) \n"
                    
traiterChoix1  _ _ = do 
    putStrLn "Cet option n'est pas disponible. \n"
    sMail

menu' :: SmartMail -> CompteSmail -> IO ()
menu' ssm csm = do
    hSetBuffering stdout NoBuffering
    putStr "\n *** MENU ***"
    putStr "\n   compte : "
    putStr (courriel csm)
    putStr "\n"
    putStr "1- Visualiser boîte de reception\n"
    putStr "2- Visualiser boîte d'envoi\n"
    putStr "3- Visualiser boîte de spams\n"
    putStr "4- Envoyer un message\n"
    putStr "5- Afficher la liste de contacts\n"
    putStr "6- Ajouter un contact\n"
    putStr "7- Bloquer un contact\n"
    putStr "8- Statistiques sur le service SmartMail\n"
    putStr "9- Vider une boîte (reception ou envoi ou spams)\n"
    putStr "10- Se connecter avec un autre compte\n"
    putStr "11- Se déconnecter et revenir au menu précédent \n"
    putStr "0- Quitter\n"
    putStr "   Option: "
    hFlush stdout
    rep <- getLine
    traiterChoix ssm csm rep



traiterChoix ssm csm "1"  = do 
        putStr $ show (reception csm)
        menu' ssm csm

traiterChoix ssm csm "2"  = do 
                    putStr $ show (envoi csm)
                    menu' ssm csm

traiterChoix ssm csm "3"  = do 
                    putStr $ show (spams csm)
                    menu' ssm csm

traiterChoix ssm csm "4"  = do 
                    (new_ssm,new_csm) <- envoyerMessage ssm csm
                    putStrLn (show ( (elem (courriel new_csm) (Map.keys new_ssm))))
                    menu' new_ssm new_csm

traiterChoix ssm csm "5"  = do 
                    putStr $ show (contacts csm)
                    menu' ssm csm
traiterChoix ssm csm "6"  = do 
                    hSetBuffering stdout NoBuffering
                    putStr "Courriel :"
                    c <- getLine
                    if courrielValide c then do 
                            new_csm <- (ajouterContact c "" "" csm)
                            new_ssm <- insert' new_csm  ssm
                            putStrLn "Adresse ajoutée avec succès"
                            menu' new_ssm new_csm
                     else do 
                        putStr "Adresse courriel non valide\n"
                        menu' ssm csm

traiterChoix ssm csm "7" = do 
                    putStrLn "adresse courriel du contact à bloquer:\n"
                    courriel <- getLine 
                    if elem courriel (courriels (contacts csm)) then do 
                        case (courriel2Personne (contacts csm) courriel) of
                            Nothing -> do
                                putStrLn "Pas dans votre Liste de contacts"
                                menu' ssm csm 
                            Just p -> do
                                new_csm <- bloquerContact csm p
                                new_ssm <- insert' new_csm  ssm
                                putStrLn "Contact bloqué avec succès\n"
                                menu' new_ssm new_csm
                    else do 
                        putStrLn "Ce contact n'est pas dans votre liste de contact\n"
                        menu' ssm csm
traiterChoix ssm csm "8" = do
                    hSetBuffering stdout NoBuffering
                    putStr "Mot de passe administrateur :"
                    mdp <- getLine
                    case mdp of
                        "12345" -> do 
                            menuAdmin ssm csm
                        _ -> do 
                            putStrLn "Mot de passe non valide !"
                            menu' ssm csm

traiterChoix ssm csm "9" = do
                    putStrLn "1- Vider la boîte de reception"
                    putStrLn "2- Vider la boîte d'envoi"
                    putStrLn "3- Vider la boîte de spams"
                    putStrLn "0- Annuler "
                    rep <- getLine
                    case rep of
                        "1" -> do 
                            new_csm <- viderBoite csm "Reception"
                            new_ssm <- insert' new_csm  ssm
                            putStrLn "Boîte de reception vidée\n"
                            menu' new_ssm new_csm
                        "2" -> do 
                            new_csm <- viderBoite csm "Envoi"
                            new_ssm <- insert' new_csm  ssm
                            putStrLn "Boîte d'envoi vidée\n"
                            menu' new_ssm new_csm
                        "3" -> do 
                            new_csm <- viderBoite csm "Spams"
                            new_ssm <- insert' new_csm  ssm
                            putStrLn "Boîte de spams vidée\n"
                            menu' new_ssm new_csm
                        "0" -> do 
                            menu' ssm csm
                        

traiterChoix ssm csm "10"  = do 
                         traiterChoix10 ssm csm

traiterChoix ssm csm "11"  = menu ssm

traiterChoix _ _ "0" = do
                putStr "Bye Bye ! nous esperons vous revoir bientot :) \n"

traiterChoix ssm csm _  = do 
                putStr "Cet option n'est pas disponible. \n"
                menu' ssm csm




traiterChoix10 ssm csm = do 
        putStrLn "Votre adresse courriel :\n"
        courriel <- getLine 
        if courrielValide courriel then do 
            case (Map.lookup courriel ssm) of
                Nothing -> do
                    putStrLn "Cet adresse n'a pas de messagerie associée"
                    menu' ssm csm
                Just csm1 -> do
                    menu' ssm csm1
        else do 
            putStr "Adresse courriel non valide\n"
            menu' ssm csm



menuAdmin :: SmartMail -> CompteSmail -> IO ()
menuAdmin ssm csm = do
    hSetBuffering stdout NoBuffering
    putStr "\n   *** MENU ADMINISTRATEUR : Stistiques ***"
    putStr "\n   compte : "
    putStr (courriel csm)
    putStr "\n"
    putStr "1- Nombre de spams recus dans le systeme de messagerie au complet\n"
    putStr "2- Ensemble de tous les spams du système dans une même liste \n"
    putStr "3- Liste qui associe à chaque inscrit (son courriel seulement), le nombre de spams recus \n"
    putStr "4- Liste qui associe à chaque inscrit (ayant envoyé au moins 1 spam) (son courriel seulement), le nombre de spams produit\n"
    putStr "5- Revenir dans votre boîte de messagerie \n"
    putStr "0- Quitter\n"
    putStr "   Option: "
    rep <- getLine
    putStr "\n"
    traiterChoixAdmin ssm csm rep

traiterChoixAdmin ssm csm "1" = do 
        putStr (show ((nbTotalSpams ssm) :: Int)) 
        menuAdmin ssm csm

traiterChoixAdmin ssm csm "2" = do 
        putStr (show ((tousLesSpams ssm) :: [(Trame, Explications)]))
        menuAdmin ssm csm

traiterChoixAdmin ssm csm "3" = do 
        putStr (show ((statSpamsRecus ssm) :: [(String, Int)]))
        menuAdmin ssm csm

traiterChoixAdmin ssm csm "4" = do 
        putStr (show ((statSpamsEnvoyes ssm) :: [(Courriel, Int)]))
        menuAdmin ssm csm

traiterChoixAdmin ssm csm "5" = do 
        menu' ssm csm     
traiterChoixAdmin ssm csm "0" = traiterChoix ssm csm "0" 
traiterChoixAdmin ssm csm _ = do 
    putStrLn "Cet option n'est pas disponible. \n"
    menuAdmin ssm csm

