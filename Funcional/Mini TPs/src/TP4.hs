module TP4 where
import Video
import EdicionVideos
import Data.Char

-- Definir las funciones de acuerdo a lo indicado en las consignas del TP4

-------------------------
-- Hashtags
-------------------------
minimoRelacion :: Int
minimoRelacion = 3

stringToLower :: String -> String
stringToLower = map toLower

tieneHashtag :: String -> Video -> Bool
--tieneHashtag hashtag video = any ((==).stringToLower $ hashtag).map stringToLower.words $ (hashtags video)
tieneHashtag hashtag video = elem (stringToLower $ hashtag).map stringToLower.words $ (hashtags video)

hashtagsEnComun :: Video -> Video -> [String]
hashtagsEnComun video1 video2 = filter (`tieneHashtag` video1).words $ (hashtags video2)

estanRelacionados :: Video -> Video -> Bool
estanRelacionados video1 = (>=minimoRelacion).length.(hashtagsEnComun video1)

agregarHashtag :: String -> Video -> Video
agregarHashtag hashtag video
  | tieneHashtag hashtag video = video
  | otherwise = Video { titulo = (titulo video), duracion = (duracion video), hashtags = (hashtags video ++ " " ++ hashtag) }

hashtagsSinRepetir :: [String] -> Video -> String
hashtagsSinRepetir [] video = ""
hashtagsSinRepetir (x:xs) video
  | elem x (words (hashtags video)) = hashtagsSinRepetir xs video
  | otherwise = (hashtagsSinRepetir xs video) ++ " " ++ x

hashtagsSinRepetir' :: [String] -> Video -> String
hashtagsSinRepetir' listaHashtags video = concat.map (" "++).filter (not.(`elem` (words (hashtags video)))) $ listaHashtags

agregarHashtags :: [String] -> Video -> Video
agregarHashtags listaHashtags video = foldr agregarHashtag video listaHashtags

agregarHashtags' :: [String] -> Video -> Video
agregarHashtags' listaHashtags video = Video { titulo = (titulo video), duracion = (duracion video), hashtags = (hashtags video)++(hashtagsSinRepetir listaHashtags video)}

-------------------------
-- Edicion de videos
-------------------------

subir :: Video -> VideoVersionado
subir video = Version { versionActual = video, versionesAnteriores = []}

editar :: (Video -> Video) -> VideoVersionado -> VideoVersionado
editar f video 
 | versionActual video == f (versionActual video) = video
 | otherwise = Version { versionActual = f (versionActual video), versionesAnteriores = (versionActual video) : (versionesAnteriores video) }

postProcesamiento :: String -> Duracion -> Video -> VideoVersionado
postProcesamiento titulo duracion video = editar (recortar duracion).editar (renombrar titulo) $ subir video