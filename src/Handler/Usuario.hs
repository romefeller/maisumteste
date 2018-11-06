{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Control.Monad.Zip

-- (,) :: a -> b -> (a,b)
-- pure (,) :: Form (a -> b -> (a,b))
-- pure (,) <*> (Usuario ...) :: Form b -> Form (Usuario, b)
-- pure (,) <*> (Usuario ...) <*> areq ... :: Form (Usuario, Text)
formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $  pure (,)
    <*> (Usuario 
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing 
        <*> areq passwordField "Password: " Nothing
    )
    <*> areq passwordField "Password Confirmation: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widgetUsu, enctype) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/usuario.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do 
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usr, passwordC) -> do 
            if (usuarioSenha usr) == passwordC then do
                runDB $ insert usr 
                setMessage [shamlet|
                    <h1>
                        Usuario cadastrado!
                |]
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <h1>
                        Senhas não conferem
                |]
                redirect UsuarioR
        _ -> redirect UsuarioR