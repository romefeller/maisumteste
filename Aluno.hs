{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.Aluno where

import Import

getBuscarR :: Text -> Handler TypedContent
getBuscarR nome = do 
    aluno <- runDB $ selectList [Filter AlunoNome (Left $  concat ["%", nome, "%"]) (BackendSpecificFilter "ILIKE")] []
    sendStatusJSON ok200 (object ["resp" .= aluno])

-- {"resp":2}
postAlunoR :: Handler TypedContent
postAlunoR = do 
    aluno <- requireJsonBody :: Handler Aluno
    alunoid <- runDB $ insert aluno
    sendStatusJSON created201 (object ["resp" .= alunoid])

-- select * from aluno order by aluno.nome;
getListAlunoR :: Handler TypedContent
getListAlunoR = do 
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    sendStatusJSON ok200 (object ["resp" .= alunos])

getPerfilR :: AlunoId -> Handler TypedContent
getPerfilR alunoid = do 
    aluno <- runDB $ get404 alunoid
    sendStatusJSON ok200 (object ["resp" .= aluno])

deleteApagarR :: AlunoId -> Handler TypedContent
deleteApagarR alunoid = do 
    _ <- runDB $ get404 alunoid
    runDB $ delete alunoid
    sendStatusJSON noContent204 (object [])

putAlterarR :: AlunoId -> Handler TypedContent
putAlterarR alunoid = do 
    _ <- runDB $ get404 alunoid
    novoAluno <- requireJsonBody :: Handler Aluno
    runDB $ replace alunoid novoAluno
    sendStatusJSON noContent204 (object [])

-- UPDATE from aluno 
-- set aluno.nome = nome
-- where aluno.id = alunoid
patchAltNomeR :: AlunoId -> Text -> Handler TypedContent
patchAltNomeR alunoid nome = do 
    _ <- runDB $ get404 alunoid
    runDB $ update alunoid [AlunoNome =. nome]
    sendStatusJSON noContent204 (object [])