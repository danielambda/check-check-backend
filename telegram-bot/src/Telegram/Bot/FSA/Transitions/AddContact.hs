{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.AddContact (handleTransition) where

import Telegram.Bot.Simple (replyText)
import Data.Text (Text)
import qualified Data.Text as T
import Servant.API.UVerb (matchUnion)
import Servant.API (WithStatus)

import SmartPrimitives.TextMaxLen (mkTextMaxLen)
import CheckCheck.Contracts.Users.Contacts (CreateContactReqBody(..))
import Clients.AuthService (AuthServiceUser(..), getUser, UserQuery (..))
import Clients.Backend (createContact)
import Clients.Utils (runReq, runReq_)
import Telegram.Bot.AppM (currentUser, authViaTelegram, (<#), Eff', tg)
import Telegram.Bot.FSA (State(InitialState), Transition)

handleTransition :: Text -> State -> Eff' Transition State
handleTransition content _ = InitialState <# do
  token <- authViaTelegram =<< currentUser
  case T.uncons content of
    Nothing -> do
      tg $ replyText "Please, enter non empty username to add to contacts"
    Just ('@', content') -> case T.span (/= ' ') content' of
      (contactTgUsername, "") -> do
        u <- runReq $ getUser token (UserTgUsernameQuery contactTgUsername)
        if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
              let reqBody = CreateContactReqBody
                    { contactUserId = userId
                    , contactName = Nothing
                    }
              runReq_ $ createContact token reqBody
              tg $ replyText $ "contact @" <> contactTgUsername <> " successfully added"
           | Just _ <- matchUnion @(WithStatus 404 ()) u -> do
              tg $ replyText $ T.unlines
                [ "User @" <> contactTgUsername <> " is not registered in check-check"
                , "Send them the following link to join:"
                ]
              tg $ replyText "https://t.me/CheckCheckTgBot?start=start" -- TODO remove the hardlink
            | otherwise ->
              error "unreachable"
      (contactTgUsername, mContactName) -> case mkTextMaxLen mContactName of
        Nothing -> tg $ replyText $
          "contact name " <> mContactName <> " is too long, 50 symbols is the max length"
        Just contactName -> do
          u <- runReq $ getUser token (UserTgUsernameQuery contactTgUsername)
          if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
              let reqBody = CreateContactReqBody
                    { contactUserId = userId
                    , contactName = Just contactName
                    }
              runReq_ $ createContact token reqBody
              tg $ replyText $ "contact " <> contactTgUsername <> " successfully added as " <> mContactName
             | Just _ <- matchUnion @(WithStatus 404 ()) u -> do
              tg $ replyText $ T.unlines
                [ "User " <> contactTgUsername <> " is not registered in check-check"
                , "Send them the following link to join:"
                ]
              tg $ replyText "https://t.me/CheckCheckTgBot?start=start" -- TODO remove the hardlink
             | otherwise -> error "unreachable"
    _ -> case T.span (/= ' ') content of
      (contactUsername, "") -> do
        u <- runReq $ getUser token (UserUsernameQuery contactUsername)
        if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
            let reqBody = CreateContactReqBody
                  { contactUserId = userId
                  , contactName = Nothing
                  }
            runReq_ $ createContact token reqBody
            tg $ replyText $
                "contact " <> contactUsername <> " successfully added"
           | Just _ <- matchUnion @(WithStatus 404 ()) u -> do
            tg $ replyText $
              "User " <> contactUsername <> " is not registered in check-check"
           | otherwise -> error "unreachable"

      (contactUsername, mContactName) -> case mkTextMaxLen mContactName of
        Nothing -> tg $ replyText $
          "contact name " <> mContactName <> " is too long, 50 symbols is the max length"
        Just contactName -> do
          u <- runReq $ getUser token (UserUsernameQuery contactUsername)
          if | Just AuthServiceUser{ userId } <- matchUnion @AuthServiceUser u -> do
              let reqBody = CreateContactReqBody
                    { contactUserId = userId
                    , contactName = Just contactName
                    }
              runReq_ $ createContact token reqBody
              tg $ replyText $
                  "contact " <> contactUsername <> " successfully added as " <> mContactName
             | Just _ <- matchUnion @(WithStatus 404 ()) u -> do
              tg $ replyText $
                "User " <> contactUsername <> " is not registered in check-check"
             | otherwise -> error "unreachable"
