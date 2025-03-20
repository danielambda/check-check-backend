{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSA.Transitions.SelectRequestRecipient (handleTransition) where

import Telegram.Bot.Simple (replyText)
import Data.UUID (UUID)

import Data.List.NonEmpty (singleton)

import CheckCheck.Contracts.Users.OutgoingRequests (SendRequestReqBody(..), IndexSelectionReqBody (..))
import Clients.Utils (runReq_)
import Clients.Backend (sendRequest)
import Telegram.Bot.AppM ((<#), tg, Eff', authViaTelegram, currentUser)
import Telegram.Bot.FSA (State(InitialState, SelectingRequestRecipient), Transition)

handleTransition :: UUID -> State -> Eff' Transition State
handleTransition recipientId (SelectingRequestRecipient qr indices) = InitialState <# do
  token <- authViaTelegram =<< currentUser
  let reqBody = SendReceiptItemsRequestReqBody
        { receiptQr = qr
        , indexSelections = singleton IndexSelectionReqBody{..}
        }
  runReq_ $ sendRequest token reqBody
  tg $ replyText "Request successfully sent"
handleTransition _ _ = error "TODO"

