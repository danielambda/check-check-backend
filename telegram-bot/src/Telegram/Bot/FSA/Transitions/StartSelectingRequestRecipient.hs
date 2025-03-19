{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Telegram.Bot.FSA.Transitions.StartSelectingRequestRecipient (handleTransition) where

import Optics ((&), view, (<&>))
import qualified Data.Text as T
import Telegram.Bot.API (InlineKeyboardMarkup(..), SomeReplyMarkup (..))
import Telegram.Bot.Simple (ReplyMessage (..), replyText, reply, toReplyMessage, actionButton)

import Control.Monad (forM)
import Data.Foldable (toList)
import Data.List.NonEmpty (nonEmpty)

import SmartPrimitives.TextLenRange (TextLenRange(..))
import SmartPrimitives.TextMaxLen (unTextMaxLen)
import CheckCheck.Contracts.Users.Contacts (ContactResp(..))
import Clients.Backend (getContacts)
import Clients.Utils (runReq)
import Models (ReceiptItem(..), UserContact(..))
import Telegram.Bot.AppM ((<#), tg, authViaTelegram, currentUser, Eff')
import Telegram.Bot.FSA
  ( State(SelectingReceiptItems, SelectingRequestRecipient)
  , Transition (CancelSelectingRequestRecipient, SelectRequestRecipient)
  )

handleTransition :: State -> Eff' Transition State
handleTransition (SelectingReceiptItems qr items) = case nonEmpty $ items & filter fst & map snd of
  Nothing -> SelectingReceiptItems qr items <# do
    tg $ replyText "this text has to be edited, btw you did not select anything"

  Just filteredItems -> SelectingRequestRecipient qr (view #index <$> filteredItems) <# do
    let overallSum = (`divide` 100) $ sum $ filteredItems <&>
          \ReceiptItem{ quantity, price } ->
            round (fromIntegral price * quantity)
    let replyMsgText = T.unlines
          $ ("В сумме на: " <> T.pack (show overallSum))
          : (view #name <$> toList filteredItems)
    token <- authViaTelegram =<< currentUser
    contactsResp <- runReq $ getContacts token
    let contacts = contactsResp <&> \ContactResp{..} ->
          UserContact{ mContactName = contactName, ..}
    buttons <- forM contacts $ \UserContact{ contactUsername = TextLenRange contactUsername, ..} -> do
      let name = maybe contactUsername unTextMaxLen mContactName
      return $ actionButton name (SelectRequestRecipient contactUserId)
    let buttons' = buttons ++ [actionButton "Cancel" CancelSelectingRequestRecipient]
    let replyMsg = (toReplyMessage replyMsgText)
          { replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ InlineKeyboardMarkup
            { inlineKeyboardMarkupInlineKeyboard = (:[]) <$> buttons' }
          }
    tg $ reply replyMsg
handleTransition _ = error "TODO"

divide :: Int -> Double -> Double
divide a b = fromIntegral a / b
