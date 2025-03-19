module Telegram.Bot.FSA.Transitions.CancelSelectingRequestRecipient (handleTransition) where

import Telegram.Bot.FSA (State(InitialState))

handleTransition :: Applicative f => State -> f State
handleTransition _ = pure InitialState
