module Core.Users.Requests.Domain.RequestStatus (RequestStatus(..)) where

data RequestStatus = Pending | Done
  deriving (Eq)
