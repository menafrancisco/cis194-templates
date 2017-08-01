{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import           Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
--parseMessage = undefined
parseMessage m = case messageCode of
                   "E" -> LogMessage (Error errorCode) timeStamp stringMessage
                   "I" -> LogMessage Info timeStamp stringMessage
                   "W" -> LogMessage Warning timeStamp stringMessage
                   _ -> Unknown stringMessage
    where
        messageWord= words m
        messageCode= head messageWord
        index = case messageCode of
                   "E" -> 2
                   "I" -> 1
                   "W" -> 1
                   _   -> -1
        stringMessage = unwords( drop (index+1) messageWord)
        timeStamp = read (messageWord!!index):: Int
        errorCode = read (messageWord!!1) :: Int



parse :: String -> [LogMessage]
--parse :: String -> [String]
--parse = undefined
parse [] = []
parse n = xz :  parse rest
    where
      listSr = words n
      head1 = head listSr
      head'= unwords (takeWhile (\ x -> x /= "E" && x/="W" && x/="I" ) (tail listSr))
      head''= head1 ++" "++ head'
      index'= length head'' + 1
      rest = drop index' n
      xz= parseMessage head''

-------testParse parse 10 "sample.log"

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- >>> insert (LogMessage Info 5 "Info5") (Node Leaf (LogMessage (Error 2) 2 "Error2") Leaf)
-- Node Leaf (LogMessage Info 5 "Info5") (Node Leaf (LogMessage (Error 2) 2 "Error2") Leaf)
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
--insert = undefined
insert (Unknown _) messageTree = messageTree
insert logMessage messageTree  = Node Leaf logMessage messageTree

----------------------------------------------------------------------
-- Exercise 3
---------------------------------------------whatWentWrong = undefined--------------------------

-- >>> build [LogMessage Info 5 "Info5", LogMessage (Error 2) 2 "Error2"]
-- Node Leaf (LogMessage Info 5 "Info5") (Node Leaf (LogMessage (Error 2) 2 "Error2") Leaf)
-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
--build = undefined
build = foldr (Node Leaf) Leaf
-- build [] = Leaf
-- build (logMessage:logMessages) = Node Leaf logMessage (build logMessages)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- >>> inOrder $ Node Leaf (LogMessage Info 5 "Info5") (Node Leaf (LogMessage (Error 2) 2 "Error2") Leaf)
-- >>> inOrder $ Node (Node Leaf (LogMessage (Error 7) 7 "Error7") Leaf) (LogMessage Info 5 "Info5") (Node Leaf (LogMessage (Error 2) 2 "Error2") Leaf)
-- [LogMessage Info 5 "Info5",LogMessage (Error 2) 2 "Error2"]
-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
--inOrder = undefined
inOrder Leaf = []
inOrder (Node Leaf logMessage Leaf) = [logMessage]
inOrder (Node Leaf logMessage messageTreeR) = logMessage : inOrder messageTreeR
inOrder (Node messageTreeL logMessage Leaf) = inOrder messageTreeL ++ [logMessage]
inOrder (Node messageTreeL logMessage messageTreeR) =  inOrder messageTreeL ++ [logMessage] ++ inOrder messageTreeR

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------


-- >>> whatWentWrong [LogMessage (Error 1) 1 "Error1", LogMessage (Error 2) 2 "Error2", LogMessage Info 5 "Info5"]
-- ["Error1","Error2"]
-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
--whatWentWrong = undefined
whatWentWrong [] = []
whatWentWrong (LogMessage (Error _) _ message:lms) = message : whatWentWrong lms
whatWentWrong (_:logMessages) = whatWentWrong logMessages

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = undefined
