{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Char 

-- Parses an individual line
-- > parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- > parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- > parseMessage "This is not in the right format"
-- >   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage x = let wordlist = words x in 
                 case wordlist of
                    ("I":ts:msg) -> LogMessage Info (read ts :: Int) (unwords msg)
                    ("W":ts:msg) -> LogMessage Warning (read ts :: Int) (unwords msg)
                    ("E":lvl:ts:msg) -> LogMessage (Error (read lvl :: Int)) 
                                        (read ts :: Int) (unwords msg)
                    _ -> Unknown (unwords wordlist)


-- Parse the log file and return it's contents
parse :: String -> [LogMessage]
parse inpFile = map parseMessage  (lines inpFile)

-- Insert Log messages into Binary Search Tree.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (LogMessage msgType ts msg) (Node ltree (LogMessage msgTypeNode tsNode msgNode) rtree) 
    | ts < tsNode = Node 
                        (insert (LogMessage msgType ts msg) ltree) 
                        (LogMessage msgTypeNode tsNode msgNode) 
                        rtree
    | ts > tsNode = Node 
                        ltree 
                        (LogMessage msgTypeNode tsNode msgNode) 
                        (insert (LogMessage msgType ts msg) rtree)

-- Build a Binary Tree from a list of log Messages
build :: [LogMessage] -> MessageTree
build [] = Leaf
build xs = insert (head xs) (build (tail xs))

-- Inorder Traversal of Tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree logMessage rtree) = 
        inOrder ltree ++ [logMessage] ++ inOrder rtree

-- Extract the errors with a severity of atleast 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = 
    [ msg | (LogMessage msgType ts msg) <- inOrder(build xs), 
            checkMsgSeverity msgType 50  ]

-- Check if the msgType is Error and check if the lvl of error is greater than n
checkMsgSeverity :: MessageType -> Int -> Bool
checkMsgSeverity (Error msglvl) lvl = msglvl > lvl
checkMsgSeverity _ lvl = False