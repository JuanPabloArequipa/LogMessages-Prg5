module LogParser where

import Log  


parseMessage :: String -> LogMessage
parseMessage str = parseMessage' $ words str
  where
    parseMessage' :: [String] -> LogMessage
    parseMessage' ("I":timestamp:msg) = LogMessage Info (read timestamp) (unwords msg)
    parseMessage' ("W":timestamp:msg) = LogMessage Warning (read timestamp) (unwords msg)
    parseMessage' ("E":severity:timestamp:msg) = LogMessage (Error (read severity)) (read timestamp) (unwords msg)
    parseMessage' _ = Unknown str


parse :: String -> [LogMessage]
parse = map parseMessage . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _ ) tree = tree 
insert logMsg Leaf = Node Leaf logMsg Leaf


insert logMsg (Node left lgMsg right)
    | getTime logMsg <= getTime lgMsg = Node (insert logMsg left) lgMsg right
    | otherwise = Node left logMsg (insert logMsg right)


getTime :: LogMessage -> TimeStamp
getTime (LogMessage _ time _ ) = time
getTime (Unknown _ ) = 0


build :: [LogMessage] -> MessageTree
build = foldr insert Leaf


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . inOrder . build . filter isRelevantError . filter isSevereError
    where 
        isRelevantError (LogMessage (Error severity) _ _) = severity >= 50
        isRelevantError _ = False
        isSevereError (LogMessage (Error severity) _ _) = severity >= 50
        isSevereError _ = False
        extractMessage (LogMessage _ _ msg) = msg
        extractMessage _ = ""