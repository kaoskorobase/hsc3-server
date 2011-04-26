module Sound.SC3.Server.Connection.ListenerMap.List where

import Sound.OpenSoundControl (OSC)

type ListenerId  = Int
type Listener    = OSC -> IO ()
data ListenerMap = ListenerMap [(ListenerId, Listener)] !ListenerId

broadcast :: OSC -> ListenerMap -> IO ()
broadcast osc (ListenerMap m _) = mapM_ (\(_, l) -> l osc) m

empty :: IO ListenerMap
empty = return $ ListenerMap [] 0

add :: Listener -> ListenerMap -> IO (ListenerId, ListenerMap)
add l (ListenerMap m i) = return (i, ListenerMap ((i,l):m) (i+1))

deletePred :: (a -> Bool) -> [a] -> [a]
deletePred _ [] = []
deletePred f (x:xs) =
    if f x
    then xs
    else x : deletePred f xs

delete :: ListenerId -> ListenerMap -> IO ListenerMap
delete uid (ListenerMap m i) = return (ListenerMap  (deletePred (\(uid', _) -> uid' == uid) m) i)
