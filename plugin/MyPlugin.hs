{-# LANGUAGE OverloadedStrings #-}

module MyPlugin (plugin) where

import GHC.Plugins

-- This is the entry point for the plugin
plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install
}

-- This function modifies the compilation pipeline
install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    putMsgS "Hello from the plugin!"
    return (CoreDoPluginPass "MyPluginPass" pass : todos)

-- This is the core-to-core pass
pass :: ModGuts -> CoreM ModGuts
pass guts = do
    -- Do something with the Core here
    putMsgS "Running my pass"
    return guts