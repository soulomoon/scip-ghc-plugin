{-# LANGUAGE OverloadedStrings #-}

module MyPlugin (plugin) where

import GHC.Plugins
import GHC.Tc.Types
import GHC.Hs
import Ext.Ast
import Ext.Types

-- This is the entry point for the plugin
plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = install,
    typeCheckResultAction = typechecked,
    renamedResultAction = renamedResult
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

renamedResult :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
renamedResult _ env hsGroup = do
    return (env{tcg_rn_decls=Just hsGroup}, hsGroup)


typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typechecked _ ms env = do
    let (Just renamed) = tcg_rn_decls env
        ex = tcg_rn_imports env
        ix = tcg_rn_exports env
        (lhrn, xrec) = tcg_hdr_info env
    hf <- mkHieFile ms env (renamed, ex, ix, lhrn, xrec)
    liftIO $ putStrLn $ showSDocUnsafe $ ppr (renamed, ex, ix, lhrn, xrec)
    liftIO $ putStrLn $ showSDocUnsafe $ ppr (hie_asts hf)
    return env
