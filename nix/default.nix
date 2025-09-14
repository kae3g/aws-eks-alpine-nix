# Haskell Application Build Configuration 💙
# This Nix expression defines how to build our Haskell application
# in a completely reproducible way using Nix's declarative package management

{ pkgs ? import <nixpkgs> {} }:

# Build a simple Haskell application
pkgs.haskellPackages.buildPackage {
  pname = "alpine-nix-demo";
  version = "1.0.0";
  
  # Source files - in a real project, these would be your Haskell source
  src = pkgs.writeText "Main.hs" ''
    module Main where

    import System.Environment
    import Network.Wai
    import Network.Wai.Handler.Warp
    import Network.HTTP.Types

    main :: IO ()
    main = do
        putStrLn "🚀 Alpine + Nix + Haskell Demo Server Starting..."
        putStrLn "💙 Built with declarative infrastructure!"
        port <- getEnv "PORT" `catch` \_ -> return "8080"
        run (read port) app

    app :: Application
    app req respond = respond $ case pathInfo req of
[] -> responseLBS status200 [("Content-Type", "text/html; charset=utf-8")] $
            mconcat
[ "<!DOCTYPE html><html><head><title>Alpine + Nix Demo</title></head><body>"
                , "<h1>💙 Hello from Alpine + Nix + Haskell!</h1>"
                , "<p>This application was built using:</p>"
                , "<ul>"
                , "<li>Alpine Linux (minimal, secure base)</li>"
                , "<li>Nix Package Manager (declarative dependencies)</li>"
                , "<li>Haskell (functional programming)</li>"
                , "<li>Warp (lightweight web server)</li>"
                , "</ul>"
, "<p><strong>Mission:</strong> Sovereign, reproducible, and secure
infrastructure</p>"
                , "</body></html>"
                ]
["health"] -> responseLBS status200 [("Content-Type", "application/json")]
"{\"status\":\"healthy\"}"
        _ -> responseLBS status404 [("Content-Type", "text/plain")] "Not Found"
  '';
  
  # Dependencies for our Haskell application
  buildDepends = with pkgs.haskellPackages; [
    wai
    warp
    http-types
  ];
  
  # Build tools
  buildTools = with pkgs.haskellPackages; [
    cabal-install
    ghc
  ];
  
  # Meta information
  meta = {
description = "A demonstration Haskell web application built with Alpine Linux
and Nix";
    license = pkgs.lib.licenses.mit;
    maintainers = [ "alpine-nix-team" ];
  };
}
