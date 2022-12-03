(import ./default.nix { }).shellFor {
    tools = {
        cabal = "latest";
        hlint = "latest";
        haskell-language-server = "latest";
        markdown-unlit = "latest";
    };
    
    withHoogle = true;

    exactDeps = true;
}