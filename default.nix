{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for cardano-marketplace.
#
# This file defines various attributes that are used for building and
# developing cardano-marketplace.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   cardano-marketplace: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs cardano-marketplace;
  project = cardano-marketplace.haskell.project;
in
{
  inherit pkgs cardano-marketplace;

  inherit project;
}
