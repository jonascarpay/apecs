{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Debug.Trace (trace)


main = do
    pkgDescr <- readPackageDescription verbose "apecs-physics.cabal"
    let newPkgDescr = addCFilesIfNeeded pkgDescr
    print newPkgDescr
    defaultMainNoRead newPkgDescr
  where
    addCFilesIfNeeded genPkgDescr =
#if __GLASGOW_HASKELL__ >= 802
      genPkgDescr
#else
      addCFiles genPkgDescr
#endif

    addCFiles genPkgDescr = genPkgDescr
      { condLibrary = fmap addCFiles' pkgLibrary
      }
      where
        pkgLibrary = condLibrary genPkgDescr 

        addCFiles' (CondNode lib constraints components) =
          CondNode (addCFiles'' lib) constraints components

        addCFiles'' lib = lib
          { libBuildInfo =
              let buildInfo = libBuildInfo lib in
                buildInfo
                { cSources = cSources buildInfo ++
                    [ "src/Apecs/Physics/Constraint.c"
                    , "src/Apecs/Physics/Space.c"
                    , "src/Apecs/Physics/Query.c"
                    , "src/Apecs/Physics/Body.c"
                    , "src/Apecs/Physics/Collision.c"
                    , "src/Apecs/Physics/Shape.c"
                    ]
                }
          }

