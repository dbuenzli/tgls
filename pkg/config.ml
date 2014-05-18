#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default
  
  let build_support = Some "pkg/build_support.ml"
  let git_hook = build_support 
  let distrib_hook = build_support 
  
  let distrib_remove = "support" :: distrib_remove

  let vars =
    [ "NAME", "tgls";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "Daniel Bünzli <daniel.buenzl i\\@erratique.ch>" ]
end
