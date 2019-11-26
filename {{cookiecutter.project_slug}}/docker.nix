{
  my-service-static = pkgs.haskell.lib.justStaticExecutables haskellPackages.{{cookiecutter.project_slug}};
  dockerImage = pkgs.dockerTools.buildImage {
    name = "{{cookiecutter.project_slug}}";
    tag = "latest";
    contents = "${pkgs.final.my-service-static}/bin";
    config = {
      Cmd = ["${pkgs.final.my-service-static}/bin/{{cookiecutter.project_slug}}"];
    };
  };
}
