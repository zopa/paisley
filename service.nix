{ config, lib, pkgs, ... }:

with lib;

let

  client  = pkgs.haskell.packages.ghcjs.callPackage ./src/client {};
  server  = pkgs.haskellPackages.callPackage ./src/server {};

  cfg = config.services.paisley;

in
{

  options = {

    services.paisley = {

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run the Paisley Farms payment form server
        '';
      };

      port = mkOption {
        type = types.int;
        default = 8125;
        description = ''
          The port on which the server runs.
        '';
      };

      dataDir = mkOption {
        type = types.path;
        default = "/var/db/acid/paisley";
        description = ''
          Location of the AcidState log.
        '';
      };

      cert = mkOption {
        type = types.path;
        default = "/tmp/not-yet-implemented";
        description = ''
          Location of the certificate (for TLS support).
        '';
      };
  };};

  config = mkIf config.services.paisley.enable {

    users.extraGroups.paisley.name = "paisley";
    users.extraGroups.acid.name = "acid";
    users.extraUsers.paisley = {
      name = "paisley";
      group = "acid";
      description = "Paisley payments form user";
    };

    systemd.services.paisley = {
      description = "Paisley payments system";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];


      path = [ server client ];

      preStart = ''
        ACIDDIR=`dirname ${cfg.dataDir}`
        if ! test -e $ACIDDIR
          mkdir -m 3770  $ACIDDIR
          chown -R :acid $ACIDDIR
        if ! test -e ${cfg.dataDir}; then
          mkdir -m 3700 ${cfg.dataDir}
          chown -R paisley:paisley ${cfg.dataDir}
        fi
      '';

      serviceConfig = {
        ExecStart = ''
          ${server}/bin/paisley-server \
            -d ${cfg.dataDir} -i ${client} -p ${toString cfg.port}
        '';
        User = "paisley";
        Group = "paisley";
        PermissionsStartOnly = true; # We need the prestart step to run as root, so as to create the data directory.
      };

      unitConfig.RequiresMountsFor = "${cfg.dataDir}";
  };};

}
