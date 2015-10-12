{ config, lib, pkgs, ... }:

let 
  client  = pkgs.haskell.packages.ghcjs.paisley-client;
  server  = pkgs.haskellPackages.paisley-client;

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

      port = mkOption = {
        type = types.int;
        default = 8125;
        description = ''
          The port on which the server runs.
        '';
      };

      dataDir = mkOption {
        type = types.path;
        default = "/var/db/acidstate/paisley";
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

  config = mkIf config.services.paisly.enable {

    users.extraUsers.paisley = {
      name = "paisley"
      group = "paisley"
      description = "Paisley payments form user";
    };

    systemd.services.paisley = {
      description = "Paisley payments system";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

     
      path = [ paisley.server paisley.client ];
    
      preStart = ''
        if ! test -e ${cfg.dataDir}; then
          mkdir -m 0700 -p ${cfg.dataDir}
          chown -R paisley ${cfg.dataDir} 
        fi
      ''; 
    
      serviceConfig = {
        ExecStart = ''
          ${paisley-server}/bin/paisley-server \
            -d ${cfg.dataDir} -c ${paisley-client} -p ${cfg.port}
        '';
        User = "paisley";
        Group = "paisley";
        PermissionsStartOnly = true; # We need the prestart step to run as root, so as to create the data directory.
      }; 

      unitconfig.RequiresMountsFor = "${cfg.dataDir}";
  };};
   
}
