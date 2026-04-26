{ config, lib, ... }:

let
  cfg = config.my.nixos.arrs;
  user = config.my.nixos.mainUser;
  inherit (cfg) group;
  inherit (cfg.sabnzbd) port;

  toINI =
    sections:
    lib.generators.toINIWithGlobalSection
      {
        mkKeyValue =
          k: v:
          if lib.isAttrs v then
            "[[${k}]]\n"
            + lib.generators.toINIWithGlobalSection { } {
              globalSection = v;
            }
          else
            lib.generators.mkKeyValueDefault {
            } "=" k v;
      }
      {
        globalSection = { };
        inherit sections;
      };

  publicSettings = {
    misc = {
      inherit port;

      bandwidth_max = "";
      bandwidth_perc = 100;
      cache_limit = "1G";
      email_endjob = "never";
      email_from = "";
      email_full = false;
      email_rss = false;
      email_server = "";
      email_to = "";
      enable_https = false;
      host = "0.0.0.0";
      html_login = true;
      https_cert = null;
      https_key = null;
      inet_exposure = "api+web (locally no auth)";

      action_on_unwanted_extensions = 0;
      admin_dir = "admin";
      allow_incomplete_nzb = false;
      allow_old_ssl_tls = false;
      ampm = false;
      api_logging = true;
      api_warnings = true;
      auto_browser = false;
      auto_disconnect = true;
      auto_sort = "";
      backup_dir = "";
      backup_for_duplicates = false;
      check_new_rel = true;
      cleanup_list = ",";
      complete_dir = "Downloads/complete";
      complete_free = "";
      config_conversion_version = 4;
      config_lock = false;
      date_categories = "tv,";
      date_sort_string = "";
      deobfuscate_final_filenames = true;
      direct_unpack = true;
      direct_unpack_tested = true;
      direct_unpack_threads = 3;
      dirscan_dir = "";
      dirscan_speed = 5;
      disable_archive = false;
      disable_par2cmdline = false;
      download_dir = "Downloads/incomplete";
      download_free = "";
      downloader_sleep_time = 10;
      dupes_propercheck = true;
      email_account = "";
      email_cats = "*,";
      email_dir = "";
      email_pwd = "";
      empty_postproc = false;
      enable_7zip = true;
      enable_all_par = false;
      enable_broadcast = true;
      enable_date_sorting = false;
      enable_filejoin = true;
      enable_https_verification = true;
      enable_movie_sorting = false;
      enable_par_cleanup = true;
      enable_recursive = true;
      enable_season_sorting = true;
      enable_tsjoin = true;
      enable_tv_sorting = false;
      enable_unrar = true;
      end_queue_script = "None";
      episode_rename_limit = "20M";
      ext_rename_ignore = ",";
      fail_hopeless_jobs = true;
      fast_fail = true;
      fixed_ports = true;
      flat_unpack = false;
      folder_rename = true;
      fulldisk_autoresume = false;
      helpful_warnings = true;
      history_limit = 10;
      history_retention = "";
      history_retention_number = 1;
      history_retention_option = "all";
      host_whitelist = "skrapnel, skrapnel.zero, skrapnel.lan, skrapnel.gumiho-matrix.ts.net";
      https_chain = "";
      https_port = "";
      ignore_samples = 0;
      ignore_unrar_dates = false;
      interface_settings = "";
      ionice = "";
      ipv6_hosting = false;
      ipv6_servers = true;
      ipv6_staging = false;
      keep_awake = true;
      language = "en";
      local_ranges = ",";
      log_dir = "logs";
      max_art_tries = 3;
      max_foldername_length = 246;
      max_url_retries = 10;
      movie_categories = "movies,";
      movie_rename_limit = "100M";
      movie_sort_extra = "-cd%1";
      movie_sort_string = "";
      new_nzb_on_failure = false;
      nice = "";
      no_dupes = false;
      no_penalties = false;
      no_series_dupes = false;
      no_smart_dupes = false;
      nomedia_marker = "";
      notified_new_skin = 2;
      nzb_backup_dir = "";
      outgoing_nntp_ip = "";
      overwrite_files = false;
      par_option = "";
      password_file = "";
      pause_on_post_processing = 0;
      pause_on_pwrar = true;
      permissions = "";
      pre_check = 0;
      pre_script = "None";
      preserve_paused_state = false;
      process_unpacked_par2 = true;
      propagation_delay = 0;
      queue_complete = "";
      queue_complete_pers = false;
      queue_limit = 20;
      quick_check_ext_ignore = "nfo, sfv, srr";
      quota_day = "";
      quota_period = "m";
      quota_resume = false;
      quota_size = "";
      receive_threads = 2;
      refresh_rate = 1;
      replace_dots = false;
      replace_spaces = false;
      replace_underscores = false;
      req_completion_rate = "100.2";
      rss_filenames = false;
      rss_odd_titles = "nzbindex.nl/, nzbindex.com/, nzbclub.com/";
      rss_rate = 60;
      safe_postproc = true;
      sanitize_safe = false;
      schedlines = ",";
      script_can_fail = false;
      script_dir = "";
      selftest_host = "self-test.sabnzbd.org";
      sfv_check = true;
      size_limit = 0;
      socks5_proxy_url = "";
      sorters_converted = 1;
      ssdp_broadcast_interval = 15;
      start_paused = false;
      switchinterval = "0.005";
      top_only = false;
      tray_icon = true;
      tv_categories = "tv,";
      tv_sort_string = "";
      unrar_parameters = "";
      unwanted_extensions = ",";
      unwanted_extensions_mode = 0;
      url_base = "/sabnzbd";
      verify_xff_header = false;
      wait_ext_drive = 5;
      wait_for_dfolder = false;
      warn_dupl_jobs = false;
      web_color = "Auto";
      web_dir = "Glitter";
      win_process_prio = 3;
      x_frame_options = true;
    };

    ntfosd = {
      ntfosd_enable = true;
      ntfosd_cats = "*,";
      ntfosd_prio_complete = 1;
      ntfosd_prio_disk_full = 1;
      ntfosd_prio_download = 0;
      ntfosd_prio_error = 0;
      ntfosd_prio_failed = 1;
      ntfosd_prio_new_login = 0;
      ntfosd_prio_other = 1;
      ntfosd_prio_pause_resume = 0;
      ntfosd_prio_pp = 0;
      ntfosd_prio_queue_done = 0;
      ntfosd_prio_quota = 1;
      ntfosd_prio_startup = 0;
      ntfosd_prio_warning = 0;
    };

    servers."news.newshosting.com" = {
      enable = true;
      connections = 60;
      displayname = "news.newshosting.com";
      expire_date = null;
      host = "news.newshosting.com";
      name = "news.newshosting.com";
      optional = false;
      port = 563;
      priority = 0;
      required = false;
      ssl = true;
      ssl_verify = "strict";
      timeout = 60;

      notes = "";
      quota = "";
      retention = 0;
      usage_at_start = 0;
    };

    logging = {
      log_backups = 5;
      log_level = 1;
      max_log_size = 5242880;
    };

    ncenter = {
      ncenter_enable = false;
      ncenter_cats = "*,";
      ncenter_prio_complete = 1;
      ncenter_prio_disk_full = 1;
      ncenter_prio_download = 0;
      ncenter_prio_error = 0;
      ncenter_prio_failed = 1;
      ncenter_prio_new_login = 0;
      ncenter_prio_other = 1;
      ncenter_prio_pause_resume = 0;
      ncenter_prio_pp = 0;
      ncenter_prio_queue_done = 0;
      ncenter_prio_quota = 1;
      ncenter_prio_startup = 0;
      ncenter_prio_warning = 0;
    };

    acenter = {
      acenter_enable = false;
      acenter_cats = "*,";
      acenter_prio_complete = 1;
      acenter_prio_disk_full = 1;
      acenter_prio_download = 0;
      acenter_prio_error = 0;
      acenter_prio_failed = 1;
      acenter_prio_new_login = 0;
      acenter_prio_other = 1;
      acenter_prio_pause_resume = 0;
      acenter_prio_pp = 0;
      acenter_prio_queue_done = 0;
      acenter_prio_quota = 1;
      acenter_prio_startup = 0;
      acenter_prio_warning = 0;
    };

    prowl = {
      prowl_enable = false;
      prowl_apikey = "";
      prowl_cats = "*,";
      prowl_prio_complete = 0;
      prowl_prio_disk_full = 1;
      prowl_prio_download = -3;
      prowl_prio_error = -3;
      prowl_prio_failed = 1;
      prowl_prio_new_login = -3;
      prowl_prio_other = 0;
      prowl_prio_pause_resume = -3;
      prowl_prio_pp = -3;
      prowl_prio_queue_done = -3;
      prowl_prio_quota = 0;
      prowl_prio_startup = -3;
      prowl_prio_warning = -3;
    };

    pushover = {
      pushover_enable = false;
      pushover_cats = "*,";
      pushover_device = "";
      pushover_emergency_expire = 3600;
      pushover_emergency_retry = 60;
      pushover_prio_complete = -1;
      pushover_prio_disk_full = 1;
      pushover_prio_download = -2;
      pushover_prio_error = 1;
      pushover_prio_failed = -1;
      pushover_prio_new_login = -3;
      pushover_prio_other = -1;
      pushover_prio_pause_resume = -2;
      pushover_prio_pp = -3;
      pushover_prio_queue_done = -3;
      pushover_prio_quota = -1;
      pushover_prio_startup = -3;
      pushover_prio_warning = 1;
      pushover_token = "";
      pushover_userkey = "";
    };

    pushbullet = {
      pushbullet_enable = false;
      pushbullet_apikey = "";
      pushbullet_cats = "*,";
      pushbullet_device = "";
      pushbullet_prio_complete = 1;
      pushbullet_prio_disk_full = 1;
      pushbullet_prio_download = 0;
      pushbullet_prio_error = 0;
      pushbullet_prio_failed = 1;
      pushbullet_prio_new_login = 0;
      pushbullet_prio_other = 1;
      pushbullet_prio_pause_resume = 0;
      pushbullet_prio_pp = 0;
      pushbullet_prio_queue_done = 0;
      pushbullet_prio_quota = 1;
      pushbullet_prio_startup = 0;
      pushbullet_prio_warning = 0;
    };

    apprise = {
      apprise_enable = false;
      apprise_cats = "*,";
      apprise_target_complete = "";
      apprise_target_complete_enable = true;
      apprise_target_disk_full = "";
      apprise_target_disk_full_enable = false;
      apprise_target_download = "";
      apprise_target_download_enable = false;
      apprise_target_error = "";
      apprise_target_error_enable = false;
      apprise_target_failed = "";
      apprise_target_failed_enable = true;
      apprise_target_new_login = "";
      apprise_target_new_login_enable = true;
      apprise_target_other = "";
      apprise_target_other_enable = true;
      apprise_target_pause_resume = "";
      apprise_target_pause_resume_enable = false;
      apprise_target_pp = "";
      apprise_target_pp_enable = false;
      apprise_target_queue_done = "";
      apprise_target_queue_done_enable = false;
      apprise_target_quota = "";
      apprise_target_quota_enable = true;
      apprise_target_startup = "";
      apprise_target_startup_enable = false;
      apprise_target_warning = "";
      apprise_target_warning_enable = false;
      apprise_urls = "";
    };

    nscript = {
      nscript_enable = false;
      nscript_cats = "*,";
      nscript_parameters = "";
      nscript_prio_complete = 1;
      nscript_prio_disk_full = 1;
      nscript_prio_download = 0;
      nscript_prio_error = 0;
      nscript_prio_failed = 1;
      nscript_prio_new_login = 0;
      nscript_prio_other = 1;
      nscript_prio_pause_resume = 0;
      nscript_prio_pp = 0;
      nscript_prio_queue_done = 0;
      nscript_prio_quota = 1;
      nscript_prio_startup = 0;
      nscript_prio_warning = 0;
      nscript_script = "";
    };

    categories = {
      "*" = {
        name = "*";
        order = 0;
        pp = 3;
        script = "None";
        dir = "";
        newzbin = "";
        priority = 0;
      };
      movies = {
        name = "movies";
        order = 1;
        pp = "";
        script = "Default";
        dir = "";
        newzbin = "";
        priority = -100;
      };
      tv = {
        name = "tv";
        order = 2;
        pp = "";
        script = "Default";
        dir = "";
        newzbin = "";
        priority = -100;
      };
      audio = {
        name = "audio";
        order = 3;
        pp = "";
        script = "Default";
        dir = "";
        newzbin = "";
        priority = -100;
      };
      software = {
        name = "software";
        order = 4;
        pp = "";
        script = "Default";
        dir = "";
        newzbin = "";
        priority = -100;
      };
      prowlarr = {
        name = "prowlarr";
        order = 5;
        pp = "";
        script = "Default";
        dir = "";
        newzbin = "";
        priority = -100;
      };
      music = {
        name = "music";
        order = 6;
        pp = "";
        script = "Default";
        dir = "";
        newzbin = "";
        priority = -100;
      };
    };
  };

  privateSettings = {
    misc = {
      api_key = config.sops.placeholder.sabnzbd-api-key;
      nzb_key = config.sops.placeholder.sabnzbd-nzb-key;
      password = config.sops.placeholder.sabnzbd-password;
      username = config.sops.placeholder.sabnzbd-username;
    };
    servers."news.newshosting.com" = {
      password = config.sops.placeholder.newshosting-password;
      username = config.sops.placeholder.newshosting-username;
    };
  };
in
{
  options.my.nixos.arrs.sabnzbd = {
    port = lib.mkOption {
      type = lib.types.port;
      description = ''
        Port for the sabnzbd Web UI to listen on for incoming connections.
      '';
      default = 8080;
      example = 12345;
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.my.nixos.sops.enable;
        message = "sops must be enabled to use sabnzbd module";
      }
    ];

    sops = {
      secrets = {
        newshosting-password = { };
        newshosting-username = { };
        sabnzbd-api-key = { };
        sabnzbd-nzb-key = { };
        sabnzbd-password = { };
        sabnzbd-username = { };
      };
      templates."sabnzbd-secrets.ini" = {
        content = toINI privateSettings;
        inherit group;
        owner = user;
      };
    };

    services.sabnzbd = {
      enable = true;

      inherit group user;
      configFile = null;
      openFirewall = true;
      settings = publicSettings;
      secretFiles = [ config.sops.templates."sabnzbd-secrets.ini".path ];
    };
  };
}
