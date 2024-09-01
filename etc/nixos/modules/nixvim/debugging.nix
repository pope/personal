{ pkgs, helpers, ... }:

let
  inherit (import ./lib.nix { inherit helpers; }) mkLazyKeys;
  ezkeymap = desc: key: cmd: {
    lhs = "${key}";
    rhs = "<cmd>lua require('dap').${cmd}()<cr>";
    inherit desc;
  };
  ezuikeymap = desc: key: cmd: {
    lhs = "${key}";
    rhs = "<cmd>lua require('dap.ui.widgets').${cmd}()<cr>";
    mode = [ "n" "v" ];
    inherit desc;
  };
  uikeymap = desc: key: cmd: {
    lhs = "${key}";
    rhs = helpers.mkRaw /* lua */ ''
      function()
        local widgets = require('dap.ui.widgets')
        widgets.centered_float(widgets.${cmd})
      end
    '';
    inherit desc;
  };
in
{
  config.plugins.lazy.plugins = with pkgs.vimPlugins; [
    {
      pkg = nvim-dap;
      dependencies = [
        nvim-dap-ui
        nvim-nio
        telescope-nvim
        telescope-dap-nvim
      ];
      keys = mkLazyKeys [
        (ezkeymap "Debug toggle breakpoint" "<leader>b" "toggle_breakpoint")
        (ezkeymap "Debug set breakpoint" "<leader>B" "set_breakpoint")
        (ezkeymap "Debug continue" "<F5>" "continue")
        (ezkeymap "Debug continue" "<leader>dc" "continue")
        (ezkeymap "Debug terminate" "<leader>dt" "terminate")
        (ezkeymap "Debug step over" "<F10>" "step_over")
        (ezkeymap "Debug step over" "<leader>do" "step_over")
        (ezkeymap "Debug step into" "<F11>" "step_into")
        (ezkeymap "Debug step into" "<leader>di" "step_into")
        (ezkeymap "Debug step out" "<F12>" "step_out")
        (ezkeymap "Debug step out" "<leader>dO" "step_out")
        (ezkeymap "Debug REPL open" "<leader>dr" "repl.open")
        (ezkeymap "Debug run last" "<leader>dl" "run_last")
        (ezuikeymap "Debug hover" "<leader>dh" "hover")
        (ezuikeymap "Debug preview" "<leader>dp" "preview")
        (uikeymap "Debug frames" "<leader>df" "frames")
        (uikeymap "Debug scopes" "<leader>ds" "scopes")
        {
          lhs = "<leader>dC";
          rhs = "<cmd>lua require('telescope').extensions.dap.configurations()<cr>";
          desc = "Debug telescope configurations";
        }
      ];
      config = /* lua */ ''
        function ()
          local dap = require("dap")

          -- C & C++
          dap.adapters.gdb = {
            type = "executable",
            command = "gdb",
            args = { "-i", "dap" }
          }
          dap.adapters.lldb = {
            type = 'executable',
            command = '${pkgs.lldb}/bin/lldb-vscode', -- adjust as needed, must be absolute path
            name = 'lldb'
          }
          dap.configurations.c = {
            {
              name = "Launch (gdb)",
              type = "gdb",
              request = "launch",
              program = function()
                return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
              end,
              cwd = "''${workspaceFolder}",
              stopAtBeginningOfMainSubprogram = false,
            },
            {
              name = "Launch (lldb)",
              type = "lldb",
              request = "launch",
              program = function()
                return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
              end,
              cwd = "''${workspaceFolder}",
              stopOnEntry = false,
              args = {},
            },
          }
          dap.configurations.cpp = dap.configurations.c

          -- Godot
          dap.adapters.godot = {
            type = "server",
            host = "127.0.0.1",
            port = 6006,
          }
          dap.configurations.gdscript = {
            {
              name = "Launch scene",
              type = "godot",
              request = "launch",
              project = "''${workspaceFolder}",
              launch_scene = true,
            },
          }

          local sign = vim.fn.sign_define
          sign("DapBreakpoint", { text = "", texthl = "ErrorMsg", linehl = "", numhl = "" })
          sign("DapLogPoint", { text = "", texthl = "ErrorMsg", linehl = "", numhl = "" })
          sign("DapBreakpointCondition", { text = "", texthl = "ErrorMsg", linehl = "", numhl = "" })
          sign("DapBreakpointRejected", { text = "", texthl = "ErrorMsg", linehl = "", numhl = "" })
          sign("DapStopped", { text = "", texthl = "ErrorMsg", linehl = "", numhl = "ErrorMsg" })
          -- sign("DapStopped", { text = "", texthl = "ErrorMsg", linehl = "", numhl = "ErrorMsg" })

          require("telescope").load_extension("dap")

          local dapui = require("dapui")
          dapui.setup()
          dap.listeners.before.attach.dapui_config = function()
            dapui.open()
          end
          dap.listeners.before.launch.dapui_config = function()
            dapui.open()
          end
          dap.listeners.before.event_terminated.dapui_config = function()
            dapui.close()
          end
          dap.listeners.before.event_exited.dapui_config = function()
            dapui.close()
          end
        end
      '';
    }
  ];
}
