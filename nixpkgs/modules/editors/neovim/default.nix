{ pkgs, ... }: {

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  programs.neovim = {
    enable = true;

    viAlias = true;
    vimAlias = true;

    extraConfig = ''
      :set number
    '';

    plugins = with pkgs.vimPlugins; [
      ctrlp
      editorconfig-vim
      gruvbox
      nerdtree
      tabular
      vim-nix
      vim-markdown
    ];
  };
}
