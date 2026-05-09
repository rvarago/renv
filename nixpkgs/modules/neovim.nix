{ pkgs, ... }: {

  home.sessionVariables = {
    EDITOR = "nvim";
  };

  programs.neovim = {
    enable = true;

    withPython3 = true;
    withRuby = true;

    viAlias = true;
    vimAlias = true;

    extraConfig = ''
      :set number
    '';

    plugins = with pkgs.vimPlugins; [
      ctrlp-vim
      editorconfig-vim
      gruvbox
      nerdtree
      tabular
      vim-nix
      vim-markdown
    ];
  };
}
