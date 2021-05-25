{ pkgs, ... }: {

  programs.neovim = {
    enable = true;

    withPython = false;

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
