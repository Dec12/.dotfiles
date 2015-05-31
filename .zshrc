# # -----------------------------
# # Import Local Settings
# # -----------------------------
for file in `find "$HOME/.zsh.d/" -name "*.sh"`; do 
   source "$file"
done


# # ----------------------------
# # Display
# # ----------------------------

case ${OSTYPE} in
    darwin*)
        eval $(gdircolors ~/.dotfiles/.dircolors-solarized/dircolors.256dark)
    ;;
    linux*)
        eval $(dircolors ~/.dotfiles/.dircolors-solarized/dircolors.256dark)
    ;;
esac


# # ----------------------------
# # Aliases
# # ----------------------------

case ${OSTYPE} in
    darwin*)
        alias ls='gls --color=auto'
    ;;
    linux*)
        alias ls='ls --color=auto'
    ;;
esac

alias la='ls -Alh'
alias ll='ls -lh'
alias l=ls
alias p=pushd pp=popd
alias df='df -h'
alias du='du -h'
alias tmux='tmux -2'


# # ------------------------------
# # General Settings
# # ------------------------------

# Keybinding And Charsets
export EDITOR=emacs      # エディタをemacsに設定
export LANG=ja_JP.UTF-8  # 文字コードをUTF-8に設定
export KCODE=u           # KCODEにUTF-8を設定
export AUTOFEATURE=true  # autotestでfeatureを動かす
bindkey -e               # キーバインドをemacsモードに設定

# Utils
setopt no_beep           # ビープ音を鳴らさないようにする
setopt auto_cd           # ディレクトリ名の入力のみで移動する 
setopt auto_pushd        # cd時にディレクトリスタックにpushdする
setopt correct           # コマンドのスペルを訂正する
setopt magic_equal_subst # =以降も補完する(--prefix=/usrなど)
setopt prompt_subst      # プロンプト定義内で変数置換やコマンド置換を扱う
setopt notify            # バックグラウンドジョブの状態変化を即時報告する
setopt equals            # =commandを`which command`と同じ処理にする

# Complement 
autoload -Uz compinit    # 補完機能を有効にする
compinit
setopt auto_list         # 補完候補を一覧で表示する(d)
setopt auto_menu         # 補完キー連打で補完候補を順に表示する(d)
setopt list_packed       # 補完候補をできるだけ詰めて表示する
setopt list_types        # 補完候補にファイルの種類も表示する
bindkey "^[[Z" reverse-menu-complete                # Shift-Tabで補完候補を逆順する("\e[Z"でも動作する)
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # 補完時に大文字小文字を区別しない
zstyle ':completion:*:default' menu select=1

# Glob #
setopt extended_glob      # グロブ機能を拡張する
unsetopt caseglob         # ファイルグロブで大文字小文字を区別しない

# History #
HISTFILE=~/.zsh_history   # ヒストリを保存するファイル
HISTSIZE=10000            # メモリに保存されるヒストリの件数
SAVEHIST=10000            # 保存されるヒストリの件数
setopt bang_hist          # !を使ったヒストリ展開を行う(d)
setopt extended_history   # ヒストリに実行時間も保存する
setopt hist_ignore_dups   # 直前と同じコマンドはヒストリに追加しない
setopt share_history      # 他のシェルのヒストリをリアルタイムで共有する
setopt hist_reduce_blanks # 余分なスペースを削除してヒストリに保存する

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

function history-all { history -E 1 }
function chpwd() { ls }


# # -----------------------------
# # VCS
# # http://qiita.com/mollifier/items/8d5a627d773758dd8078
# # http://qiita.com/Fuhduki/items/ca32ae955ad5f6c54a81
# # -----------------------------

# vcs_info 設定

autoload -Uz vcs_info
autoload -Uz add-zsh-hook
autoload -Uz is-at-least
autoload -Uz colors

# 以下の3つのメッセージをエクスポートする
#   $vcs_info_msg_0_ : 通常メッセージ用 (緑)
#   $vcs_info_msg_1_ : 警告メッセージ用 (黄色)
#   $vcs_info_msg_2_ : エラーメッセージ用 (赤)
zstyle ':vcs_info:*' max-exports 3

zstyle ':vcs_info:*' enable git svn hg bzr
# 標準のフォーマット(git 以外で使用)
# misc(%m) は通常は空文字列に置き換えられる
zstyle ':vcs_info:*' formats '[%b]'
zstyle ':vcs_info:*' actionformats '[%b]' '%m' '<!%a>'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

if is-at-least 4.3.10; then
    # git 用のフォーマット
    # git のときはステージしているかどうかを表示
    zstyle ':vcs_info:git:*' formats '[%b]' '%c%u%m'
    zstyle ':vcs_info:git:*' actionformats '[%b]' '%c%u%m' '<!%a>'
    zstyle ':vcs_info:git:*' check-for-changes true
    zstyle ':vcs_info:git:*' stagedstr "+"    # %c で表示する文字列
    zstyle ':vcs_info:git:*' unstagedstr "-"  # %u で表示する文字列
fi

# hooks 設定
if is-at-least 4.3.11; then
    # git のときはフック関数を設定する

    # formats '(%s)-[%b]' '%c%u %m' , actionformats '(%s)-[%b]' '%c%u %m' '<!%a>'
    # のメッセージを設定する直前のフック関数
    # 今回の設定の場合はformat の時は2つ, actionformats の時は3つメッセージがあるので
    # 各関数が最大3回呼び出される。
    zstyle ':vcs_info:git+set-message:*' hooks \
                                            git-hook-begin \
                                            git-untracked \
                                            git-push-status \
                                            git-nomerge-branch \
                                            git-stash-count

    # フックの最初の関数
    # git の作業コピーのあるディレクトリのみフック関数を呼び出すようにする
    # (.git ディレクトリ内にいるときは呼び出さない)
    # .git ディレクトリ内では git status --porcelain などがエラーになるため
    function +vi-git-hook-begin() {
        if [[ $(command git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
            # 0以外を返すとそれ以降のフック関数は呼び出されない
            return 1
        fi

        return 0
    }

    # untracked ファイル表示
    #
    # untracked ファイル(バージョン管理されていないファイル)がある場合は
    # unstaged (%u) に ? を表示
    function +vi-git-untracked() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        if command git status --porcelain 2> /dev/null \
            | awk '{print $1}' \
            | command grep -F '??' > /dev/null 2>&1 ; then

            # unstaged (%u) に追加
            hook_com[unstaged]+='?'
        fi
    }

    # push していないコミットの件数表示
    #
    # リモートリポジトリに push していないコミットの件数を
    # pN という形式で misc (%m) に表示する
    function +vi-git-push-status() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        if [[ "${hook_com[branch]}" != "master" ]]; then
            # master ブランチでない場合は何もしない
            return 0
        fi

        # push していないコミット数を取得する
        local ahead
        ahead=$(command git rev-list origin/master..master 2>/dev/null \
            | wc -l \
            | tr -d ' ')

        if [[ "$ahead" -gt 0 ]]; then
            # misc (%m) に追加
            hook_com[misc]+="(p${ahead})"
        fi
    }

    # マージしていない件数表示
    #
    # master 以外のブランチにいる場合に、
    # 現在のブランチ上でまだ master にマージしていないコミットの件数を
    # (mN) という形式で misc (%m) に表示
    function +vi-git-nomerge-branch() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        if [[ "${hook_com[branch]}" == "master" ]]; then
            # master ブランチの場合は何もしない
            return 0
        fi

        local nomerged
        nomerged=$(command git rev-list master..${hook_com[branch]} 2>/dev/null | wc -l | tr -d ' ')

        if [[ "$nomerged" -gt 0 ]] ; then
            # misc (%m) に追加
            hook_com[misc]+="(m${nomerged})"
        fi
    }


    # stash 件数表示
    #
    # stash している場合は :SN という形式で misc (%m) に表示
    function +vi-git-stash-count() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        local stash
        stash=$(command git stash list 2>/dev/null | wc -l | tr -d ' ')
        if [[ "${stash}" -gt 0 ]]; then
            # misc (%m) に追加
            hook_com[misc]+=":S${stash}"
        fi
    }

fi

function _update_vcs_info_msg() {
    psvar=()

    LANG=en_US.UTF-8 vcs_info

    if [[ -z ${vcs_info_msg_0_} ]]; then
        # vcs_info で何も取得していない場合はプロンプトを表示しない
        psvar[1]=""
        psvar[2]=""
        psvar[3]=""
    else
        # vcs_info で情報を取得した場合
        # $vcs_info_msg_0_ , $vcs_info_msg_1_ , $vcs_info_msg_2_ を
        # それぞれ緑、黄色、赤で表示する
        [[ -n "$vcs_info_msg_0_" ]] && psvar[1]=( "${vcs_info_msg_0_}" )
        [[ -n "$vcs_info_msg_1_" ]] && psvar[2]=( "${vcs_info_msg_1_}" )
        [[ -n "$vcs_info_msg_2_" ]] && psvar[3]=( "${vcs_info_msg_2_}" )
    fi
}
add-zsh-hook precmd _update_vcs_info_msg

# # ----------------------------
# # Prompt
# # ----------------------------

autoload -U colors; colors

case ${UID} in
    0)
        # root
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    # unfunction precmd
    # unfunction preexec        unsetopt zle
    tmp_prompt="%m:%~> "
    tmp_prompt2=""
    tmp_rprompt=""
    tmp_sprompt=""

    ;;
    *)
    # user
    vcs_prompt="%F{red}%3v%f%F{yellow}%2v%f%F{green}%1v%f "
    tmp_prompt="%{${fg[magenta]}%}%n@%m${WINDOW:+"[$WINDOW]"} %B%(?,%{${fg[green]}%},%{${fg[red]}%})%(!,#,>)%b %{${reset_color}%}"
    tmp_prompt2="%{${fg[magenta]}%}%_> %{${reset_color}%}"
    tmp_rprompt="%{${fg[green]}%}[%~] %{${fg[yellow]}%}[%W %T]%{${reset_color}%}"
    tmp_sprompt="%{${fg[yellow]}%}%r is correct? [Yes, No, Abort, Edit]:%{${reset_color}%}"
    # # ssh
    # [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    # tmp_prompt="%{${fg[red]}%}%n@%m${WINDOW:+"[$WINDOW]"} %B%(?,%{${fg[green]}%},%{${fg[red]}%})%(!,#,>)%b %{${reset_color}%}"
    # ;
    ;;
esac

PROMPT=$tmp_prompt               # 通常のプロンプト
PROMPT2=$tmp_prompt2             # セカンダリのプロンプト(コマンドが2行以上の時に表示される)
RPROMPT=$vcs_prompt$tmp_rprompt  # 右側のプロンプト
SPROMPT=$tmp_sprompt             # スペル訂正用プロンプト


# Title (user@hostname) #
case "${TERM}" in
kterm*|xterm*|)
  precmd() {
    echo -ne "\033]0;${USER}@${HOST%%.*}\007"
  }
  ;;
esac


# # -----------------------------
# # Emacs Tramp 
# # -----------------------------

case "$TERM" in
    dumb)
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
        PS1='$ '
        ;;
esac
