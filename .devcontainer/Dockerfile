# Use the [Choice] comment to indicate option arguments that should appear in VS Code UX
#
# [Choice] Haskell version: 9, 8
ARG VARIANT=9
FROM haskell:${VARIANT}

# Use the [Option] comment to specify true/false arguments that should appear in VS Code UX
#
# [Option] Install zsh
ARG INSTALL_ZSH="true"
# [Option] Upgrade OS packages to their latest versions
ARG UPGRADE_PACKAGES="false"

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID
COPY library-scripts/*.sh /tmp/library-scripts/
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && /bin/bash /tmp/library-scripts/common-debian.sh "${INSTALL_ZSH}" "${USERNAME}" "${USER_UID}" "${USER_GID}" "${UPGRADE_PACKAGES}" "true" "true"\
    && apt-get autoremove -y && apt-get clean -y && rm -rf /var/lib/apt/lists/* /tmp/library-scripts

# Setup ghcup, rbenv, and ruby dependencies.
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh
# && echo '[ -f "/home/vscode/.ghcup/env" ] && source "/home/vscode/.ghcup/env" # ghcup-env' >> /home/vscode/.zshrc \
# && git clone https://github.com/rbenv/rbenv.git /home/vscode/.rbenv \
# && cd /home/vscode/.rbenv && src/configure && make -C src \
# && echo '[ -f "/home/vscode/.rbenv/bin/rbenv" ] && eval "$(rbenv init - bash)" # rbenv' >> /home/vscode/.zshrc \
# && echo 'export PATH="/home/vscode/.rbenv/bin:$PATH"' >> /home/vscode/.zshrc \
# && echo 'export PATH="/home/vscode/.rbenv/bin:$PATH"' >> /home/vscode/.bashrc \
# && mkdir -p /home/vscode/.rbenv/plugins \
# && git clone https://github.com/rbenv/ruby-build.git /home/vscode/.rbenv/plugins/ruby-build
