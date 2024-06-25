import type { Tokens } from "marked";
import * as marked from "marked";

export type CodeBlock = {
  name: string;
  lang: string | undefined;
  code: string;
  original: string;
};

const langToExtension: { [key: string]: string } = {
  typescript: "ts",
  javascript: "js",
  html: "html",
  css: "css",
  shell: "sh",
  bash: "sh",
  plaintext: "txt",
  json: "json",
  yaml: "yaml",
  yml: "yaml",
  markdown: "md",
  md: "md",
  rust: "rs",
  haskell: "hs",
  python: "py",
  go: "go",
  java: "java",
  kotlin: "kt",
  swift: "swift",
  c: "c",
  cpp: "cpp",
  csharp: "cs",
  php: "php",
  ruby: "rb",
  perl: "pl",
  r: "r",
  sql: "sql",
  graphql: "graphql",
  dockerfile: "dockerfile",
  docker: "dockerfile",
};

/**
 * Extracts code blocks from markdown source.
 *
 */
export const codeblocks = (src: string, lang: string): CodeBlock[] => {
  var renderer = new marked.Renderer();
  var renderers = Object.getOwnPropertyNames(marked.Renderer.prototype);

  // Remove all other renderers.
  for (var i = 0; i < renderers.length; i++) {
    var f = renderers[i];
    if (f !== "constructor") {
      const renderName = renderers[i];
      (renderer as any)[renderName] = () => {
        return "";
      };
    }
  }

  let currentHeading: string | undefined;
  renderer.heading = (token: Tokens.Heading) => {
    currentHeading = token.text;
    return "";
  };

  let codeblocks: CodeBlock[] = [];
  let usedFilenames: string[] = [];
  renderer.code = (token: Tokens.Code): string => {
    const [blockLanguage, ...langAttributes] = (token.lang || "")?.split(" ");

    // Check if we can convert the language to a file extension.
    let fileExtension = blockLanguage;
    if (blockLanguage in langToExtension) {
      fileExtension = langToExtension[blockLanguage];
    }

    // Construct a unique name for the codeblock.
    let name = `${codeblocks.length + 1}.${fileExtension}`;
    // Make command line blocks a bit more descriptive.
    if (
      blockLanguage == "bash" ||
      blockLanguage == "shell" ||
      blockLanguage == "zsh"
    ) {
      name = `terminal (${codeblocks.length + 1}).${fileExtension}`;
    }
    // Look up the name attribute.
    for (const langAttribute of langAttributes) {
      if (langAttribute.startsWith("name=")) {
        const findSlashes = new RegExp("/", "g");
        const sanitizedName = langAttribute
          // Remove the attribute marker.
          .replace("name=", "")
          // Gists can't have slashes (/) in their name so we replace all slashes with backslashes.
          .replace(findSlashes, "\\")
          .trim();
        const alternativeName =
          `${sanitizedName} (${currentHeading}).${fileExtension}`.replace(
            findSlashes,
            "\\"
          );
        // If the name hasn't been used, we try to just give it that name. This simplifies
        // moving codeblocks around without needing to update the Gist URLs.
        if (!usedFilenames.includes(sanitizedName)) {
          name = `${sanitizedName}`;
        } else if (!usedFilenames.includes(alternativeName)) {
          name = `${alternativeName}`;
        } else {
          name = `${sanitizedName} (${codeblocks.length + 1}).${fileExtension}`;
        }
        usedFilenames.push(name);
        break;
      }
    }

    codeblocks.push({
      name,
      lang: blockLanguage,
      code: token.text,
      original: token.raw,
    });
    return "";
  };

  var output = marked.parse(src, { renderer, gfm: true }) as string;
  output = output.replace(/\n+$/g, "");

  return codeblocks;
};
