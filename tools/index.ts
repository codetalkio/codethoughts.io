/**
 * Run with: GITHUB_TOKEN=<token> bun run index.ts
 *
 */
import { $, Glob } from "bun";
import { Octokit } from "octokit";

import type { CodeBlock } from "./codeblock";
import { codeblocks } from "./codeblock";

const { GITHUB_TOKEN } = process.env;
const RELATIVE_DIRECTORY = "../legacy/posts";

type GistFiles = {
  [key: string]: {
    filename?: string;
    content: string;
  };
};

type GistMetadata = {
  id: string;
  url: string;
  embeds: string[];
};

// Create a personal access token at https://github.com/settings/tokens/new?scopes=repo
const octokit = new Octokit({ auth: GITHUB_TOKEN });

/**
 * Create a new Gist with the content of a file.
 *
 * https://docs.github.com/en/rest/gists/gists?apiVersion=2022-11-28
 */
const createGist = async (filename: string, codeBlocks: CodeBlock[]) => {
  // Convert our codeblocks to files.
  const files: GistFiles = {};
  let i = 0;
  for (const block of codeBlocks) {
    i = i + 1;
    files[block.name] = { content: block.code };
  }

  const directory = "gists";
  const filepath = `${directory}/${filename}`;
  // Create a directory to store Gist metadata in.
  await $`mkdir -p ${directory}`;
  // Look up if we already have create a Gist for this file.
  const file = Bun.file(filepath);
  const gistExists = await file.exists();

  const description = `Gists for the ${filename} post on codethoughts.io`;
  // Prepend the filename as an empty file to the Gist to make it easier to reference.
  files[`....-${filename}`] = {
    content: description,
  };

  if (gistExists) {
    // Try to update the existing file.
    const gistMetadata = await file.json();
    console.log(`Updating gist ${gistMetadata.id} (${gistMetadata.url})`);

    // Check if we need to remove any old files and set their filename to an
    // empty object.
    const existingGist = await octokit.rest.gists.get({
      gist_id: gistMetadata.id,
    });
    const oldFiles = Object.keys(existingGist.data.files ?? {});
    oldFiles.forEach((oldFilename) => {
      if (!(oldFilename in files)) {
        files[oldFilename] = {} as any;
      }
    });

    const response = await octokit.rest.gists.update({
      gist_id: gistMetadata.id,
      description,
      files,
      public: false,
    });
    const gistId = response.data.id;
    const gistUrl = response.data.html_url;
    const embeds = Object.keys(files).map(
      (filename) => `${gistUrl}.js?file=${filename}`
    );
    const updatedGistMetadata = { id: gistId, url: gistUrl, embeds };
    await Bun.write(filepath, JSON.stringify(updatedGistMetadata));
    return updatedGistMetadata;
  } else {
    // Otherwise, create a new Gist.
    const response = await octokit.rest.gists.create({
      description,
      files,
      public: false,
    });
    const gistId = response.data.id;
    const gistUrl = response.data.html_url;
    const embeds = Object.keys(files).map(
      (filename) => `${gistUrl}.js?file=${filename}`
    );
    const gistMetadata = { id: gistId, url: gistUrl, embeds };
    await Bun.write(filepath, JSON.stringify(gistMetadata));
    return gistMetadata;
  }
};

/**
 * Extract code blocks from a Markdown file and return a list of them.
 */
const extractCodeBlocks = async (filename: string): Promise<CodeBlock[]> => {
  const filepath = `${RELATIVE_DIRECTORY}/${filename}`;
  const file = Bun.file(filepath);
  const contents = await file.text();
  const codeBlocks = codeblocks(contents, "*");
  return codeBlocks;
};

/**
 * List all Markdown files in the directory.
 *
 * NOTE: We filter out files that are older than 2023-10-06.
 */
const listFiles = (): string[] => {
  const glob = new Glob("*");
  const files = [...glob.scanSync(RELATIVE_DIRECTORY)]
    .sort()
    .filter((file) => file.endsWith(".md") && file > "2023-10-05");
  return files;
};

/**
 * Generate a post that embeds the Gist instead of the codeblocks.
 *
 * This is handy for posts that need to be imported into Medium or Substack.
 */
const generateGistPost = async (
  filename: string,
  metadata: GistMetadata,
  codeBlocks: CodeBlock[]
) => {
  // Create a directory to store converted Posts in.
  const directory = "gist-posts";
  await $`mkdir -p ${directory}`;

  // Read the original post.
  const file = Bun.file(`${RELATIVE_DIRECTORY}/${filename}`);
  const originalContent = await file.text();

  // Replace the codeblocks with the Gist embeds.
  let newContent = originalContent;
  for (const block of codeBlocks) {
    // NOTE: It's important to not use a global replace, but instead replace the first occurence.
    // This is because we might have multiple codeblocks with the same content, so by going through them
    // in order of the parsed codeblocks, we ensure each link to its corresponding Gist.
    const replace = `<script src="${metadata.url}.js?file=${block.name}"></script>`;
    newContent = newContent.replace(block.original, replace);
  }

  const convertedPostFilepath = `${directory}/${filename}`;
  await Bun.write(convertedPostFilepath, newContent);
};

const main = async () => {
  const files = listFiles();
  for (const file of files) {
    const codeBlocks = await extractCodeBlocks(file);
    const gistMetadata = await createGist(file, codeBlocks);
    console.log(gistMetadata);
    await generateGistPost(file, gistMetadata, codeBlocks);
  }
};

await main();
