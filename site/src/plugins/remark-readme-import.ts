import { fromMarkdown } from "mdast-util-from-markdown";

interface PluginOptions {
  githubUrl: string;
}

/**
 * Remark plugin that replaces the content of getting-started.md with filtered README content
 */
export function remarkReadmeImport(options: PluginOptions) {
  const githubUrl = options.githubUrl;

  return function transformer(tree: any, file: any) {
    // Only process the README.md file
    if (!file.path || !file.path.includes("README.md")) {
      return;
    }

    try {
      const readmeContent = file.value;
      const filteredContent = filterReadmeContent(readmeContent);
      const processedContent = processLinksForGithub(
        filteredContent,
        githubUrl,
      );

      // Parse the processed markdown back into AST nodes
      const readmeAst = fromMarkdown(processedContent);

      // Replace all content after the frontmatter with README content
      // Keep the root node but replace its children
      tree.children = readmeAst.children;
    } catch (error) {
      console.error("Error processing README import:", error);

      // Replace with error message
      tree.children = [
        {
          type: "paragraph",
          children: [
            {
              type: "text",
              value: `Error: Could not import README content - ${error}`,
            },
          ],
        },
      ];
    }
  };
}

/**
 * Filters README content by removing sections marked with GUIDE_EXCLUDE comments
 */
function filterReadmeContent(content: string): string {
  const lines = content.split("\n");
  const filteredLines: string[] = [];
  let isExcluding = false;

  for (const line of lines) {
    const trimmedLine = line.trim();

    if (trimmedLine === "<!-- GUIDE_EXCLUDE_START -->") {
      isExcluding = true;
      continue;
    }

    if (trimmedLine === "<!-- GUIDE_EXCLUDE_END -->") {
      isExcluding = false;
      continue;
    }

    if (!isExcluding) {
      filteredLines.push(line);
    }
  }

  return filteredLines.join("\n").trim();
}

/**
 * Processes relative links to point to GitHub repository
 */
function processLinksForGithub(content: string, githubUrl: string): string {
  let processed = content;

  // Convert relative links with ./ to absolute GitHub links
  processed = processed.replace(
    /\[([^\]]+)\]\(\.\/([^)]+)\)/g,
    `[$1](${githubUrl}/blob/main/$2)`,
  );

  // Convert relative links without ./ prefix to GitHub links (but not already processed ones)
  processed = processed.replace(
    /\[([^\]]+)\]\((?!https?:\/\/)([^)]+\.(toml|md|rs|txt|json|yaml|yml))\)/g,
    `[$1](${githubUrl}/blob/main/$2)`,
  );

  return processed;
}

export default remarkReadmeImport;
