// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import catppuccin from "@catppuccin/starlight";
import { remarkReadmeImport } from "./src/plugins/remark-readme-import.ts";

// https://astro.build/config
export default defineConfig({
  site: "https://glidewm.org",
  integrations: [
    starlight({
      title: "glide",
      logo: {
        light: "./src/assets/parachute.svg",
        dark: "./src/assets/parachute-dark.svg",
        alt: "glide logo",
      },
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/glide-wm/glide",
        },
      ],
      editLink: {
        baseUrl: "https://github.com/glide-wm/glide/edit/main/site/",
      },
      plugins: [
        catppuccin({
          dark: { accent: "lavender" },
          light: { accent: "blue" },
        }),
      ],
      customCss: ["./src/style.css"],
      sidebar: [
        {
          label: "Guides",
          items: [
            // Each item here is one entry in the navigation menu.
            { label: "Getting Started", slug: "guides/getting-started" },
          ],
        },
        {
          label: "Reference",
          autogenerate: { directory: "reference" },
        },
      ],
    }),
  ],
  markdown: {
    remarkPlugins: [
      [remarkReadmeImport, { githubUrl: "https://github.com/glide-wm/glide" }],
    ],
  },
});
