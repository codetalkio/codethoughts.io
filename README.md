# codethoughts

This is the code for the blog [codethoughts.io](https://codethoughts.io), where Christian Kjær (Tehnix) dumps his thoughts every now and then.

```bash
$ just
Available recipes:
    build           # Build blog artifacts and static files.
    build-js        # Bundle and minify standalone JavaScript files.
    code            # Open project workspace in VS Code.
    dev             # Run zola development server in watch mode.
    generate-gists  # Generate posts with Gists instead of code blocks.
    help            # Display help information.
    install-tooling # Install tooling for working with the codethoughts blog.
```

The normal workflow would be:
- `just code` to open the project in VS Code/Cursor/whichever editor you've configured to open `.code-workspace` files.
- `just dev` to run the `zola` development server, and `tailwindcss` in parallel.
- Write you draft posts in `content/drafts/` and move them to `content/posts/` when you're ready to publish.

# Stack

- [`zola`](https://www.getzola.org/documentation/getting-started/overview/) for static site generation.
- [`tailwindcss`](https://tailwindcss.com/) for styling (defined in `styles/input.css` and compiled to `static/styles/output.css`).
- [`bun`](https://bun.sh/) for package management and any javascript/typescript code.
- [`just`](https://just.systems/) for running commands.

And then we pull in:
- [`mermaid`](https://mermaid.js.org/) for diagrams.

Static files are served from `static/`.

# Syntax and Shortcodes

There's a few custom [shortcodes](https://www.getzola.org/documentation/content/shortcodes/) that are used to add extra functionality to the blog, and make certain things a bit easier. They are all defined in `templates/shortcodes/`.

You can see most of these in action in the [component-overview](drafts/component-overview/index.md) draft post, or live at [https://codethoughts.io/drafts/component-overview/](https://codethoughts.io/drafts/component-overview/).

### Excerpts

To mark the end of the part of a post that is shown as a preview, you can use the `<div></div><!-- more -->` tag.

```markdown
<div></div><!-- more -->
```

NOTE: Without this tag, there will be no preview/summary generated for the post.

This will be the part of the post that is shown previews, such as on [the home page](https://codethoughts.io/), accessible as the `page.summary` (used in the [index.html](templates/index.html) template).


### Table of Contents

Generates a table of contents for the current page. It's placed after the summary cutoff point, and will generate a table of contents for the current page.

Simply just place it where you want the table of contents to be generated, like this:

```markdown
{{ toc() }}
```

### Aside/Callouts 💡

Adds an aside block to the current page. It's placed after the summary cutoff point, and will add an aside block to the current page.

```markdown
{% aside() %}
  I will show up as a callout block on the page with a nice lightbulb icon (💡) in front of it.
{% end %}
```

### Images

Colocated images (placed in the same folder as the `index.md` of the post) can be loaded like this:


```markdown
{{ image(path="Screenshot_2024-07-08_at_16.18.43.png", caption="The Redirect Rules entry for catching incoming requests", width=600) }}
```

This automatically resizes the image to the width specified keeping the aspect ratio, and adds a caption if provided.


### Mermaid Diagrams

Mermaid diagrams are conditionally loaded, based on the `mermaidjs` extra field in the post frontmatter. If it is set to `true`, the diagram will be loaded, otherwise it will be ignored.

```markdown
+++
...
[extra]
mermaidjs = "true"
+++
```

You can then make diagrams using `<pre class="mermaid">` tags, like this:

```markdown
<pre class="mermaid">
graph TD
  A --> B
  %% ...etc

</pre>
```

### Code from files

To load code from a colocated file, you can use the `load_code` shortcode.

````markdown
```typescript,hide_lines=1-5 10-15
{{ load_code(path="example.ts") }}
```
````

This will load the code from the file `example.ts` and display it in a code block.

### Medium link

To standardize linking to Medium comments, you can use the `medium_comments` shortcode, usually at the end of the post:

```markdown
{{ medium_comments(post="improved-turso-libsql-ergonomics-in-rust-b2c0bf134d46") }}
```

Which will generate something like this, linking to the Medium post:

```
👉 Let me know what you think over on Medium or in the comments below 👇
```

This is defined in `templates/shortcodes/medium_comments.html`.

### Snapshot style sections ⌜ ｣

To make a snapshot style section, like on [https://codethoughts.io/pages/about/](https://codethoughts.io/pages/about/), you can use the `snapshot_section_wrapper` shortcode together with the `icon_section` shortcode.

```
{{ snapshot_section_wrapper(part="start") }}

{% icon_section(name="pencil-square", title="About the blog") %}
  Codethoughts is a place where I ...
{% end %}

{{ snapshot_section_wrapper(part="end") }}
```

The available svg icons are defined in `templates/macros/icon.html`.
