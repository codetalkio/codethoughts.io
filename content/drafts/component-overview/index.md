+++
title = "Component overview"
date = 2023-11-29
+++

Generated Table of Contents by inserting a `{{/* toc() */}}` shortcode with a depth of 3:

{{ toc() }}

This shortcode actually just inserts `<!-- toc -->` which gets replaced by running the `preprocess_post(resource=page)` macro on it, which calls `generate_toc(toc, level, depth)` under-the-hood.

## Summary

Mark the summary cutoff point by placing `<div></div><!-- more -->` at the desired location.

<div></div><!-- more -->

## Links

Internal links get underlined with orange: [Such as this link](/).

External links get underlined with blue: [Such as this link](https://www.google.com).

## Lists

Unordered list:

- First level item no. 1
- First level item no. 2
  - Second level item no. 1
  - Second level item no. 2
    - Third level item no. 1
    - Third level item no. 2
      - Fourth level item no. 1
      - Fourth level item no. 2
  - Second level item no. 3
- First level item no. 3

Ordered list:

1. First level item no. 1
2. First level item no. 2
   1. Second level item no. 1
   2. Second level item no. 2
      1. Third level item no. 1
      2. Third level item no. 2
         1. Fourth level item no. 1
         2. Fourth level item no. 2
  3. Second level item no. 3
3. First level item no. 3

## Footnotes

Here's a simple footnote,[^1] and here's a longer one.[^bignote]

[^1]: This is the first footnote.

[^bignote]: Multiline footnotes are *not* supported ([#1282](https://github.com/getzola/zola/issues/1282#issuecomment-753445777)), and will end up wrapping everything in a code block outside the footnote `<div>`.

    Indent paragraphs to include them in the footnote.

    `{ my code }`

    Add as many paragraphs as you like.

## Code blocks

Code blocks support the following annotations:

- `linenos`: Display line numbers column
- `hl_lines`: Highlight specific lines, e.g. `hl_lines=1 3-5 9`
- `linenostart`: Start line number, e.g. `linenostart=20`
- `hide_lines`: Hide specific lines, e.g. `hide_lines=1-2`

It's placed after the language, which will look like this:

````
```rust,linenos,hl_lines=1 3-5 9
println!("Hello, world!");
```
````

### Examples

Plain, without line numbers:

```typescript
// @ts-check

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  trailingSlash: true,
  experimental: {
    // Statically type links to prevent typos and other errors when using next/link, improving type safety when navigating between pages.
    typedRoutes: true,
  },
};

module.exports = nextConfig;
```

With line numbers:

```typescript,linenos
// @ts-check

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  trailingSlash: true,
  experimental: {
    // Statically type links to prevent typos and other errors when using next/link, improving type safety when navigating between pages.
    typedRoutes: true,
  },
};

module.exports = nextConfig;
```

With line numbers and highlighting:

```typescript,linenos,hl_lines=1 3-5 9
// @ts-check

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  trailingSlash: true,
  experimental: {
    // Statically type links to prevent typos and other errors when using next/link, improving type safety when navigating between pages.
    typedRoutes: true,
  },
};

module.exports = nextConfig;
```

Without line numbers but with highlighting:

```typescript,hl_lines=1 3-5 9 13
// @ts-check

/** @type {import('next').NextConfig} */
const nextConfig = {
  output: "export",
  trailingSlash: true,
  experimental: {
    // Statically type links to prevent typos and other errors when using next/link, improving type safety when navigating between pages.
    typedRoutes: true,
  },
};

module.exports = nextConfig;
```

## Code from files

Loading code from a colocated file, and hiding specific lines:

```typescript,hide_lines=1-5 10-15
{{ load_code(path="example.ts") }}
```

## Inline code

We're using [leptos_i18n](https://github.com/Baptistemontan/leptos_i18n) for localization in Leptos, which supports an API that's very close to the one we used in Next.js. We already pulled in the dependency when we updated our `Cargo.toml` file earlier, so let's get the rest of it set up.

We'll create a `ui-internal/messages/` folder where our locales will live....

## Images

Pulling in a colocated image:

{{ image(path="logo.png", alt="codethoughts logo", width=200) }}

Pulling in a colocated image with caption:

{{ image(path="logo.png", caption="codethoughts", width=200) }}

Showing multiple images:

{{ images(paths=["logo.png", "logo.png", "logo.png"], captions=["codethoughts logo 1", "logo 2"], widths=[100, 200, 100], defaultAlt="The default alt", defaultWidth=200) }}

For quick copying, use this one:

```
{{ images(paths=["logo.png", "logo.png"], captions=["logo1", "logo2"], widths=[400, 400]) }}
```

## Tables

A simple table with a header, defaulting to centered alignment:

|          | Header 2 | Header 3 |
|----------|----------|----------|
| Row 1    | Row 1    | Row 1    |
| Row 2    | Row 2    | Row 2    |
| Row 3    | Row 3    | Row 3    |

A table with different text alignment:

| Longer header 1 | Longer header 2 | Longer header 3 |
| :--- | :--: | ---: |
| Row 1    | Row 1    | Row 1    |
| Row 2    | Row 2    | Row 2    |

A left-aligned table (uses `<left>...</left>` HTML tags):

<left>

| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| Row 1    | Row 1    | Row 1    |
| Row 2    | Row 2    | Row 2    |

</left>

A right-aligned table (uses `<right>...</right>` HTML tags):

<right>

| Header 1 | Header 2 | Header 3 |
|----------|----------|----------|
| Row 1    | Row 1    | Row 1    |
| Row 2    | Row 2    | Row 2    |

</right>


A simple table without header (empty headers hidden with CSS):

||||
|----------|----------|----------|
| Row 1    | Row 1    | Row 1    |
| Row 2    | Row 2    | Row 2    |
| Row 3    | Row 3    | Row 3    |

This is a clever little trick of convoluted logic (lots of negation!) which checks:

- If there's a `thead` element
- That do **not** have `th` elements
- That are **not** empty
- Then hide it

```css
thead:not(thead:has(th:not(:empty))) {
  display: none;
}
```

## Quotes

A single line quote:

> This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet.

And a multi-line quote:

> This is a blockquote with two paragraphs. Lorem ipsum dolor sit amet.
>
> And it can span multiple lines...

## Callouts

A callout:

{% aside() %}
  This is meant to be a tip or `call` out some [additonal info](#).
{% end %}

## Headings

# H1 (should not be used in posts)

## H2 headings get an underline

### H3 is slightly smaller

#### H4 smaller still

##### H5 smallest heading size

###### H6 has the same size as H5
