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
