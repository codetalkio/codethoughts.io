/** @type {import('tailwindcss').Config} */

const brand = {
  50: "#FFF7EB",
  100: "#FFE8C6",
  200: "#FFCF88",
  300: "#FFAB40", // Primary
  400: "#FF9320",
  500: "#F96D07",
  600: "#DD4A02",
  700: "#B72F06",
  800: "#94230C",
  900: "#7A1F0D",
  950: "#460C02",
};

const black = {
  50: "#f7f7f8",
  100: "#efeef0",
  200: "#dbd9de",
  300: "#bcb8c1",
  400: "#96919f",
  500: "#797384",
  600: "#635d6c",
  700: "#514c58",
  800: "#46414b",
  900: "#3a373e", // Primary
  950: "#28262b",
};

const yellow = {
  50: "#ffffe5",
  100: "#fdffc7",
  200: "#f9ff95",
  300: "#eeff41", // Primary
  400: "#e1f625",
  500: "#c3dd05",
  600: "#98b100",
  700: "#728605",
  800: "#5a690b",
  900: "#4b590e",
  950: "#283201",
};

const blue = {
  50: "#edf1ff",
  100: "#dfe4ff",
  200: "#c5cdff",
  300: "#a2acff",
  400: "#7d80fc",
  500: "#625bf6", // Primary
  600: "#5540eb",
  700: "#4833cf",
  800: "#3b2ca7",
  900: "#342b84",
  950: "#1f194d",
};

const red = {
  50: "#fef2f3",
  100: "#fce7e7",
  200: "#f9d2d5",
  300: "#f5acb2",
  400: "#ee7e8a",
  500: "#e35064",
  600: "#d0324f", // Primary
  700: "#ae223f",
  800: "#921f3b",
  900: "#7d1e38",
  950: "#450c1a",
};

const purple = {
  50: "#faf6fe",
  100: "#f2ebfc",
  200: "#e7dafa",
  300: "#d5bdf5",
  400: "#bc93ed",
  500: "#a16ae2",
  600: "#8b4ad3",
  700: "#7638b8",
  800: "#663399", // Primary
  900: "#522a79",
  950: "#371358",
};

module.exports = {
  content: {
    files: ["./templates/**/*.html", "./theme/**/*.html"],
  },
  darkMode: "selector",
  theme: {
    // Normal: -apple-system,BlinkMacSystemFont,"Segoe UI","Noto Sans",Helvetica,Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji"
    // Title: -apple-system, BlinkMacSystemFont, "Segoe UI", "Noto Sans", Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji"
    // Code: ui-monospace,SFMono-Regular,SF Mono,Menlo,Consolas,Liberation Mono,monospace
    fontFamily: {
      sans: [
        "-apple-system",
        "BlinkMacSystemFont",
        "segoe ui",
        "noto sans",
        "Helvetica",
        "Arial",
        "sans-serif",
        "apple color emoji",
        "segoe ui emoji",
      ],
      code: [
        "ui-monospace",
        "SFMono-Regular",
        "SF Mono",
        "Menlo",
        "Consolas",
        "Liberation Mono",
        "monospace",
      ],
    },
    colors: {
      transparent: "transparent",
      current: "currentColor",
      white: black["50"],
    },
    extend: {
      colors: {
        background: "var(--background)",
        backgroundCode: "var(--background-code)",
        backgroundInlineCode: "var(--background-inline-code)",
        subtle: "var(--text-subtle)",
        headline: "var(--text-headline)",
        primary: "var(--text-primary)",
        link: {
          DEFAULT: "var(--link-color)",
          hover: "var(--link-color-hover)",
        },
        dark: {
          background: "#0E1116",
          // GitHub style background for code blocks (original #171B22).
          backgroundCode: "#171B22",
          backgroundInlineCode: "#6e768166",
          text: black["50"],
          headline: brand["300"],
          subtleText: black["400"],
          link: {
            DEFAULT: blue["300"],
            hover: blue["200"],
            visited: blue["500"],
          },
        },
        light: {
          background: black["50"],
          // GitHub style background for code blocks (original #F6F8FA).
          backgroundCode: black["100"],
          backgroundInlineCode: "#afb8c133",
          text: black["900"],
          headline: brand["400"],
          subtleText: black["400"],
          link: {
            DEFAULT: blue["700"],
            hover: blue["700"],
            visited: blue["700"],
          },
        },
        brand, // Orange is the brand color.
        black,
        yellow,
        blue,
        red,
        purple,
      },
      keyframes: {
        fadeIn: {
          "0%": { transform: "opacity: 0" },
          "100%": { transform: "opacity: 1" },
        },
      },
    },
  },
  plugins: [],
};
