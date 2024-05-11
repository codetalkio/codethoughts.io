/** @type {import('tailwindcss').Config} */
module.exports = {
  content: {
    files: ["./templates/**/*.html", "./theme/**/*.html"],
  },
  darkMode: "selector",
  theme: {
    colors: {
      transparent: "transparent",
      current: "currentColor",
      white: "#F7F7F8",
    },
    extend: {
      colors: {
        transparent: "transparent",
        current: "currentColor",
        white: "#F7F7F8",
        brand: {
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
        },
        dark: {
          background: "#0E1116",
          text: "#F7F7F8",
        },
        light: {
          background: "#F7F7F8",
          text: "#3A373E",
        },
      },
    },
  },
  plugins: [],
};
