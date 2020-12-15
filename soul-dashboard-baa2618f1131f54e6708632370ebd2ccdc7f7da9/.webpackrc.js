const path = require("path");

export default {
  es5ImcompatibleVersions: true,
  extraBabelIncludes: [
    "node_modules",
  ],
  entry: "./src/index.js",
  extraBabelPlugins: [
    ["import", {libraryName: "antd", libraryDirectory: "es", style: true}]
  ],
  env: {
    development: {
      extraBabelPlugins: ["dva-hmr"]
    }
  },
  alias: {
    components: path.resolve(__dirname, "src/components/")
  },
  ignoreMomentLocale: true,
  theme: "./src/theme.js",
  html: {
    template: "./src/index.ejs"
  },
  lessLoaderOptions: {
    javascriptEnabled: true
  },
  disableDynamicImport: true,
  publicPath: "/",
  hash: true,
  proxy: {
    "/": {
      target: "http://localhost:9095",
      changeOrigin: true,
      pathRewrite: {"^/": ""}
    }
  }
};
