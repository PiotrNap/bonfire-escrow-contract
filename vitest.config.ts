import { defineConfig } from "vitest/config"

export default defineConfig({
  test: {
    include: ["./tests/helios/*.{test,spec}.?(c|m)[jt]s?(x)"],
    exclude: ["./dist-newstyle/**"],
  },
})
