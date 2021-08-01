
declare namespace archetype {
  interface Compiler {
    compile(input: string, options?: any): string
  }
}

declare module archetype {
  const archetypeModule: archetype.Compiler;
  export = archetypeModule;
}
