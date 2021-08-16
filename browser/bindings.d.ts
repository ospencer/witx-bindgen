export function addBrowserToImports(imports: any, obj: Browser, get_export: (string) => WebAssembly.ExportValue): void;
export interface Browser {
  log(msg: string): void;
  error(msg: string): void;
}
