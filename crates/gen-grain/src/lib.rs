use heck::*;
use std::mem;
use wit_bindgen_gen_core::wit_parser::abi::{
    AbiVariant, Bindgen, Bitcast, Instruction, LiftLower, WasmType,
};
use wit_bindgen_gen_core::{wit_parser::*, TypeInfo, Types};
use wit_bindgen_gen_core::{Direction, Files, Generator, Source};

#[derive(Default)]
pub struct Grain {
    src: Source,
    opts: Opts,
    in_import: bool,
    sizes: SizeAlign,
    types: Types,
    return_pointer_area_size: usize,
}

#[derive(Default, Debug, Clone)]
#[cfg_attr(feature = "structopt", derive(structopt::StructOpt))]
pub struct Opts {
    // ...
}

impl Opts {
    pub fn build(&self) -> Grain {
        let mut r = Grain::new();
        r.opts = self.clone();
        r
    }
}

impl Grain {
    pub fn new() -> Grain {
        Grain::default()
    }
}

impl GrainGenerator for Grain {
    fn push_str(&mut self, s: &str) {
        self.src.push_str(s);
    }

    fn info(&self, ty: TypeId) -> TypeInfo {
        self.types.get(ty)
    }
}

impl Generator for Grain {
    fn preprocess_one(&mut self, iface: &Interface, _dir: Direction) {
        self.sizes.fill(iface);
    }

    fn type_flags(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        name: &str,
        flags: &Flags,
        _docs: &Docs,
    ) {
        let typename = name.to_camel_case();
        let ty = int_repr(flags_repr(flags));
        self.src
            .push_str(&format!("export type {typename} = {ty}\n\n"));
        let repr = flags_repr(flags);
        for (i, field) in flags.flags.iter().enumerate() {
            self.src.push_str(&format!(
                "export let _{}_{}: {typename} = {}.shl(1{}, {}{})\n",
                name.to_shouty_snake_case(),
                field.name.to_shouty_snake_case(),
                int_repr(repr),
                int_suffix(repr),
                i,
                int_suffix(repr),
            ));
        }
        self.src.push_str("\n");
    }

    fn type_tuple(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        _name: &str,
        _tuple: &Tuple,
        _docs: &Docs,
    ) {
        // No type needed
    }

    fn type_option(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        _name: &str,
        _option: &Type,
        _docs: &Docs,
    ) {
        // No type needed
    }

    fn type_expected(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        _name: &str,
        _expected: &Expected,
        _docs: &Docs,
    ) {
        // No type needed
    }

    fn type_record(
        &mut self,
        iface: &Interface,
        _id: TypeId,
        name: &str,
        record: &Record,
        _docs: &Docs,
    ) {
        if record.fields.len() == 0 {
            // Empty records don't exist in Grain
            return;
        }

        self.src
            .push_str(&format!("export record {r} {{", r = name.to_camel_case()));
        for (_i, field) in record.fields.iter().enumerate() {
            self.src
                .push_str(&format!("\n{f}: ", f = to_grain_ident(&field.name)));
            self.print_ty(iface, &field.ty);
            self.src.push_str(",");
        }
        self.src.push_str("\n}")
    }

    fn type_variant(
        &mut self,
        iface: &Interface,
        _id: TypeId,
        name: &str,
        variant: &Variant,
        _docs: &Docs,
    ) {
        self.src
            .push_str(&format!("export enum {r} {{", r = name.to_camel_case()));
        for case in variant.cases.iter() {
            self.src.push_str("\n");
            self.src.push_str(&case_name(name));
            self.src.push_str(&case_name(&case.name));
            if !is_empty_type(iface, &case.ty) {
                self.src.push_str("(");
                self.print_ty(iface, &case.ty);
                self.src.push_str(")")
            }
            self.src.push_str(",");
        }
        self.src.push_str("\n}");
    }

    fn type_enum(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        name: &str,
        enum_: &Enum,
        _docs: &Docs,
    ) {
        self.src
            .push_str(&format!("export enum {r} {{", r = name.to_camel_case()));
        for case in enum_.cases.iter() {
            self.src.push_str("\n");
            self.src.push_str(&case_name(name));
            self.src.push_str(&case_name(&case.name));
            self.src.push_str(",");
        }
        self.src.push_str("\n}");
    }

    fn type_union(
        &mut self,
        iface: &Interface,
        _id: TypeId,
        name: &str,
        union_: &Union,
        _docs: &Docs,
    ) {
        self.src
            .push_str(&format!("export enum {r} {{", r = name.to_camel_case()));
        for (i, case) in union_.cases.iter().enumerate() {
            self.src.push_str("\n");
            self.src.push_str(&case_name(name));
            self.src.push_str(&i.to_string());
            self.src.push_str("(");
            self.print_ty(iface, &case.ty);
            self.src.push_str(")");
            self.src.push_str(",");
        }
        self.src.push_str("\n}");
    }

    fn type_resource(&mut self, iface: &Interface, ty: ResourceId) {
        let name = &iface.resources[ty].name;
        let typename = name.to_camel_case();
        self.src.push_str(&format!("type {typename} = Int32\n"));
    }

    fn type_alias(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        _name: &str,
        _ty: &Type,
        _docs: &Docs,
    ) {
        unimplemented!()
    }

    fn type_list(&mut self, iface: &Interface, id: TypeId, name: &str, _ty: &Type, docs: &Docs) {
        self.type_alias(iface, id, name, &Type::Id(id), docs);
    }

    fn type_builtin(&mut self, iface: &Interface, id: TypeId, name: &str, ty: &Type, docs: &Docs) {
        self.type_alias(iface, id, name, ty, docs)
    }

    fn import(&mut self, iface: &Interface, func: &Function) {
        let sig = FnSig::default();
        let wasm_sig = iface.wasm_signature(AbiVariant::GuestImport, func);
        self.src.push_str("@externalName(\"");
        self.src.push_str(&func.name);
        self.src.push_str("\")\n");
        self.src.push_str("import foreign wasm ");
        self.push_str("wit_bindgen_");
        self.push_str(&to_grain_ident(&func.name));
        self.push_str(": (");
        let params: Vec<&str> = wasm_sig
            .params
            .iter()
            .map(|param| wasm_type(*param))
            .collect();
        self.push_str(&params.join(", "));
        self.push_str(") -> ");
        match wasm_sig.results.len() {
            0 => self.push_str("Void"),
            1 => {
                for result in wasm_sig.results.iter() {
                    self.push_str(wasm_type(*result));
                }
            }
            _ => {
                panic!("multivalue");
            }
        }
        self.push_str(" from ");
        self.push_str(&format!("\"{}\"", iface.name));
        self.src.push_str("\n\n");
        let params = self.print_signature(iface, func, &sig);
        self.src.push_str(" => {\n");

        let mut f = FunctionBindgen::new(self, params);
        iface.call(
            AbiVariant::GuestImport,
            LiftLower::LowerArgsLiftResults,
            func,
            &mut f,
        );
        let FunctionBindgen {
            needs_cleanup_list,
            src,
            ..
        } = f;

        if needs_cleanup_list {
            self.src.push_str("let mut cleanupList = 0n\n");
        }
        self.src.push_str(&String::from(src));

        self.src.push_str("}\n\n");
    }

    fn export(&mut self, iface: &Interface, func: &Function) {
        self.src.push_str("@unsafe\n");
        self.src.push_str("export let ");
        self.src.push_str(&func.name);
        self.src.push_str(" = (");
        let sig = iface.wasm_signature(AbiVariant::GuestExport, func);
        let mut params = Vec::new();
        let mut is_first = true;
        for (i, param) in sig.params.iter().enumerate() {
            if is_first {
                is_first = false;
            } else {
                self.src.push_str(", ");
            }
            let name = format!("arg{}", i);
            self.src.push_str(&name);
            self.src.push_str(": ");
            self.wasm_type(*param);
            params.push(name);
        }
        self.src.push_str(") => {\n");

        let mut f = FunctionBindgen::new(self, params);
        iface.call(
            AbiVariant::GuestExport,
            LiftLower::LiftArgsLowerResults,
            func,
            &mut f,
        );
        let FunctionBindgen {
            needs_cleanup_list,
            src,
            ..
        } = f;
        assert!(!needs_cleanup_list);
        self.src.push_str(&String::from(src));
        self.src.push_str("}\n\n");
    }

    fn finish_one(&mut self, _iface: &Interface, files: &mut Files) {
        let mut full_src = Source::default();

        // TODO: Selectively include only used libraries
        full_src.push_str("import DataStructures from \"runtime/dataStructures\"\n");
        full_src.push_str("import WasmI32 from \"runtime/unsafe/wasmi32\"\n");
        full_src.push_str("import WasmI64 from \"runtime/unsafe/wasmi64\"\n");
        full_src.push_str("import WasmF32 from \"runtime/unsafe/wasmf32\"\n");
        full_src.push_str("import WasmF64 from \"runtime/unsafe/wasmf64\"\n");
        full_src.push_str("import Memory from \"runtime/unsafe/memory\"\n");
        full_src.push_str("import Int32 from \"int32\"\n");
        full_src.push_str("import Int64 from \"int64\"\n");
        full_src.push_str("import Char from \"char\"\n");
        full_src.push_str("import List from \"list\"\n");

        full_src.push_str("\n");

        if self.return_pointer_area_size > 0 {
            full_src.push_str("@unsafe\n");
            full_src.push_str(&format!(
                "let _RET_AREA = Memory.malloc({}n)\n\n",
                self.return_pointer_area_size
            ));
        }

        full_src.push_str(self.src.as_mut_string());

        files.push("bindings.gr", full_src.as_bytes());
    }

    fn generate_one(&mut self, iface: &Interface, dir: Direction, files: &mut Files) {
        self.preprocess_one(iface, dir);

        // Process flags first
        for (id, ty) in iface.types.iter() {
            let name = match &ty.name {
                Some(name) => name,
                None => continue,
            };
            match &ty.kind {
                TypeDefKind::Flags(flags) => self.type_flags(iface, id, name, flags, &ty.docs),
                _ => {
                    continue;
                }
            }
        }

        // Process all other types, allowing them to be mutually recursive
        let mut is_first = true;
        for (id, ty) in iface.types.iter() {
            let name = match &ty.name {
                Some(name) => name,
                None => continue,
            };
            match &ty.kind {
                TypeDefKind::Record(record) => {
                    if record.fields.len() == 0 {
                        // Empty records don't exist in Grain
                        continue;
                    }
                    if is_first {
                        is_first = false
                    } else {
                        self.src.push_str(",\n");
                    }
                    self.type_record(iface, id, name, record, &ty.docs)
                }
                TypeDefKind::Variant(variant) => {
                    if is_first {
                        is_first = false
                    } else {
                        self.src.push_str(",\n");
                    }
                    self.type_variant(iface, id, name, variant, &ty.docs)
                }
                TypeDefKind::Enum(enum_) => {
                    if is_first {
                        is_first = false
                    } else {
                        self.src.push_str(",\n");
                    }
                    self.type_enum(iface, id, name, enum_, &ty.docs)
                }
                TypeDefKind::Union(union_) => {
                    if is_first {
                        is_first = false
                    } else {
                        self.src.push_str(",\n");
                    }
                    self.type_union(iface, id, name, union_, &ty.docs)
                }
                TypeDefKind::Flags(_) => {
                    // already handled above
                    continue;
                }
                TypeDefKind::List(_)
                | TypeDefKind::Type(_)
                | TypeDefKind::Tuple(_)
                | TypeDefKind::Option(_)
                | TypeDefKind::Expected(_) => {
                    if is_first {
                        is_first = false
                    } else {
                        self.src.push_str(",\n");
                    }
                    let typename = name.to_camel_case();
                    self.src.push_str(&format!("\nexport type {typename} = "));
                    self.print_tydef(iface, &ty.kind);
                }
                TypeDefKind::Stream(_) => unimplemented!(),
            }
        }
        self.src.push_str("\n\n");

        for (id, _resource) in iface.resources.iter() {
            self.type_resource(iface, id);
        }

        if let Direction::Export = dir {
            self.src.push_str(&format!(
                "import Bindgen{m} from \"./{}\"\n\n",
                iface.name,
                m = iface.name.to_camel_case(),
            ))
        }

        for f in iface.functions.iter() {
            match dir {
                Direction::Import => self.import(iface, &f),
                Direction::Export => self.export(iface, &f),
            }
        }

        self.finish_one(iface, files)
    }
}

pub fn to_grain_ident(name: &str) -> String {
    match name {
        "assert" => "assert_".into(),
        "break" => "break_".into(),
        "continue" => "continue_".into(),
        "else" => "else_".into(),
        "enum" => "enum_".into(),
        "except" => "except_".into(),
        "exception" => "exception_".into(),
        "export" => "export_".into(),
        "fail" => "fail_".into(),
        "false" => "false_".into(),
        "for" => "for_".into(),
        "foreign" => "foreign_".into(),
        "from" => "from_".into(),
        "if" => "if_".into(),
        "import" => "import_".into(),
        "let" => "let_".into(),
        "match" => "match_".into(),
        "mut" => "mut_".into(),
        "primitive" => "primitive_".into(),
        "rec" => "rec_".into(),
        "record" => "record_".into(),
        "throw" => "throw_".into(),
        "true" => "true_".into(),
        "type" => "type_".into(),
        "void" => "void_".into(),
        "wasm" => "wasm_".into(),
        "when" => "when_".into(),
        "while" => "while_".into(),
        s => s.to_mixed_case(),
    }
}

pub fn is_empty_type(iface: &Interface, ty: &Type) -> bool {
    let id = match ty {
        Type::Id(id) => *id,
        Type::Unit => return true,
        _ => return false,
    };
    match &iface.types[id].kind {
        TypeDefKind::Type(t) => is_empty_type(iface, t),
        TypeDefKind::Record(r) => r.fields.is_empty(),
        TypeDefKind::Tuple(t) => t.types.is_empty(),
        _ => false,
    }
}

pub fn wasm_zero(ty: WasmType) -> &'static str {
    match ty {
        WasmType::I32 => "0n",
        WasmType::I64 => "0N",
        WasmType::F32 => "0.0w",
        WasmType::F64 => "0.0W",
    }
}

pub fn wasm_type(ty: WasmType) -> &'static str {
    match ty {
        WasmType::I32 => "WasmI32",
        WasmType::I64 => "WasmI64",
        WasmType::F32 => "WasmF32",
        WasmType::F64 => "WasmF64",
    }
}

pub fn int_repr(repr: Int) -> &'static str {
    match repr {
        Int::U8 => "Int32",
        Int::U16 => "Int32",
        Int::U32 => "Int32",
        Int::U64 => "Int64",
    }
}

fn flags_repr(f: &Flags) -> Int {
    match f.repr() {
        FlagsRepr::U8 => Int::U8,
        FlagsRepr::U16 => Int::U16,
        FlagsRepr::U32(1) => Int::U32,
        FlagsRepr::U32(2) => Int::U64,
        repr => panic!("unimplemented flags {:?}", repr),
    }
}

pub fn int_suffix(repr: Int) -> &'static str {
    match repr {
        Int::U8 => "l",
        Int::U16 => "l",
        Int::U32 => "l",
        Int::U64 => "L",
    }
}

pub fn case_name(id: &str) -> String {
    if id.chars().next().unwrap().is_alphabetic() {
        id.to_camel_case()
    } else {
        format!("V{}", id)
    }
}

struct FunctionBindgen<'a> {
    gen: &'a mut Grain,
    params: Vec<String>,
    src: Source,
    blocks: Vec<(String, Vec<String>)>,
    block_storage: Vec<(Source, Vec<String>)>,
    tmp: usize,
    needs_cleanup_list: bool,
    free_list: Vec<String>,
}

impl FunctionBindgen<'_> {
    fn new(gen: &mut Grain, params: Vec<String>) -> FunctionBindgen<'_> {
        FunctionBindgen {
            gen,
            params,
            src: Default::default(),
            blocks: Vec::new(),
            block_storage: Vec::new(),
            tmp: 0,
            needs_cleanup_list: false,
            free_list: Vec::new(),
        }
    }
}

pub trait GrainGenerator {
    fn push_str(&mut self, s: &str);
    fn info(&self, ty: TypeId) -> TypeInfo;

    fn graindoc(&mut self, docs: &Docs) {
        let docs = match &docs.contents {
            Some(docs) => docs,
            None => return,
        };
        for line in docs.trim().lines() {
            self.push_str("/// ");
            self.push_str(line);
            self.push_str("\n");
        }
    }

    fn graindoc_params(&mut self, docs: &[(String, Type)], header: &str) {
        drop((docs, header));
        // let docs = docs
        //     .iter()
        //     .filter(|param| param.docs.trim().len() > 0)
        //     .collect::<Vec<_>>();
        // if docs.len() == 0 {
        //     return;
        // }

        // self.push_str("///\n");
        // self.push_str("/// ## ");
        // self.push_str(header);
        // self.push_str("\n");
        // self.push_str("///\n");

        // for param in docs {
        //     for (i, line) in param.docs.lines().enumerate() {
        //         self.push_str("/// ");
        //         // Currently wasi only has at most one return value, so there's no
        //         // need to indent it or name it.
        //         if header != "Return" {
        //             if i == 0 {
        //                 self.push_str("* `");
        //                 self.push_str(to_grain_ident(param.name.as_str()));
        //                 self.push_str("` - ");
        //             } else {
        //                 self.push_str("  ");
        //             }
        //         }
        //         self.push_str(line);
        //         self.push_str("\n");
        //     }
        // }
    }

    fn print_signature(&mut self, iface: &Interface, func: &Function, sig: &FnSig) -> Vec<String> {
        self.print_docs_and_params(iface, func, &sig)
    }
    fn print_docs_and_params(
        &mut self,
        iface: &Interface,
        func: &Function,
        sig: &FnSig,
    ) -> Vec<String> {
        self.graindoc(&func.docs);
        self.push_str("@unsafe\n");
        if !sig.exported {
            self.push_str("export ");
        }
        self.push_str("let ");
        let func_name = &func.name;
        self.push_str(&to_grain_ident(&func_name));
        self.push_str(": (");
        let mut params = Vec::new();
        let length = func.params.len();
        for (i, (_, param)) in func.params.iter().enumerate() {
            self.print_ty(iface, param);
            if i < length - 1 {
                self.push_str(", ");
            }
        }
        self.push_str(") -> ");
        self.print_results(iface, func);
        self.push_str(" = (");
        for (i, (name, _)) in func.params.iter().enumerate() {
            let name = to_grain_ident(name);
            self.push_str(&name);
            params.push(name);
            if i < length - 1 {
                self.push_str(", ");
            }
        }
        self.push_str(")");
        params
    }

    fn print_results(&mut self, iface: &Interface, func: &Function) {
        self.print_ty(iface, &func.result);
    }

    fn print_tydef(&mut self, iface: &Interface, ty: &TypeDefKind) {
        match ty {
            TypeDefKind::Type(t) => self.print_ty(iface, t),
            TypeDefKind::List(Type::Char) => self.push_str("String"),
            TypeDefKind::List(Type::U8) => self.push_str("Bytes"),
            TypeDefKind::List(t) => {
                self.push_str("List<");
                self.print_ty(iface, t);
                self.push_str(">");
            }
            TypeDefKind::Option(t) => {
                self.push_str("Option<");
                self.print_ty(iface, t);
                self.push_str(">");
            }
            TypeDefKind::Expected(Expected { ok, err }) => {
                self.push_str("Result<");
                self.print_ty(iface, ok);
                self.push_str(", ");
                self.print_ty(iface, err);
                self.push_str(">");
            }
            TypeDefKind::Tuple(Tuple { types }) => {
                if types.len() == 0 {
                    // Grain does not have zero-length tuples
                    self.push_str("Void");
                    return;
                }
                self.push_str("(");
                let singleton = types.len() == 1;
                for (i, ty) in types.iter().enumerate() {
                    self.print_ty(iface, ty);
                    if singleton || i != types.len() - 1 {
                        self.push_str(", ");
                    }
                }
                self.push_str(")");
            }
            _ => {
                unreachable!()
            }
        }
    }
    fn print_ty(&mut self, iface: &Interface, ty: &Type) {
        match ty {
            Type::Unit => self.push_str("Void"),
            Type::Bool => self.push_str("Bool"),
            Type::U8 => self.push_str("Int32"),
            Type::S8 => self.push_str("Int32"),
            Type::U16 => self.push_str("Int32"),
            Type::S16 => self.push_str("Int32"),
            Type::U32 => self.push_str("Int32"),
            Type::S32 => self.push_str("Int32"),
            Type::U64 => self.push_str("Int64"),
            Type::S64 => self.push_str("Int64"),
            Type::Float32 => self.push_str("Float32"),
            Type::Float64 => self.push_str("Float64"),
            Type::Char => self.push_str("Char"),
            Type::String => self.push_str("String"),
            Type::Handle(id) => {
                self.push_str(&iface.resources[*id].name.to_camel_case());
            }
            Type::Id(id) => {
                if is_empty_type(iface, ty) {
                    self.push_str("Void");
                    return;
                }
                let ty = &iface.types[*id];
                if let Some(name) = &ty.name {
                    self.push_str(&name.to_camel_case());
                    return;
                }
                self.print_tydef(iface, &ty.kind);
            }
        }
    }

    fn print_tyid(&mut self, iface: &Interface, id: TypeId) {
        let ty = &iface.types[id];
        if ty.name.is_some() {
            let name = self.result_name(iface, id);
            self.push_str(&name);
            return;
        }

        match &ty.kind {
            TypeDefKind::List(t) => self.print_list(iface, t),
            TypeDefKind::Tuple(t) => {
                self.push_str("(");
                // Note the trailing comma after each member to
                // appropriately handle 1-tuples.
                for field in t.types.iter() {
                    self.print_ty(iface, &field);
                    self.push_str(",");
                }
                self.push_str(")");
            }
            TypeDefKind::Type(t) => self.print_ty(iface, t),
            _ => {
                panic!("unsupported anonymous type reference")
            }
        }
    }

    fn print_list(&mut self, iface: &Interface, ty: &Type) {
        match ty {
            Type::Char => self.push_str("String"),
            _ => {
                self.push_str("List<");
                self.print_ty(iface, ty);
                self.push_str(">");
            }
        }
    }

    fn int_repr(&mut self, repr: Int) {
        self.push_str(int_repr(repr));
    }

    fn wasm_type(&mut self, ty: WasmType) {
        self.push_str(wasm_type(ty));
    }

    fn param_name(&self, iface: &Interface, ty: TypeId) -> String {
        iface.types[ty].name.as_ref().unwrap().to_camel_case()
    }

    fn result_name(&self, iface: &Interface, ty: TypeId) -> String {
        iface.types[ty].name.as_ref().unwrap().to_camel_case()
    }
}

#[derive(Default)]
pub struct FnSig {
    pub rec: bool,
    pub exported: bool,
}

pub trait GrainFunctionGenerator {
    fn push_str(&mut self, s: &str);
    fn tmp(&mut self) -> usize;
    fn grain_gen(&self) -> &dyn GrainGenerator;
    fn lift_lower(&self) -> LiftLower;

    fn tuple_lower(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        tuple: &Tuple,
        operand: &str,
        results: &mut Vec<String>,
    ) {
        let tmp = self.tmp();
        let length = tuple.types.len();
        if length == 0 {
            return;
        }
        self.push_str("let (");
        for i in 0..length {
            let arg = format!("t{}_{}", tmp, i);
            self.push_str(&arg);
            if i < length - 1 {
                self.push_str(", ");
            }
            results.push(arg);
        }
        if length == 1 {
            self.push_str(",")
        }
        self.push_str(") = ");
        self.push_str(operand);
        self.push_str("\n");
    }

    fn tuple_lift(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        _ty: &Tuple,
        operands: &[String],
        results: &mut Vec<String>,
    ) {
        if operands.len() == 0 {
            results.push("void".to_string());
        } else if operands.len() == 1 {
            results.push(format!("({},)", operands[0]));
        } else {
            results.push(format!("({})", operands.join(", ")));
        }
    }

    fn record_lower(
        &mut self,
        iface: &Interface,
        id: TypeId,
        record: &Record,
        operand: &str,
        results: &mut Vec<String>,
    ) {
        let tmp = self.tmp();
        // if record.is_tuple() {
        //     let length = record.fields.len();
        //     if length == 0 {
        //         return;
        //     }
        //     self.push_str("let (");
        //     for i in 0..length {
        //         let arg = format!("t{}_{}", tmp, i);
        //         self.push_str(&arg);
        //         if i < length - 1 {
        //             self.push_str(", ");
        //         }
        //         results.push(arg);
        //     }
        //     if length == 1 {
        //         self.push_str(",")
        //     }
        //     self.push_str(") = ");
        //     self.push_str(operand);
        //     self.push_str("\n");
        // } else {
        let num_fields = record.fields.len();
        if num_fields > 0 {
            self.push_str("let ");
            self.push_str("{ ");
            for (i, field) in record.fields.iter().enumerate() {
                let name = to_grain_ident(&field.name);
                let arg = format!("{}{}", name, tmp);
                self.push_str(&name);
                self.push_str(": ");
                self.push_str(&arg);
                if i < num_fields - 1 {
                    self.push_str(", ");
                }
                results.push(arg);
            }
            let name = self.typename_lower(iface, id);
            self.push_str(&format!(" }}: {} = ", name));
            self.push_str(operand);
            self.push_str("\n");
        }
    }

    fn record_lift(
        &mut self,
        _iface: &Interface,
        _id: TypeId,
        ty: &Record,
        operands: &[String],
        results: &mut Vec<String>,
    ) {
        // if ty.is_tuple() {
        //     if operands.len() == 0 {
        //         results.push("void".to_string());
        //     } else if operands.len() == 1 {
        //         results.push(format!("({},)", operands[0]));
        //     } else {
        //         results.push(format!("({})", operands.join(", ")));
        //     }
        // } else {
        if operands.len() > 0 {
            let mut result = String::new();
            result.push_str("{ ");
            let num_fields = ty.fields.len();
            for ((i, field), val) in ty.fields.iter().enumerate().zip(operands) {
                result.push_str(&to_grain_ident(&field.name));
                result.push_str(": ");
                result.push_str(&val);
                if i < num_fields - 1 {
                    result.push_str(", ");
                }
            }
            result.push_str(" }");
            results.push(result);
        } else {
            results.push("void".to_string())
        }
    }

    fn typename_lower(&self, iface: &Interface, id: TypeId) -> String {
        match self.lift_lower() {
            LiftLower::LowerArgsLiftResults => self.grain_gen().param_name(iface, id),
            LiftLower::LiftArgsLowerResults => self.grain_gen().result_name(iface, id),
        }
    }

    fn typename_lift(&self, iface: &Interface, id: TypeId) -> String {
        match self.lift_lower() {
            LiftLower::LiftArgsLowerResults => self.grain_gen().param_name(iface, id),
            LiftLower::LowerArgsLiftResults => self.grain_gen().result_name(iface, id),
        }
    }

    fn enum_lower(
        &mut self,
        iface: &Interface,
        id: TypeId,
        enum_: &Enum,
        operand: &str,
        results: &mut Vec<String>,
    ) {
        let has_name = iface.types[id].name.is_some();
        let mut result_operand = String::new();

        let tmp = self.tmp();
        results.push(format!("result{}", tmp));
        result_operand.push_str(&format!("let mut {} = 0n\n", format!("result{}", tmp)));
        result_operand.push_str("match (");
        result_operand.push_str(operand);
        result_operand.push_str(") {\n");
        for (i, case) in enum_.cases.iter().enumerate() {
            if has_name {
                let name = self.typename_lower(iface, id);
                result_operand.push_str(&name);
                result_operand.push_str(&case_name(&case.name));
            } else {
                unimplemented!()
            }
            result_operand.push_str(" => {\n");
            result_operand.push_str(&format!("{} = {}n\n", format!("result{}", tmp), i));
            result_operand.push_str("},\n");
        }
        result_operand.push_str("}\n");
        self.push_str(&result_operand);
    }

    fn enum_lift_case(
        &mut self,
        iface: &Interface,
        id: TypeId,
        case: &EnumCase,
        result: &mut String,
    ) {
        if iface.types[id].name.is_some() {
            result.push_str(&self.typename_lift(iface, id));
            // result.push_str("::");
            result.push_str(&case_name(&case.name));
        } else {
            unimplemented!()
        }
    }

    fn variant_lower(
        &mut self,
        iface: &Interface,
        id: TypeId,
        variant: &Variant,
        result_types: &&[wit_bindgen_gen_core::wit_parser::abi::WasmType],
        operand: &str,
        results: &mut Vec<String>,
        blocks: Vec<(String, Vec<String>)>,
    ) {
        let has_name = iface.types[id].name.is_some();
        let mut result_operand = String::new();

        let tmp = self.tmp();
        for (i, ty) in result_types.iter().enumerate() {
            results.push(format!("result{}_{}", tmp, i));
            let initial_value = wasm_zero(*ty);
            result_operand.push_str(&format!(
                "let mut {} = {}\n",
                format!("result{}_{}", tmp, i),
                initial_value
            ))
        }
        result_operand.push_str("match (");
        result_operand.push_str(operand);
        result_operand.push_str(") {\n");
        for (case, block) in variant.cases.iter().zip(blocks) {
            if has_name {
                let name = self.typename_lower(iface, id);
                result_operand.push_str(&name);
                result_operand.push_str(&case_name(&case.name));
                if !is_empty_type(iface, &case.ty) {
                    result_operand.push_str("(e)")
                }
            } else {
                unimplemented!()
            }
            result_operand.push_str(" => {\n");
            let (block_src, block_operands) = block;
            result_operand.push_str(&block_src);
            for (i, op) in block_operands.iter().enumerate() {
                result_operand.push_str(&format!("{} = {}\n", format!("result{}_{}", tmp, i), op))
            }
            result_operand.push_str("},\n");
        }
        result_operand.push_str("}\n");
        self.push_str(&result_operand);
    }

    fn variant_lift_case(
        &mut self,
        iface: &Interface,
        id: TypeId,
        case: &Case,
        block: &(String, Vec<String>),
        result: &mut String,
    ) {
        let (block, ops) = block;
        result.push_str(block);
        let args = &ops.join(", ");
        if iface.types[id].name.is_some() {
            result.push_str(&self.typename_lift(iface, id));
            // result.push_str("::");
            result.push_str(&case_name(&case.name));
            if !is_empty_type(iface, &case.ty) {
                result.push_str("(");
                result.push_str(args);
                result.push_str(")");
            }
        } else {
            unimplemented!()
        }
    }

    fn union_lower(
        &mut self,
        iface: &Interface,
        id: TypeId,
        union: &Union,
        result_types: &&[wit_bindgen_gen_core::wit_parser::abi::WasmType],
        operand: &str,
        results: &mut Vec<String>,
        blocks: Vec<(String, Vec<String>)>,
    ) {
        let mut result_operand = String::new();

        let tmp = self.tmp();
        for (i, ty) in result_types.iter().enumerate() {
            results.push(format!("result{}_{}", tmp, i));
            let initial_value = wasm_zero(*ty);
            result_operand.push_str(&format!(
                "let mut {} = {}\n",
                format!("result{}_{}", tmp, i),
                initial_value
            ))
        }
        result_operand.push_str("match (");
        result_operand.push_str(operand);
        result_operand.push_str(") {\n");
        for (i, (case, block)) in union.cases.iter().zip(blocks).enumerate() {
            let name = self.typename_lower(iface, id);
            result_operand.push_str(&name);
            result_operand.push_str(&i.to_string());
            match case.ty {
                Type::Unit => (),
                _ => result_operand.push_str("(e)"),
            }
            result_operand.push_str(" => {\n");
            let (block_src, block_operands) = block;
            result_operand.push_str(&block_src);
            for (i, op) in block_operands.iter().enumerate() {
                result_operand.push_str(&format!("{} = {}\n", format!("result{}_{}", tmp, i), op))
            }
            result_operand.push_str("},\n");
        }
        result_operand.push_str("}\n");
        self.push_str(&result_operand);
    }

    fn union_lift_case(
        &mut self,
        iface: &Interface,
        id: TypeId,
        case: &UnionCase,
        number: usize,
        block: &(String, Vec<String>),
        result: &mut String,
    ) {
        let (block, ops) = block;
        result.push_str(block);
        let args = &ops.join(", ");
        if iface.types[id].name.is_some() {
            result.push_str(&self.typename_lift(iface, id));
            // result.push_str("::");
            result.push_str(&number.to_string());
            match case.ty {
                Type::Unit => (),
                _ => {
                    result.push_str("(");
                    result.push_str(args);
                    result.push_str(")");
                }
            }
        } else {
            unimplemented!()
        }
    }
}

impl GrainFunctionGenerator for FunctionBindgen<'_> {
    fn push_str(&mut self, s: &str) {
        self.src.push_str(s);
    }

    fn tmp(&mut self) -> usize {
        let ret = self.tmp;
        self.tmp += 1;
        ret
    }

    fn grain_gen(&self) -> &dyn GrainGenerator {
        self.gen
    }

    fn lift_lower(&self) -> LiftLower {
        if self.gen.in_import {
            LiftLower::LowerArgsLiftResults
        } else {
            LiftLower::LiftArgsLowerResults
        }
    }
}

impl Bindgen for FunctionBindgen<'_> {
    type Operand = String;

    fn push_block(&mut self) {
        let prev_src = mem::take(&mut self.src);
        let prev_cleanup = mem::take(&mut self.free_list);
        self.block_storage.push((prev_src, prev_cleanup));
    }

    fn finish_block(&mut self, operands: &mut Vec<String>) {
        if self.free_list.len() > 0 {
            self.needs_cleanup_list = true;
            for ptr in mem::take(&mut self.free_list) {
                self.push_str("cleanupList = WasmI32.fromGrain((");
                self.push_str(&ptr);
                self.push_str(", cleanupList))\n");
            }
        }
        let (prev_src, prev_cleanup) = self.block_storage.pop().unwrap();
        let src = mem::replace(&mut self.src, prev_src);
        self.free_list = prev_cleanup;
        self.blocks.push((src.into(), mem::take(operands)));
    }

    fn return_pointer(&mut self, size: usize, _align: usize) -> String {
        self.gen.return_pointer_area_size = self.gen.return_pointer_area_size.max(size);
        "_RET_AREA".into()
    }

    fn sizes(&self) -> &SizeAlign {
        &self.gen.sizes
    }

    fn is_list_canonical(&self, _iface: &Interface, ty: &Type) -> bool {
        match ty {
            // Lists of u8 are Grain Bytes, lists of chars are Grain Strings
            Type::U8 | Type::Char => true,
            _ => false,
        }
    }

    fn emit(
        &mut self,
        iface: &Interface,
        inst: &Instruction<'_>,
        operands: &mut Vec<String>,
        results: &mut Vec<String>,
    ) {
        match inst {
            Instruction::GetArg { nth } => results.push(self.params[*nth].clone()),
            Instruction::I32Const { val } => results.push(format!("{}n", val)),
            Instruction::ConstZero { tys } => {
                for ty in tys.iter() {
                    results.push(wasm_zero(*ty).to_string())
                }
            }

            Instruction::UnitLift => results.push("void".to_string()),
            Instruction::UnitLower => (),
            Instruction::I64FromU64 | Instruction::I64FromS64 => {
                let s = operands.pop().unwrap();
                let tmp = self.tmp();
                let id = format!("int64_{}", tmp);
                self.push_str(&format!(
                    "let {} = WasmI64.load(WasmI32.fromGrain({}), 8n)\n",
                    id, s
                ));
                results.push(id)
            }
            Instruction::I32FromChar => {
                let s = operands.pop().unwrap();
                let tmp = self.tmp();
                let id = format!("int32_{}", tmp);
                self.push_str(&format!(
                    "let {} = WasmI32.shrS(WasmI32.fromGrain(Char.code({})), 1n)\n",
                    id, s
                ));
                results.push(id)
            }

            Instruction::I32FromBool => {
                let s = operands.pop().unwrap();
                let tmp = self.tmp();
                let id = format!("i32_{}", tmp);
                self.push_str(&format!(
                    "let {} = WasmI32.shrS(WasmI32.fromGrain({}), 31n)\n",
                    id, s
                ));
                results.push(id)
            }

            Instruction::BoolFromI32 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let bool_{} = WasmI32.toGrain(WasmI32.or(WasmI32.shl({}, 31n), WasmI32.fromGrain(false))): Bool\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("bool_{}", tmp))
            }

            Instruction::I32FromU8
            | Instruction::I32FromS8
            | Instruction::I32FromU16
            | Instruction::I32FromS16
            | Instruction::I32FromU32
            | Instruction::I32FromS32 => {
                let s = operands.pop().unwrap();
                let tmp = self.tmp();
                let id = format!("int32_{}", tmp);
                self.push_str(&format!(
                    "let {} = WasmI32.load(WasmI32.fromGrain({}), 8n)\n",
                    id, s
                ));
                results.push(id)
            }

            Instruction::S32FromI32
            | Instruction::S8FromI32
            | Instruction::U8FromI32
            | Instruction::S16FromI32
            | Instruction::U16FromI32
            | Instruction::U32FromI32 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let int32_{} = WasmI32.toGrain(DataStructures.newInt32({})): Int32\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("int32_{}", tmp))
            }
            Instruction::S64FromI64 | Instruction::U64FromI64 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let int64_{} = WasmI32.toGrain(DataStructures.newInt64({})): Int64\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("int64_{}", tmp))
            }
            Instruction::Float32FromF32 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let float32_{} = WasmI32.toGrain(DataStructures.newFloat32({})): Float32\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("float32_{}", tmp))
            }
            Instruction::Float64FromF64 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let float64_{} = WasmI32.toGrain(DataStructures.newFloat64({})): Float64\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("float64_{}", tmp))
            }
            Instruction::F32FromFloat32 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let f32_{} = WasmF32.load(WasmI32.fromGrain({}), 8n)\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("f32_{}", tmp))
            }
            Instruction::F64FromFloat64 => {
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let f64_{} = WasmF64.load(WasmI32.fromGrain({}), 8n)\n",
                    tmp,
                    operands.pop().unwrap()
                ));
                results.push(format!("f64_{}", tmp))
            }
            Instruction::CharFromI32 => {
                let s = operands.pop().unwrap();
                let tmp = self.tmp();
                self.push_str(&format!(
                    "let char{} = Char.fromCode(WasmI32.toGrain(WasmI32.add(WasmI32.shl({}, 1n), 1n)): Number)\n",
                    tmp,
                    s,
                ));
                results.push(format!("char{}", tmp))
            }

            Instruction::Bitcasts { casts } => {
                for (cast, operand) in casts.iter().zip(operands) {
                    results.push(match cast {
                        Bitcast::None => operand.clone(),
                        Bitcast::I32ToI64 => format!("WasmI64.extendI32S({})", operand),
                        Bitcast::F32ToI32 => format!("WasmI32.reinterpretF32({})", operand),
                        Bitcast::F64ToI64 => format!("WasmI64.reinterpretF64({})", operand),
                        Bitcast::I64ToI32 => format!("WasmI32.wrapI64({})", operand),
                        Bitcast::I32ToF32 => format!("WasmF32.reinterpretI32({})", operand),
                        Bitcast::I64ToF64 => format!("WasmF64.reinterpretI64({})", operand),
                        Bitcast::F32ToI64 => {
                            format!("WasmI64.reinterpretF64(WasmF64.promoteF32({}))", operand)
                        }
                        Bitcast::I64ToF32 => {
                            format!("WasmF32.reinterpretI32(WasmI32.wrapI64({}))", operand)
                        }
                    });
                }
            }

            Instruction::I32FromOwnedHandle { .. } | Instruction::I32FromBorrowedHandle { .. } => {
                results.push(format!(
                    "WasmI32.load(WasmI32.fromGrain(({})), 8n)",
                    operands[0],
                ));
            }
            Instruction::HandleBorrowedFromI32 { ty } | Instruction::HandleOwnedFromI32 { ty } => {
                results.push(format!(
                    "(WasmI32.toGrain(DataStructures.newInt32({})): {})",
                    operands[0],
                    iface.resources[*ty].name.to_camel_case(),
                ));
            }

            Instruction::FlagsLower { flags, .. } => {
                let tmp = self.tmp();
                match flags.repr() {
                    FlagsRepr::U8 | FlagsRepr::U16 | FlagsRepr::U32(1) => {
                        self.push_str(&format!(
                            "let flags{} = WasmI32.load(WasmI32.fromGrain({}), 8n)\n",
                            tmp, operands[0]
                        ));
                        results.push(format!("flags{}", tmp));
                    }
                    _ => {
                        self.push_str(&format!(
                            "let flags{} = WasmI64.load(WasmI32.fromGrain({}), 8n)\n",
                            tmp, operands[0]
                        ));
                        for i in 0..flags.repr().count() {
                            results.push(format!(
                                "WasmI32.wrapI64(WasmI64.shrU(flags{}, {}N))",
                                tmp,
                                i * 32
                            ));
                        }
                    }
                }
            }
            Instruction::FlagsLift { flags, .. } => {
                let tmp = self.tmp();
                match flags.repr() {
                    FlagsRepr::U8 | FlagsRepr::U16 | FlagsRepr::U32(1) => {
                        self.push_str(&format!(
                            "let flags{} = WasmI32.toGrain(DataStructures.newInt32({})): Int32\n",
                            tmp, operands[0]
                        ));
                        results.push(format!("flags{}", tmp));
                    }
                    _ => {
                        self.push_str(&format!("let mut flags{} = 0N\n", tmp));
                        for (i, op) in operands.iter().enumerate() {
                            self.push_str(&format!(
                                "flags{tmp} = WasmI64.or(flags{tmp}, WasmI64.shl(WasmI64.extendI32U({op}), {}N))\n",
                                i * 32
                            ));
                        }
                        self.push_str(&format!(
                            "let flags{tmp} = WasmI32.toGrain(DataStructures.newInt64(flags{tmp})): Int64\n",
                        ));
                        results.push(format!("flags{}", tmp));
                    }
                }
            }

            Instruction::RecordLower { ty, record, .. } => {
                self.record_lower(iface, *ty, record, &operands[0], results);
            }
            Instruction::RecordLift { ty, record, .. } => {
                self.record_lift(iface, *ty, record, operands, results);
            }

            Instruction::TupleLower { ty, tuple, .. } => {
                self.tuple_lower(iface, *ty, tuple, &operands[0], results);
            }
            Instruction::TupleLift { ty, tuple, .. } => {
                self.tuple_lift(iface, *ty, tuple, operands, results);
            }

            Instruction::UnionLower {
                ty,
                results: result_types,
                union,
                ..
            } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - union.cases.len()..)
                    .collect::<Vec<_>>();
                self.union_lower(
                    iface,
                    *ty,
                    &union,
                    result_types,
                    &operands[0],
                    results,
                    blocks,
                );
            }
            Instruction::UnionLift { ty, union, .. } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - union.cases.len()..)
                    .collect::<Vec<_>>();
                let mut result = format!("match (");
                result.push_str(&operands[0]);
                result.push_str(") {\n");
                for (i, (case, block)) in union.cases.iter().zip(blocks).enumerate() {
                    if i == union.cases.len() - 1 {
                        result.push_str("_");
                    } else {
                        result.push_str(&i.to_string());
                        result.push_str(&"n");
                    }
                    result.push_str(" => {\n");
                    self.union_lift_case(iface, *ty, case, i, &block, &mut result);
                    result.push_str("\n},\n");
                }
                result.push_str("}");
                results.push(result);
            }

            Instruction::VariantPayloadName => results.push("e".to_string()),

            Instruction::EnumLower { enum_, ty, .. } => {
                self.enum_lower(iface, *ty, &enum_, &operands[0], results);
            }
            Instruction::EnumLift { enum_, ty, .. } => {
                let mut result = format!("match (");
                result.push_str(&operands[0]);
                result.push_str(") {\n");
                for (i, case) in enum_.cases.iter().enumerate() {
                    if i == enum_.cases.len() - 1 {
                        result.push_str("_");
                    } else {
                        result.push_str(&i.to_string());
                        result.push_str(&"n");
                    }
                    result.push_str(" => {\n");
                    self.enum_lift_case(iface, *ty, case, &mut result);
                    result.push_str("\n},\n");
                }
                result.push_str("}");
                results.push(result);
            }

            Instruction::VariantLower {
                variant,
                results: result_types,
                ty,
                ..
            } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - variant.cases.len()..)
                    .collect::<Vec<_>>();
                self.variant_lower(
                    iface,
                    *ty,
                    &variant,
                    result_types,
                    &operands[0],
                    results,
                    blocks,
                );
            }

            Instruction::VariantLift { variant, ty, .. } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - variant.cases.len()..)
                    .collect::<Vec<_>>();
                let mut result = format!("match (");
                result.push_str(&operands[0]);
                result.push_str(") {\n");
                for (i, (case, block)) in variant.cases.iter().zip(blocks).enumerate() {
                    if i == variant.cases.len() - 1 {
                        result.push_str("_");
                    } else {
                        result.push_str(&i.to_string());
                        result.push_str(&"n");
                    }
                    result.push_str(" => {\n");
                    self.variant_lift_case(iface, *ty, case, &block, &mut result);
                    result.push_str("\n},\n");
                }
                result.push_str("}");
                results.push(result);
            }

            Instruction::OptionLower {
                results: result_types,
                ..
            } => {
                let tmp = self.tmp();

                let (mut some, some_operands) = self.blocks.pop().unwrap();
                let (mut none, none_operands) = self.blocks.pop().unwrap();
                for (i, op) in some_operands.iter().enumerate() {
                    some.push_str(&format!("{} = {}\n", format!("result{}_{}", tmp, i), op))
                }
                for (i, op) in none_operands.iter().enumerate() {
                    none.push_str(&format!("{} = {}\n", format!("result{}_{}", tmp, i), op))
                }

                let mut result_operand = String::new();
                for (i, ty) in result_types.iter().enumerate() {
                    results.push(format!("result{}_{}", tmp, i));
                    let initial_value = wasm_zero(*ty);
                    result_operand.push_str(&format!(
                        "let mut {} = {}\n",
                        format!("result{}_{}", tmp, i),
                        initial_value
                    ))
                }
                let operand = &operands[0];
                result_operand.push_str(&format!(
                    "match ({operand}) {{
                        Some(e) => {{
                            {some}}},
                        None => {{
                            {none}}},
                    }}\n"
                ));
                self.push_str(&result_operand);
            }

            Instruction::OptionLift { .. } => {
                let (some, some_operands) = self.blocks.pop().unwrap();
                let _ = self.blocks.pop().unwrap();
                let args = &some_operands.join(", ");
                let operand = &operands[0];
                results.push(format!(
                    "match ({operand}) {{
                        0n => None,
                        1n => {{
                            {some}
                            Some({args})
                        }},
                        _ => fail \"invalid enum discriminant\",
                    }}"
                ));
            }

            Instruction::ExpectedLower {
                results: result_types,
                ..
            } => {
                let tmp = self.tmp();

                let (mut err, err_operands) = self.blocks.pop().unwrap();
                let (mut ok, ok_operands) = self.blocks.pop().unwrap();
                for (i, op) in err_operands.iter().enumerate() {
                    err.push_str(&format!("{} = {}\n", format!("result{}_{}", tmp, i), op))
                }
                for (i, op) in ok_operands.iter().enumerate() {
                    ok.push_str(&format!("{} = {}\n", format!("result{}_{}", tmp, i), op))
                }

                let mut result_operand = String::new();
                for (i, ty) in result_types.iter().enumerate() {
                    results.push(format!("result{}_{}", tmp, i));
                    let initial_value = wasm_zero(*ty);
                    result_operand.push_str(&format!(
                        "let mut {} = {}\n",
                        format!("result{}_{}", tmp, i),
                        initial_value
                    ))
                }
                let operand = &operands[0];
                result_operand.push_str(&format!(
                    "match ({operand}) {{
                        Ok(e) => {{
                            {ok}
                        }},
                        Err(e) => {{
                            {err}
                        }},
                    }}\n"
                ));
                self.push_str(&result_operand);
            }

            Instruction::ExpectedLift { .. } => {
                let (err, err_operands) = self.blocks.pop().unwrap();
                let (ok, ok_operands) = self.blocks.pop().unwrap();
                let err_args = &err_operands.join(", ");
                let ok_args = &ok_operands.join(", ");
                let operand = &operands[0];
                results.push(format!(
                    "match ({operand}) {{
                        0n => {{
                            {ok}
                            Ok({ok_args})
                        }},
                        1n => {{
                            {err}
                            Err({err_args})
                        }},
                        _ => fail \"invalid enum discriminant\",
                    }}"
                ));
            }

            Instruction::StringLower { realloc: _, .. } => {
                let tmp = self.tmp();
                let val = format!("vec{}", tmp);
                let ptr = format!("ptr{}", tmp);
                let len = format!("len{}", tmp);
                self.push_str(&format!("let {} = {}\n", val, operands[0]));
                self.push_str(&format!(
                    "let {} = WasmI32.add(WasmI32.fromGrain({}), 8n)\n",
                    ptr, val
                ));
                self.push_str(&format!(
                    "let {} = WasmI32.load(WasmI32.fromGrain({}), 4n)\n",
                    len, val
                ));
                results.push(ptr);
                results.push(len);
            }

            Instruction::StringLift { free, .. } => {
                // This only happens when we're receiving a list from the
                // outside world, so `free` should always be `Some`.
                assert!(free.is_some());
                let tmp = self.tmp();
                let len = format!("len{}", tmp);
                self.push_str(&format!("let {} = {}\n", len, operands[1]));
                self.push_str(&format!(
                    "let str{} = DataStructures.allocateString({})\n",
                    tmp, len
                ));
                self.push_str(&format!(
                    "Memory.copy(WasmI32.add(str{}, 8n), {}, {})\n",
                    tmp, operands[0], len
                ));
                self.push_str(&format!(
                    "let str{0} = WasmI32.toGrain(str{0}): String\n",
                    tmp
                ));
                results.push(format!("str{}", tmp));
            }

            Instruction::ListCanonLower { realloc: _, .. } => {
                // Lower Strings and Bytes
                let tmp = self.tmp();
                let val = format!("vec{}", tmp);
                let ptr = format!("ptr{}", tmp);
                let len = format!("len{}", tmp);
                self.push_str(&format!("let {} = {}\n", val, operands[0]));
                self.push_str(&format!(
                    "let {} = WasmI32.add(WasmI32.fromGrain({}), 8n)\n",
                    ptr, val
                ));
                self.push_str(&format!(
                    "let {} = WasmI32.load(WasmI32.fromGrain({}), 4n)\n",
                    len, val
                ));
                results.push(ptr);
                results.push(len);
            }

            Instruction::ListCanonLift { element, free, .. } => {
                // This only happens when we're receiving a list from the
                // outside world, so `free` should always be `Some`.
                assert!(free.is_some());
                let tmp = self.tmp();
                let len = format!("len{}", tmp);
                self.push_str(&format!("let {} = {}\n", len, operands[1]));
                match element {
                    Type::Char => {
                        self.push_str(&format!(
                            "let str{} = DataStructures.allocateString({})\n",
                            tmp, len
                        ));
                        self.push_str(&format!(
                            "Memory.copy(WasmI32.add(str{}, 8n), {}, {})\n",
                            tmp, operands[0], len
                        ));
                        self.push_str(&format!(
                            "let str{0} = WasmI32.toGrain(str{0}): String\n",
                            tmp
                        ));
                        results.push(format!("str{}", tmp));
                    }
                    _ => {
                        self.push_str(&format!(
                            "let bytes{} = DataStructures.allocateBytes({})\n",
                            tmp, len
                        ));
                        self.push_str(&format!(
                            "Memory.copy(WasmI32.add(bytes{}, 8n), {}, {})\n",
                            tmp, operands[0], len
                        ));
                        self.push_str(&format!(
                            "let bytes{0} = WasmI32.toGrain(bytes{0}): Bytes\n",
                            tmp
                        ));
                        results.push(format!("bytes{}", tmp));
                    }
                }
            }

            Instruction::ListLower { element, realloc } => {
                let body = self.blocks.pop().unwrap();
                let tmp = self.tmp();
                let vec = format!("vec{}", tmp);
                let result = format!("result{}", tmp);
                let len = format!("len{}", tmp);
                self.push_str(&format!("let {} = {}\n", vec, operands[0]));
                self.push_str(&format!(
                    "let {} = WasmI32.shrU(WasmI32.fromGrain(List.length({})), 1n)\n",
                    len, vec
                ));
                let size = self.gen.sizes.size(element);
                self.push_str(&format!(
                    "let {} = Memory.malloc(WasmI32.mul({}, {}n))\n",
                    result, len, size
                ));
                self.push_str(&format!("let mut list = vec{}\n", tmp));
                self.push_str(&format!("let mut i = 0n\n"));
                self.push_str(&format!("while (true) {{\n",));
                self.push_str(&format!("match (list) {{\n",));
                self.push_str(&format!("[] => {{\n",));
                self.push_str(&format!("break\n",));
                self.push_str(&format!("}},\n",));
                self.push_str(&format!("[e, ...rest] => {{\n",));
                self.push_str(&format!("list = rest\n",));
                self.push_str(&format!(
                    "let base = WasmI32.add({}, WasmI32.mul(i, {}n))\n",
                    result, size,
                ));
                self.push_str(&format!("i = WasmI32.add(i, 1n)\n"));
                let (body, _) = body;
                self.push_str(&body);
                self.push_str("}\n");
                self.push_str("}\n");
                self.push_str("}\n");
                results.push(format!("{}", result));
                results.push(len);

                if realloc.is_none() {
                    // If an allocator isn't requested then we must clean up the
                    // allocation ourselves since our callee isn't taking
                    // ownership.
                    self.free_list.push(result);
                }
            }

            Instruction::ListLift { element, free, .. } => {
                // This only happens when we're receiving a list from the
                // outside world, so `free` should always be `Some`.
                assert!(free.is_some());
                let (body, body_ops) = self.blocks.pop().unwrap();
                let tmp = self.tmp();
                let size = self.gen.sizes.size(element);
                let len = format!("len{}", tmp);
                let base = format!("base{}", tmp);
                let result = format!("result{}", tmp);
                self.push_str(&format!("let {} = {}\n", base, operands[0]));
                self.push_str(&format!("let {} = {}\n", len, operands[1],));
                self.push_str(&format!("let mut {} = []\n", result));
                self.push_str(&format!("Memory.incRef(WasmI32.fromGrain({}))\n", result));

                self.push_str("for (let mut i = WasmI32.sub(");
                self.push_str(&len);
                self.push_str(", 1n); WasmI32.gtU(i, 0n); i = WasmI32.sub(i, 1n)) {\n");
                self.push_str("let base = WasmI32.add(");
                self.push_str(&base);
                self.push_str(", WasmI32.mul(i, ");
                self.push_str(&size.to_string());
                self.push_str("n))\n");
                self.push_str("Memory.incRef(WasmI32.fromGrain(cons))\n");
                self.push_str(&body);
                self.push_str(&result);
                self.push_str(" = [");
                self.push_str(&body_ops[0]);
                self.push_str(", ...");
                self.push_str(&result);
                self.push_str("]\n");
                self.push_str("}\n");
                results.push(result);
            }

            Instruction::IterElem { .. } => results.push("e".to_string()),

            Instruction::IterBasePointer => results.push("base".to_string()),

            Instruction::CallWasm {
                module: _,
                name,
                sig,
            } => {
                assert!(sig.results.len() < 2);
                if sig.results.len() > 0 {
                    self.push_str("let ret = ");
                    results.push("ret".to_string());
                }
                self.push_str("wit_bindgen_");
                self.push_str(&to_grain_ident(name));
                self.push_str("(");
                self.push_str(&operands.join(", "));
                self.push_str(")\n");
            }

            Instruction::CallInterface { module, func } => {
                let mut result_operand = String::new();
                match &func.kind {
                    FunctionKind::Freestanding => {
                        result_operand.push_str(&format!(
                            "Bindgen{m}.{n}",
                            n = to_grain_ident(&func.name),
                            m = module.to_camel_case()
                        ));
                    }
                    FunctionKind::Static { resource, name }
                    | FunctionKind::Method { resource, name } => {
                        result_operand.push_str(&format!(
                            "Bindgen{r}.{}",
                            &to_grain_ident(name),
                            r = to_grain_ident(&iface.resources[*resource].name),
                        ));
                    }
                }
                result_operand.push_str("(");
                result_operand.push_str(&operands.join(", "));
                result_operand.push_str(")\n");

                self.push_str("let ret = ");
                results.push("ret".to_string());

                self.push_str(&result_operand);
            }

            Instruction::Return { amt, .. } => {
                if self.needs_cleanup_list {
                    self.push_str("while (WasmI32.ne(cleanupList, 0n)) {\n");
                    self.push_str("let (ptr, restCleanup) = WasmI32.toGrain(cleanupList): (WasmI32, WasmI32)\n");
                    self.push_str("Memory.free(ptr)\n");
                    self.push_str("Memory.free(cleanupList)\n");
                    self.push_str("cleanupList = restCleanup\n");
                    self.push_str("}\n");
                }
                match amt {
                    0 => self.push_str("void\n"),
                    1 => {
                        self.push_str(&operands[0]);
                        self.push_str("\n");
                    }
                    _ => {
                        self.push_str("(");
                        self.push_str(&operands.join(", "));
                        self.push_str(")\n");
                    }
                }
            }

            Instruction::I32Load { offset } => {
                results.push(format!("WasmI32.load({}, {}n)", operands[0], offset));
            }
            Instruction::I32Load8U { offset } => {
                results.push(format!("WasmI32.load8U({}, {}n)", operands[0], offset));
            }
            Instruction::I32Load8S { offset } => {
                results.push(format!("WasmI32.load8S({}, {}n)", operands[0], offset));
            }
            Instruction::I32Load16U { offset } => {
                results.push(format!("WasmI32.load16U({}, {}n)", operands[0], offset));
            }
            Instruction::I32Load16S { offset } => {
                results.push(format!("WasmI32.load16S({}, {}n)", operands[0], offset));
            }
            Instruction::I64Load { offset } => {
                results.push(format!("WasmI64.load({}, {}n)", operands[0], offset));
            }
            Instruction::F32Load { offset } => {
                results.push(format!("WasmF32.load({}, {}n)", operands[0], offset));
            }
            Instruction::F64Load { offset } => {
                results.push(format!("WasmF64.load({}, {}n)", operands[0], offset));
            }
            Instruction::I32Store { offset } => {
                self.push_str(&format!(
                    "WasmI32.store({}, {}, {}n)\n",
                    operands[1], operands[0], offset
                ));
            }
            Instruction::I32Store8 { offset } => {
                self.push_str(&format!(
                    "WasmI32.store8({}, {}, {}n)\n",
                    operands[1], operands[0], offset
                ));
            }
            Instruction::I32Store16 { offset } => {
                self.push_str(&format!(
                    "WasmI32.store16({}, {}, {}n)\n",
                    operands[1], operands[0], offset
                ));
            }
            Instruction::I64Store { offset } => {
                self.push_str(&format!(
                    "WasmI64.store({}, {}, {}n)\n",
                    operands[1], operands[0], offset
                ));
            }
            Instruction::F32Store { offset } => {
                self.push_str(&format!(
                    "WasmF32.store({}, {}, {}n)\n",
                    operands[1], operands[0], offset
                ));
            }
            Instruction::F64Store { offset } => {
                self.push_str(&format!(
                    "WasmF64.store({}, {}, {}n)\n",
                    operands[1], operands[0], offset
                ));
            }
            _ => unimplemented!(),
        }
    }
}
