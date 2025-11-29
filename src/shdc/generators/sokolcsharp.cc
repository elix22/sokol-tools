/*
    Generate output header in C for sokol_gfx.h
*/
#include "sokolcsharp.h"
#include "fmt/format.h"
#include "pystring.h"
#include <stdio.h>

namespace shdc::gen {

using namespace refl;

static const char* sokol_define(Slang::Enum slang) {
    switch (slang) {
        case Slang::GLSL410:
        case Slang::GLSL430:
            return "SOKOL_GLCORE";
        case Slang::GLSL300ES:
            return "SOKOL_GLES3";
        case Slang::HLSL4:
        case Slang::HLSL5:
            return "SOKOL_D3D11";
        case Slang::METAL_MACOS:
        case Slang::METAL_IOS:
        case Slang::METAL_SIM:
            return "SOKOL_METAL";
        case Slang::WGSL:
            return "SOKOL_WGPU";
        default:
            return "<INVALID>";
    }
}

ErrMsg SokolCSharpGenerator::begin(const GenInput& gen) {
    if (!gen.inp.module.empty()) {
        mod_prefix = fmt::format("{}_", gen.inp.module);
    }
    if (gen.args.output_format != Format::SOKOL_IMPL) {
        func_prefix = " ";
    }
    return Generator::begin(gen);
}

void SokolCSharpGenerator::gen_prolog(const GenInput& gen) {
    
    std::string dir;
    std::string filename;
    pystring::os::path::split(dir, filename, gen.args.output);
    if(gen.inp.module != "")
    {
        filename += std::string("_")+gen.inp.module;
    }
    filename = pystring::replace(filename, "-", "_");
    filename = pystring::replace(filename, ".", "_");
    
    l("using static Sokol.SG;\n");
    l("using static Sokol.SG.sg_backend;\n");
    l("using static Sokol.SG.sg_shader_stage;\n");
    l("using static Sokol.SG.sg_uniform_type;\n");
    l("using static Sokol.SG.sg_uniform_layout;\n");
    l("using static Sokol.SG.sg_image_type;\n");
    l("using static Sokol.SG.sg_sampler_type;\n");
    l("using static Sokol.SG.sg_image_sample_type;\n");
    l("using static Sokol.SG.sg_shader_attr_base_type;\n");
    l("using System.Numerics;\n");
    l("using System.Runtime.InteropServices;\n");
    l("\n\n");
    l("using hmm_vec2=System.Numerics.Vector2;\n");
    l("using hmm_vec3=System.Numerics.Vector3;\n");
    l("using hmm_vec4=System.Numerics.Vector4;\n");
    l("using hmm_mat4=System.Numerics.Matrix4x4;\n");
    l("\n\n");
    l("using vec2_t=System.Numerics.Vector2;\n");
    l("using vec3_t=System.Numerics.Vector3;\n");
    l("using vec4_t=System.Numerics.Vector4;\n");
    l("using mat44_t=System.Numerics.Matrix4x4;\n");
    l("\n\n");
    l("namespace {} {{\n\n",filename);
    l("public static unsafe class Shaders \n");
    l_open("{{\n");  
}

void SokolCSharpGenerator::gen_epilog(const GenInput& gen) {
    l_close("}}\n\n");
    l("}} // namescape \n\n");
}

void SokolCSharpGenerator::gen_prerequisites(const GenInput& gen) {
//    if (gen.args.output_format == Format::SOKOL_IMPL) {
//        for (const auto& item: gen.inp.programs) {
//            const Program& prog = item.second;
//            l("const sg_shader_desc* {}{}_shader_desc(sg_backend backend);\n", mod_prefix, prog.name);
//            if (gen.args.reflection) {
//                l("int {}{}_attr_slot(const char* attr_name);\n", mod_prefix, prog.name);
//                l("int {}{}_image_slot(const char* img_name);\n", mod_prefix, prog.name);
//                l("int {}{}_sampler_slot(const char* smp_name);\n", mod_prefix, prog.name);
//                l("int {}{}_uniformblock_slot(const char* ub_name);\n", mod_prefix, prog.name);
//                l("size_t {}{}_uniformblock_size(const char* ub_name);\n", mod_prefix, prog.name);
//                l("int {}{}_storagebuffer_slot(const char* sbuf_name);\n", mod_prefix, prog.name);
//                l("int {}{}_uniform_offset(const char* ub_name, const char* u_name);\n", mod_prefix, prog.name);
//                l("sg_glsl_shader_uniform {}{}_uniform_desc(const char* ub_name, const char* u_name);\n", mod_prefix, prog.name);
//            }
//        }
//    }
}

void SokolCSharpGenerator::print2dArray(std::string name,std::string type,int rows,int columns)
{
    // Generate a Collection struct pattern like in SG.cs
    int total_items = rows * columns;
    l("#pragma warning disable 169\n");
    l_open("public struct {}Collection {{\n", name);
    l("public ref {} this[int row, int column] {{ get {{ fixed ({}* pTP = &_item0) return ref *(pTP + column + (row * {})); }} }}\n", type, type, columns);
    for (int i = 0; i < total_items; i++) {
        l("private {} _item{};\n", type, i);
    }
    l_close("}}\n");
    l("#pragma warning restore 169\n");
    l("public {}Collection {};\n", name, name);
}

void SokolCSharpGenerator::print1dArray(std::string name,std::string type,int size)
{
    // Generate a Collection struct pattern like in SG.cs
    l("#pragma warning disable 169\n");
    l_open("public struct {}Collection {{\n", name);
    l("public ref {} this[int index] => ref MemoryMarshal.CreateSpan(ref _item0, {})[index];\n", type, size);
    for (int i = 0; i < size; i++) {
        l("private {} _item{};\n", type, i);
    }
    l_close("}}\n");
    l("#pragma warning restore 169\n");
    l("public {}Collection {};\n", name, name);
}

void SokolCSharpGenerator::gen_uniform_block_decl(const GenInput &gen, const UniformBlock& ub) {
    int cur_offset = 0;
    l("\n[StructLayout(LayoutKind.Sequential)]\n");
    l_open("public struct {} {{\n", struct_name(ub.name));
    l("public {}(){{}}\n", struct_name(ub.name));
    for (const Type& uniform: ub.struct_info.struct_items) {
        int next_offset = uniform.offset;
        if (next_offset > cur_offset) {
            l("fixed byte _pad_{}[{}];\n", cur_offset, next_offset - cur_offset);
            cur_offset = next_offset;
        }
       
       
        if (gen.inp.ctype_map.count(uniform.type_as_glsl()) > 0) {
            // user-provided type names
            if (uniform.array_count == 0) {
                l("public {} {};\n", gen.inp.ctype_map.at(uniform.type_as_glsl()), uniform.name);
            } else {
                // Generate Collection struct for arrays
                std::string type_name = gen.inp.ctype_map.at(uniform.type_as_glsl());
                l("#pragma warning disable 169\n");
                l_open("public struct {}Collection {{\n", uniform.name);
                l("public ref {} this[int index] => ref MemoryMarshal.CreateSpan(ref _item0, {})[index];\n", type_name, uniform.array_count);
                for (int i = 0; i < uniform.array_count; i++) {
                    l("private {} _item{};\n", type_name, i);
                }
                l_close("}}\n");
                l("#pragma warning restore 169\n");
                l("public {}Collection {};\n", uniform.name, uniform.name);
            }
        } else {
            // default type names (float)
            if (uniform.array_count == 0) {
                switch (uniform.type) {
                    case Type::Float:   l("public float {};\n", uniform.name); break;
                    case Type::Float2:  l("public fixed float {}[2];\n", uniform.name); break;
                    case Type::Float3:  l("public fixed float {}[3];\n", uniform.name); break;
                    case Type::Float4:  l("public fixed float {}[4];\n", uniform.name); break;
                    case Type::Int:     l("public int {};\n", uniform.name); break;
                    case Type::Int2:    l("public fixed int {}[2];\n", uniform.name); break;
                    case Type::Int3:    l("public fixed int {}[3];\n", uniform.name); break;
                    case Type::Int4:    l("public fixed int {}[4];\n", uniform.name); break;
                    case Type::Mat4x4:  l("public fixed float {}[16];\n", uniform.name); break;
                    default:            l("INVALID_UNIFORM_TYPE;\n"); break;
                }
            } else {
                switch (uniform.type) {
                    case Type::Float4:  
                    {
                        
//                      l("float {}[{}][4];\n", uniform.name, uniform.array_count);
                        print2dArray(uniform.name,"float",uniform.array_count,4);
                    }
                    break;
                    case Type::Int4:    
                    {
//                      l("int {}[{}][4];\n",   uniform.name, uniform.array_count);
                        print2dArray(uniform.name,"int",uniform.array_count,4);
                    }
                    break;
                    case Type::Mat4x4:
                    {
//                       l("float {}[{}][16];\n", uniform.name, uniform.array_count);
                        print2dArray(uniform.name,"float",uniform.array_count,16);
                    }
                    break;
                    default:            l("INVALID_UNIFORM_TYPE;\n"); break;
                }
            }
        }
        cur_offset += uniform.size;
    }
    // pad to multiple of 16-bytes struct size
    const int round16 = roundup(cur_offset, 16);
    if (cur_offset < round16) {
        l("fixed byte _pad_{}[{}];\n", cur_offset, round16 - cur_offset);
    }
    l_close("}};\n");

}



void SokolCSharpGenerator::gen_struct_interior_decl_std430(const GenInput& gen, const Type& struc, int pad_to_size) {
    assert(struc.type == Type::Struct);
    assert(pad_to_size > 0);

    int cur_offset = 0;
    for (const Type& item: struc.struct_items) {
        int next_offset = item.offset;
        if (next_offset > cur_offset) {
            l("fixed byte _pad_{}[{}];\n", cur_offset, next_offset - cur_offset);
            cur_offset = next_offset;
        }
        if (item.type == Type::Struct) {
            // recurse into nested struct
            l_open("public struct {} {{\n", item.name);
            //public vs_params_t(){}
            gen_struct_interior_decl_std430(gen, item, item.size);
            if (item.array_count == 0) {
                // FIXME: do we need any padding here if array_stride != struct-size?
                // NOTE: unbounded arrays are written as regular items
                l_close("}};\n");
            } else {
                l_close("}} {}[{}];\n", item.name, item.array_count); // TBD elix22 , needs work
            }
        } else if (gen.inp.ctype_map.count(item.type_as_glsl()) > 0) {
            // user-mapped typename
            if (item.array_count == 0) {
                l("{} {};\n", gen.inp.ctype_map.at(item.type_as_glsl()), item.name);
            } else {
                // Generate Collection struct for arrays
                std::string type_name = gen.inp.ctype_map.at(item.type_as_glsl());
                l("#pragma warning disable 169\n");
                l_open("public struct {}Collection {{\n", item.name);
                l("public ref {} this[int index] => ref MemoryMarshal.CreateSpan(ref _item0, {})[index];\n", type_name, item.array_count);
                for (int i = 0; i < item.array_count; i++) {
                    l("private {} _item{};\n", type_name, i);
                }
                l_close("}}\n");
                l("#pragma warning restore 169\n");
                l("public {}Collection {};\n", item.name, item.name);
            }
        } else {
            // default typenames
            if (item.array_count == 0) {
                switch (item.type) {
                    // NOTE: bool => int is not a bug!
                    case Type::Bool:    l("fixed int {};\n", item.name); break;
                    case Type::Bool2:   l("fixed int {}[2];\n", item.name); break;
                    case Type::Bool3:   l("fixed int {}[3];\n", item.name); break;
                    case Type::Bool4:   l("fixed int {}[4];\n", item.name); break;
                    case Type::Int:     l("fixed int {};\n", item.name); break;
                    case Type::Int2:    l("fixed int {}[2];\n", item.name); break;
                    case Type::Int3:    l("fixed int {}[3];\n", item.name); break;
                    case Type::Int4:    l("fixed int {}[4];\n", item.name); break;
                    case Type::UInt:    l("fixed uint {};\n", item.name); break;
                    case Type::UInt2:   l("fixed uint {}[2];\n", item.name); break;
                    case Type::UInt3:   l("fixed uint {}[3];\n", item.name); break;
                    case Type::UInt4:   l("fixed uint {}[4];\n", item.name); break;
                    case Type::Float:   l("fixed float {};\n", item.name); break;
                    case Type::Float2:  l("fixed float {}[2];\n", item.name); break;
                    case Type::Float3:  l("fixed float {}[3];\n", item.name); break;
                    case Type::Float4:  l("fixed float {}[4];\n", item.name); break;
                    case Type::Mat2x1:  l("fixed float {}[2];\n", item.name); break;
                    case Type::Mat2x2:  l("fixed float {}[4];\n", item.name); break;
                    case Type::Mat2x3:  l("fixed float {}[6];\n", item.name); break;
                    case Type::Mat2x4:  l("fixed float {}[8];\n", item.name); break;
                    case Type::Mat3x1:  l("fixed float {}[3];\n", item.name); break;
                    case Type::Mat3x2:  l("fixed float {}[6];\n", item.name); break;
                    case Type::Mat3x3:  l("fixed float {}[9];\n", item.name); break;
                    case Type::Mat3x4:  l("fixed float {}[12];\n", item.name); break;
                    case Type::Mat4x1:  l("fixed float {}[4];\n", item.name); break;
                    case Type::Mat4x2:  l("fixed float {}[8];\n", item.name); break;
                    case Type::Mat4x3:  l("fixed float {}[12];\n", item.name); break;
                    case Type::Mat4x4:  l("fixed float {}[16];\n", item.name); break;
                    default: l("INVALID_TYPE\n"); break;
                }
            } else {
                switch (item.type) {
                    // NOTE: bool => int is not a bug!
                    case Type::Bool:    print1dArray(item.name,"int",item.array_count);break;
                    case Type::Bool2:   print2dArray( item.name,"int",item.array_count,2);break;
                    case Type::Bool3:   print2dArray( item.name,"int",item.array_count,3);break;
                    case Type::Bool4:   print2dArray( item.name,"int",item.array_count,4);break;
                    case Type::Int:     print1dArray(item.name,"int",item.array_count);break;
                    case Type::Int2:    print2dArray( item.name,"int",item.array_count,2);break;
                    case Type::Int3:    print2dArray( item.name,"int",item.array_count,3);break;
                    case Type::Int4:    print2dArray( item.name,"int",item.array_count,4);break;
                    case Type::UInt:    print1dArray(item.name,"uint",item.array_count);break;
                    case Type::UInt2:   print2dArray( item.name,"uint",item.array_count,2);break;
                    case Type::UInt3:   print2dArray( item.name,"uint",item.array_count,3);break;
                    case Type::UInt4:   print2dArray( item.name,"uint",item.array_count,4);break;
                    case Type::Float:   print1dArray(item.name,"float",item.array_count);break;
                    case Type::Float2:  print2dArray( item.name,"float",item.array_count,2);break;
                    case Type::Float3:  print2dArray( item.name,"float",item.array_count,3);break;
                    case Type::Float4:  print2dArray( item.name,"float",item.array_count,4);break;
                    case Type::Mat2x1:  print2dArray( item.name,"float",item.array_count,2);break;
                    case Type::Mat2x2:  print2dArray( item.name,"float",item.array_count,4);break;
                    case Type::Mat2x3:  print2dArray( item.name,"float",item.array_count,6);break;
                    case Type::Mat2x4:  print2dArray( item.name,"float",item.array_count,8);break;
                    case Type::Mat3x1:  print2dArray( item.name,"float",item.array_count,3);break;
                    case Type::Mat3x2:  print2dArray( item.name,"float",item.array_count,6);break;
                    case Type::Mat3x3:  print2dArray( item.name,"float",item.array_count,9);break;
                    case Type::Mat3x4:  print2dArray( item.name,"float",item.array_count,12);break;
                    case Type::Mat4x1:  print2dArray( item.name,"float",item.array_count,4);break;
                    case Type::Mat4x2:  print2dArray( item.name,"float",item.array_count,8);break;
                    case Type::Mat4x3:  print2dArray( item.name,"float",item.array_count,12);break;
                    case Type::Mat4x4:  print2dArray( item.name,"float",item.array_count,16);break;
                    default: l("INVALID_TYPE\n"); break;
                }
            }
        }
        cur_offset += item.size;
    }
    if (cur_offset < pad_to_size) {
        l("fixed byte _pad_{}[{}];\n", cur_offset, pad_to_size - cur_offset);
    }
}

void SokolCSharpGenerator::gen_storage_buffer_decl(const GenInput& gen, const Type& struc) {
    l_open("struct {} {{\n", struct_name(struc.struct_typename));
    gen_struct_interior_decl_std430(gen, struc, struc.size);
    l_close("}} ;\n");
}


void SokolCSharpGenerator::gen_shader_desc_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static unsafe sg_shader_desc {}{}_shader_desc(sg_backend backend) {{\n",mod_prefix, prog.name);
    for (int i = 0; i < Slang::Num; i++) {
        Slang::Enum slang = Slang::from_index(i);
        if (gen.args.slang & Slang::bit(slang)) {
            if (gen.args.ifdef) {
                l("#if {}\n", sokol_define(slang));
            }
            l_open("if (backend == {}) {{\n", backend(slang));
            l("sg_shader_desc desc = default(sg_shader_desc);\n");
            for (int stage_index = 0; stage_index < ShaderStage::Num; stage_index++) {
                const ShaderStageArrayInfo& info = shader_stage_array_info(gen, prog, ShaderStage::from_index(stage_index), slang);
                if (info.stage == ShaderStage::Invalid) {
                    continue;
                }
                const StageReflection& refl = prog.stages[stage_index];
                std::string dsn;
                switch (info.stage) {
                    case ShaderStage::Vertex: dsn = "desc.vertex_func"; break;
                    case ShaderStage::Fragment: dsn = "desc.fragment_func"; break;
                    case ShaderStage::Compute: dsn = "desc.compute_func"; break;
                    default: dsn = "INVALID"; break;
                }
                if (info.has_bytecode) {
                    l_open("fixed (byte* ptr = {}){{\n",info.bytecode_array_name);
                    l("{}.bytecode.ptr = ptr;\n", dsn);
                    l_close("}}\n");
                    l("{}.bytecode.size = {};\n", dsn, info.bytecode_array_size);
                } else {
                    
                    l("{}.source = System.Text.Encoding.UTF8.GetString({});\n", dsn, info.source_array_name);
                    const char* d3d11_tgt = hlsl_target(slang, info.stage);
                    if (d3d11_tgt) {
                        l("{}.d3d11_target = \"{}\";\n", dsn, d3d11_tgt);
                    }
                }
                l("{}.entry = \"{}\";\n", dsn, refl.entry_point_by_slang(slang));
            }
            if (Slang::is_msl(slang) && prog.has_cs()) {
                l("desc.mtl_threads_per_threadgroup.x = {};\n", prog.cs().cs_workgroup_size[0]);
                l("desc.mtl_threads_per_threadgroup.y = {};\n", prog.cs().cs_workgroup_size[1]);
                l("desc.mtl_threads_per_threadgroup.z = {};\n", prog.cs().cs_workgroup_size[2]);
            }
            if (prog.has_vs()) {
                for (int attr_index = 0; attr_index < StageAttr::Num; attr_index++) {
                    const StageAttr& attr = prog.vs().inputs[attr_index];
                    if (attr.slot >= 0) {
                        l("desc.attrs[{}].base_type = {};\n", attr_index, attr_basetype(attr.type_info.basetype()));
                        if (Slang::is_glsl(slang)) {
                            l("desc.attrs[{}].glsl_name = \"{}\";\n", attr_index, attr.name);
                        } else if (Slang::is_hlsl(slang)) {
                            l("desc.attrs[{}].hlsl_sem_name = \"{}\";\n", attr_index, attr.sem_name);
                            l("desc.attrs[{}].hlsl_sem_index = {};\n", attr_index, attr.sem_index);
                        }
                    }
                }
            }
            for (int ub_index = 0; ub_index < MaxUniformBlocks; ub_index++) {
                const UniformBlock* ub = prog.bindings.find_uniform_block_by_sokol_slot(ub_index);
                if (ub) {
                    const std::string ubn = fmt::format("desc.uniform_blocks[{}]", ub_index);
                    l("{}.stage = {};\n", ubn, shader_stage(ub->stage));
                    l("{}.layout = SG_UNIFORMLAYOUT_STD140;\n", ubn);
                    l("{}.size = {};\n", ubn, roundup(ub->struct_info.size, 16));
                    if (Slang::is_hlsl(slang)) {
                        l("{}.hlsl_register_b_n = {};\n", ubn, ub->hlsl_register_b_n);
                    } else if (Slang::is_msl(slang)) {
                        l("{}.msl_buffer_n = {};\n", ubn, ub->msl_buffer_n);
                    } else if (Slang::is_wgsl(slang)) {
                        l("{}.wgsl_group0_binding_n = {};\n", ubn, ub->wgsl_group0_binding_n);
                    } else if (Slang::is_glsl(slang) && (ub->struct_info.struct_items.size() > 0)) {
                        if (ub->flattened) {
                            // NOT A BUG (to take the type from the first struct item, but the size from the toplevel ub)
                            l("{}.glsl_uniforms[0].type = {};\n", ubn, flattened_uniform_type(ub->struct_info.struct_items[0].type));
                            l("{}.glsl_uniforms[0].array_count = {};\n", ubn, roundup(ub->struct_info.size, 16) / 16);
                            l("{}.glsl_uniforms[0].glsl_name = \"{}\";\n", ubn, ub->name);
                        } else {
                            for (int u_index = 0; u_index < (int)ub->struct_info.struct_items.size(); u_index++) {
                                const Type& u = ub->struct_info.struct_items[u_index];
                                const std::string un = fmt::format("{}.glsl_uniforms[{}]", ubn, u_index);
                                l("{}.type = {};\n", un, uniform_type(u.type));
                                l("{}.array_count = {};\n", un, u.array_count);
                                l("{}.glsl_name = \"{}.{}\";\n", un, ub->inst_name, u.name);
                            }
                        }
                    }
                }
            }
            for (int view_index = 0; view_index < MaxViews; view_index++) {
                const Bindings::View view = prog.bindings.get_view_by_sokol_slot(view_index);
                if (view.type == BindSlot::Type::Texture) {
                    const Texture* tex = &view.texture;
                    const std::string& tn = fmt::format("desc.views[{}].texture", view_index);
                    l("{}.stage = {};\n", tn, shader_stage(tex->stage));
                    l("{}.image_type = {};\n", tn, image_type(tex->type));
                    l("{}.sample_type = {};\n", tn, image_sample_type(tex->sample_type));
                    l("{}.multisampled = {};\n", tn, tex->multisampled ? "true" : "false");
                    if (Slang::is_hlsl(slang)) {
                        l("{}.hlsl_register_t_n = {};\n", tn, tex->hlsl_register_t_n);
                    } else if (Slang::is_msl(slang)) {
                        l("{}.msl_texture_n = {};\n", tn, tex->msl_texture_n);
                    } else if (Slang::is_wgsl(slang)) {
                        l("{}.wgsl_group1_binding_n = {};\n", tn, tex->wgsl_group1_binding_n);
                    }
                } else if (view.type == BindSlot::Type::StorageBuffer) {
                    const StorageBuffer* sbuf = &view.storage_buffer;
                    const std::string& sbn = fmt::format("desc.views[{}].storage_buffer", view_index);
                    l("{}.stage = {};\n", sbn, shader_stage(sbuf->stage));
                    l("{}._readonly = {};\n", sbn, sbuf->readonly);
                    if (Slang::is_hlsl(slang)) {
                        if (sbuf->hlsl_register_t_n >= 0) {
                            l("{}.hlsl_register_t_n = {};\n", sbn, sbuf->hlsl_register_t_n);
                        }
                        if (sbuf->hlsl_register_u_n >= 0) {
                            l("{}.hlsl_register_u_n = {};\n", sbn, sbuf->hlsl_register_u_n);
                        }
                    } else if (Slang::is_msl(slang)) {
                        l("{}.msl_buffer_n = {};\n", sbn, sbuf->msl_buffer_n);
                    } else if (Slang::is_wgsl(slang)) {
                        l("{}.wgsl_group1_binding_n = {};\n", sbn, sbuf->wgsl_group1_binding_n);
                    } else if (Slang::is_glsl(slang)) {
                        l("{}.glsl_binding_n = {};\n", sbn, sbuf->glsl_binding_n);
                    }
                } else if (view.type == BindSlot::Type::StorageImage) {
                    const StorageImage* simg = &view.storage_image;
                    const std::string& sin = fmt::format("desc.views[{}].storage_image", view_index);
                    l("{}.stage = {};\n", sin, shader_stage(simg->stage));
                    l("{}.image_type = {};\n", sin, image_type(simg->type));
                    l("{}.access_format = {};\n", sin, storage_pixel_format(simg->access_format));
                    l("{}.writeonly = {};\n", sin, simg->writeonly);
                    if (Slang::is_hlsl(slang)) {
                        l("{}.hlsl_register_u_n = {};\n", sin, simg->hlsl_register_u_n);
                    } else if (Slang::is_msl(slang)) {
                        l("{}.msl_texture_n = {};\n", sin, simg->msl_texture_n);
                    } else if (Slang::is_wgsl(slang)) {
                        l("{}.wgsl_group1_binding_n = {};\n", sin, simg->wgsl_group1_binding_n);
                    } else if (Slang::is_glsl(slang)) {
                        l("{}.glsl_binding_n = {};\n", sin, simg->glsl_binding_n);
                    }
                }
            }
            for (int smp_index = 0; smp_index < MaxSamplers; smp_index++) {
                const Sampler* smp = prog.bindings.find_sampler_by_sokol_slot(smp_index);
                if (smp) {
                    const std::string sn = fmt::format("desc.samplers[{}]", smp_index);
                    l("{}.stage = {};\n", sn, shader_stage(smp->stage));
                    l("{}.sampler_type = {};\n", sn, sampler_type(smp->type));
                    if (Slang::is_hlsl(slang)) {
                        l("{}.hlsl_register_s_n = {};\n", sn, smp->hlsl_register_s_n);
                    } else if (Slang::is_msl(slang)) {
                        l("{}.msl_sampler_n = {};\n", sn, smp->msl_sampler_n);
                    } else if (Slang::is_wgsl(slang)) {
                        l("{}.wgsl_group1_binding_n = {};\n", sn, smp->wgsl_group1_binding_n);
                    }
                }
            }
            for (int tex_smp_index = 0; tex_smp_index < MaxTextureSamplers; tex_smp_index++) {
                const TextureSampler* tex_smp = prog.bindings.find_texture_sampler_by_sokol_slot(tex_smp_index);
                if (tex_smp) {
                    const std::string tsn = fmt::format("desc.texture_sampler_pairs[{}]", tex_smp_index);
                    l("{}.stage = {};\n", tsn, shader_stage(tex_smp->stage));
                    l("{}.view_slot = {};\n", tsn, prog.bindings.find_texture_by_name(tex_smp->texture_name)->sokol_slot);
                    l("{}.sampler_slot = {};\n", tsn, prog.bindings.find_sampler_by_name(tex_smp->sampler_name)->sokol_slot);
                    if (Slang::is_glsl(slang)) {
                        l("{}.glsl_name = \"{}\";\n", tsn, tex_smp->name);
                    }
                }
            }
            l("desc.label = \"{}{}_shader\";\n", mod_prefix, prog.name);
            l("return desc;\n");
            l_close("}}\n");
            if (gen.args.ifdef) {
                l("#endif /* {} */\n", sokol_define(slang));
            }
        }
    }
    l("return default(sg_shader_desc);\n");
    l_close("}}\n");
}


void SokolCSharpGenerator::gen_attr_slot_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_attr_slot(string attr_name) {{\n",mod_prefix, prog.name);
    for (const StageAttr& attr: prog.vs().inputs) {
        if (attr.slot >= 0) {
            l_open("if (attr_name == \"{}\") {{\n", attr.name);
            l("return {};\n", attr.slot);
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_texture_slot_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_texture_slot(string tex_name) {{\n", mod_prefix, prog.name);
    for (const Texture& tex: prog.bindings.textures) {
        if (tex.sokol_slot >= 0) {
            l_open("if (tex_name == \"{}\") {{\n", tex.name);
            l("return {};\n", tex.sokol_slot);
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_sampler_slot_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_sampler_slot(string smp_name) {{\n",mod_prefix, prog.name);
    for (const Sampler& smp: prog.bindings.samplers) {
        if (smp.sokol_slot >= 0) {
            l_open("if (smp_name == \"{}\") {{\n", smp.name);
            l("return {};\n", smp.sokol_slot);
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_uniform_block_slot_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_uniformblock_slot(string ub_name) {{\n",mod_prefix, prog.name);
    for (const UniformBlock& ub: prog.bindings.uniform_blocks) {
        if (ub.sokol_slot >= 0) {
            l_open("if (ub_name == \"{}\") {{\n", ub.name);
            l("return {};\n", ub.sokol_slot);
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_uniform_block_size_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_uniformblock_size(string ub_name) {{\n",mod_prefix, prog.name);
    for (const UniformBlock& ub: prog.bindings.uniform_blocks) {
        if (ub.sokol_slot >= 0) {
            l_open("if (ub_name == \"{}\") {{\n", ub.name);
            l("return Marshal.SizeOf<{}>();\n", struct_name(ub.name));
            l_close("}}\n");
        }
    }
    l("return 0;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_storage_buffer_slot_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_storagebuffer_slot(string sbuf_name) {{\n",mod_prefix, prog.name);
    for (const StorageBuffer& sbuf: prog.bindings.storage_buffers) {
        if (sbuf.sokol_slot >= 0) {
            l_open("if (sbuf_name == \"{}\") {{\n", sbuf.name);
            l("return {};\n", sbuf.sokol_slot);
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_storage_image_slot_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_storageimage_slot(string simg_name) {{\n", mod_prefix, prog.name);
    for (const StorageImage& simg: prog.bindings.storage_images) {
        if (simg.sokol_slot >= 0) {
            l_open("if (simg_name == \"{}\") {{\n", simg.name);
            l("return {};\n", simg.sokol_slot);
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_uniform_offset_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static int {}{}_uniform_offset(string ub_name, string u_name) {{\n",mod_prefix, prog.name);
    for (const UniformBlock& ub: prog.bindings.uniform_blocks) {
        if (ub.sokol_slot >= 0) {
            l_open("if (ub_name == \"{}\") {{\n", ub.name);
            for (const Type& u: ub.struct_info.struct_items) {
                l_open("if (u_name == \"{}\") {{\n", u.name);
                l("return {};\n", u.offset);
                l_close("}}\n");
            }
            l_close("}}\n");
        }
    }
    l("return -1;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_uniform_desc_refl_func(const GenInput& gen, const ProgramReflection& prog) {
    l_open("public static sg_glsl_shader_uniform {}{}_uniform_desc(string ub_name, string u_name) {{\n", mod_prefix, prog.name);
    l("sg_glsl_shader_uniform res = default;\n");
    for (const UniformBlock& ub: prog.bindings.uniform_blocks) {
        if (ub.sokol_slot >= 0) {
            l_open("if (ub_name == \"{}\") {{\n", ub.name);
            for (const Type& u: ub.struct_info.struct_items) {
                l_open("if (u_name == \"{}\") {{\n", u.name);
                l("res.type = {};\n", uniform_type(u.type));
                l("res.array_count = {};\n", u.array_count);
                l("res.glsl_name = \"{}\";\n", u.name);
                l("return res;\n");
                l_close("}}\n");
            }
            l_close("}}\n");
        }
    }
    l("return res;\n");
    l_close("}}\n");
}

void SokolCSharpGenerator::gen_shader_array_start(const GenInput& gen, const std::string& array_name, size_t num_bytes, Slang::Enum slang) {
    if (gen.args.ifdef) {
        l("#if ({})\n", sokol_define(slang));
    }
    l("static readonly byte[] {} = {{\n", array_name);
    //static readonly byte[] vs_bytecode_metal_macos =
}

void SokolCSharpGenerator::gen_shader_array_end(const GenInput& gen) {
    l("\n}};\n");
    if (gen.args.ifdef) {
        l("#endif\n");
    }
}

void SokolCSharpGenerator::gen_stb_impl_start(const GenInput &gen) {
    if (gen.args.output_format == Format::SOKOL_IMPL) {
        l("#if (SOKOL_SHDC_IMPL)\n");
    }
}

void SokolCSharpGenerator::gen_stb_impl_end(const GenInput& gen) {
    if (gen.args.output_format == Format::SOKOL_IMPL) {
        l("#endif // SOKOL_SHDC_IMPL");
    }
}

std::string SokolCSharpGenerator::lang_name() {
    return "C";
}

std::string SokolCSharpGenerator::shader_bytecode_array_name(const std::string& snippet_name, Slang::Enum slang) {
    return fmt::format("{}{}_bytecode_{}", mod_prefix, snippet_name, Slang::to_str(slang));
}

std::string SokolCSharpGenerator::shader_source_array_name(const std::string& snippet_name, Slang::Enum slang) {
    return fmt::format("{}{}_source_{}", mod_prefix, snippet_name, Slang::to_str(slang));
}

std::string SokolCSharpGenerator::comment_block_start() {
    return "/*";
}

std::string SokolCSharpGenerator::comment_block_end() {
    return "*/";
}

std::string SokolCSharpGenerator::comment_block_line_prefix() {
    return "";
}

std::string SokolCSharpGenerator::get_shader_desc_help(const std::string& prog_name) {
    return fmt::format("{}{}_shader_desc(sg_query_backend());\n", mod_prefix, prog_name);
}

std::string SokolCSharpGenerator::shader_stage(ShaderStage::Enum e) {
    switch (e) {
        case ShaderStage::Vertex: return "SG_SHADERSTAGE_VERTEX";
        case ShaderStage::Fragment: return "SG_SHADERSTAGE_FRAGMENT";
        case ShaderStage::Compute: return "SG_SHADERSTAGE_COMPUTE";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::attr_basetype(Type::Enum e) {
    switch (e) {
        case Type::Float:   return "SG_SHADERATTRBASETYPE_FLOAT";
        case Type::Int:     return "SG_SHADERATTRBASETYPE_SINT";
        case Type::UInt:    return "SG_SHADERATTRBASETYPE_UINT";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::uniform_type(Type::Enum e) {
    switch (e) {
        case Type::Float:  return "SG_UNIFORMTYPE_FLOAT";
        case Type::Float2: return "SG_UNIFORMTYPE_FLOAT2";
        case Type::Float3: return "SG_UNIFORMTYPE_FLOAT3";
        case Type::Float4: return "SG_UNIFORMTYPE_FLOAT4";
        case Type::Int:    return "SG_UNIFORMTYPE_INT";
        case Type::Int2:   return "SG_UNIFORMTYPE_INT2";
        case Type::Int3:   return "SG_UNIFORMTYPE_INT3";
        case Type::Int4:   return "SG_UNIFORMTYPE_INT4";
        case Type::Mat4x4: return "SG_UNIFORMTYPE_MAT4";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::flattened_uniform_type(Type::Enum e) {
    switch (e) {
        case Type::Float:
        case Type::Float2:
        case Type::Float3:
        case Type::Float4:
        case Type::Mat4x4:
             return "SG_UNIFORMTYPE_FLOAT4";
        case Type::Int:
        case Type::Int2:
        case Type::Int3:
        case Type::Int4:
            return "SG_UNIFORMTYPE_INT4";
        default:
            return "INVALID";
    }
}

std::string SokolCSharpGenerator::image_type(ImageType::Enum e) {
    switch (e) {
        case ImageType::_2D:     return "SG_IMAGETYPE_2D";
        case ImageType::CUBE:    return "SG_IMAGETYPE_CUBE";
        case ImageType::_3D:     return "SG_IMAGETYPE_3D";
        case ImageType::ARRAY:   return "SG_IMAGETYPE_ARRAY";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::image_sample_type(ImageSampleType::Enum e) {
    switch (e) {
        case ImageSampleType::FLOAT: return "SG_IMAGESAMPLETYPE_FLOAT";
        case ImageSampleType::DEPTH: return "SG_IMAGESAMPLETYPE_DEPTH";
        case ImageSampleType::SINT:  return "SG_IMAGESAMPLETYPE_SINT";
        case ImageSampleType::UINT:  return "SG_IMAGESAMPLETYPE_UINT";
        case ImageSampleType::UNFILTERABLE_FLOAT:  return "SG_IMAGESAMPLETYPE_UNFILTERABLE_FLOAT";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::sampler_type(SamplerType::Enum e) {
    switch (e) {
        case SamplerType::FILTERING:     return "SG_SAMPLERTYPE_FILTERING";
        case SamplerType::COMPARISON:    return "SG_SAMPLERTYPE_COMPARISON";
        case SamplerType::NONFILTERING:  return "SG_SAMPLERTYPE_NONFILTERING";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::storage_pixel_format(refl::StoragePixelFormat::Enum e) {
    switch (e) {
        case StoragePixelFormat::RGBA8:     return "SG_PIXELFORMAT_RGBA8";
        case StoragePixelFormat::RGBA8SN:   return "SG_PIXELFORMAT_RGBA8SN";
        case StoragePixelFormat::RGBA8UI:   return "SG_PIXELFORMAT_RGBA8UI";
        case StoragePixelFormat::RGBA8SI:   return "SG_PIXELFORMAT_RGBS8SI";
        case StoragePixelFormat::RGBA16UI:  return "SG_PIXELFORMAT_RGBA16UI";
        case StoragePixelFormat::RGBA16SI:  return "SG_PIXELFORMAT_RGBA16SI";
        case StoragePixelFormat::RGBA16F:   return "SG_PIXELFORMAT_RGBA16F";
        case StoragePixelFormat::R32UI:     return "SG_PIXELFORMAT_R32UI";
        case StoragePixelFormat::R32SI:     return "SG_PIXELFORMAT_R32SI";
        case StoragePixelFormat::R32F:      return "SG_PIXELFORMAT_R32F";
        case StoragePixelFormat::RG32UI:    return "SG_PIXELFORMAT_RG32UI";
        case StoragePixelFormat::RG32SI:    return "SG_PIXELFORMAT_RG32SI";
        case StoragePixelFormat::RG32F:     return "SG_PIXELFORMAT_RG32F";
        case StoragePixelFormat::RGBA32UI:  return "SG_PIXELFORMAT_RGBA32UI";
        case StoragePixelFormat::RGBA32SI:  return "SG_PIXELFORMAT_RGBA32SI";
        case StoragePixelFormat::RGBA32F:   return "SG_PIXELFORMAT_RGBA32F";
        default: return "INVALID";
    }
}

std::string SokolCSharpGenerator::backend(Slang::Enum e) {
    switch (e) {
        case Slang::GLSL410:
        case Slang::GLSL430:
            return "SG_BACKEND_GLCORE";
        case Slang::GLSL300ES:
            return "SG_BACKEND_GLES3";
        case Slang::HLSL4:
        case Slang::HLSL5:
            return "SG_BACKEND_D3D11";
        case Slang::METAL_MACOS:
            return "SG_BACKEND_METAL_MACOS";
        case Slang::METAL_IOS:
            return "SG_BACKEND_METAL_IOS";
        case Slang::METAL_SIM:
            return "SG_BACKEND_METAL_SIMULATOR";
        case Slang::WGSL:
            return "SG_BACKEND_WGPU";
        default:
            return "<INVALID>";
    }
}

std::string SokolCSharpGenerator::struct_name(const std::string& name) {
    return fmt::format("{}{}_t", mod_prefix, name);
}

std::string SokolCSharpGenerator::vertex_attr_name(const std::string& prog_name, const StageAttr& attr) {
    return fmt::format("ATTR_{}{}_{}", mod_prefix, prog_name, attr.name);
}

std::string SokolCSharpGenerator::texture_bind_slot_name(const Texture& tex) {
    return fmt::format("VIEW_{}{}", mod_prefix, tex.name);
}

std::string SokolCSharpGenerator::storage_buffer_bind_slot_name(const StorageBuffer& sbuf) {
    return fmt::format("VIEW_{}{}", mod_prefix, sbuf.name);
}

std::string SokolCSharpGenerator::storage_image_bind_slot_name(const StorageImage& simg) {
    return fmt::format("VIEW_{}{}", mod_prefix, simg.name);
}


std::string SokolCSharpGenerator::sampler_bind_slot_name(const Sampler& smp) {
    return fmt::format("SMP_{}{}", mod_prefix, smp.name);
}

std::string SokolCSharpGenerator::uniform_block_bind_slot_name(const UniformBlock& ub) {
    return fmt::format("UB_{}{}", mod_prefix, ub.name);
}

std::string SokolCSharpGenerator::vertex_attr_definition(const std::string& prog_name, const StageAttr& attr) {
    return fmt::format("public const int {} = {};", vertex_attr_name(prog_name, attr), attr.slot);
}

std::string SokolCSharpGenerator::texture_bind_slot_definition(const Texture& tex) {
    return fmt::format("public const int {} = {};", texture_bind_slot_name(tex), tex.sokol_slot);
}

std::string SokolCSharpGenerator::sampler_bind_slot_definition(const Sampler& smp) {
    return fmt::format("public const int {} = {};", sampler_bind_slot_name(smp), smp.sokol_slot);
}

std::string SokolCSharpGenerator::uniform_block_bind_slot_definition(const UniformBlock& ub) {
    return fmt::format("public const int {} = {};", uniform_block_bind_slot_name(ub), ub.sokol_slot);
}

std::string SokolCSharpGenerator::storage_buffer_bind_slot_definition(const StorageBuffer& sbuf) {
    return fmt::format("public const int {} = {};", storage_buffer_bind_slot_name(sbuf), sbuf.sokol_slot);
}

std::string SokolCSharpGenerator::storage_image_bind_slot_definition(const StorageImage& simg) {
    return fmt::format("public const int {} = {};", storage_image_bind_slot_name(simg), simg.sokol_slot);
}

} // namespace
