/*
    Generate output header in C for sokol_gfx.h
*/
#include "shdc.h"
#include "fmt/format.h"
#include "pystring.h"
#include <stdio.h>

namespace shdc {

using namespace util;

static std::string file_content;

#if defined(_MSC_VER)
#define L(str, ...) file_content.append(fmt::format(str, __VA_ARGS__))
#else
#define L(str, ...) file_content.append(fmt::format(str, ##__VA_ARGS__))
#endif

static const char* uniform_type_to_sokol_type_str(uniform_t::type_t type) {
    switch (type) {
        case uniform_t::FLOAT:  return "SG_UNIFORMTYPE_FLOAT";
        case uniform_t::FLOAT2: return "SG_UNIFORMTYPE_FLOAT2";
        case uniform_t::FLOAT3: return "SG_UNIFORMTYPE_FLOAT3";
        case uniform_t::FLOAT4: return "SG_UNIFORMTYPE_FLOAT4";
        case uniform_t::INT:    return "SG_UNIFORMTYPE_INT";
        case uniform_t::INT2:   return "SG_UNIFORMTYPE_INT2";
        case uniform_t::INT3:   return "SG_UNIFORMTYPE_INT3";
        case uniform_t::INT4:   return "SG_UNIFORMTYPE_INT4";
        case uniform_t::MAT4: return "SG_UNIFORMTYPE_MAT4";
        default: return "FIXME";
    }
}

static const char* uniform_type_to_flattened_sokol_type_str(uniform_t::type_t type) {
    switch (type) {
        case uniform_t::FLOAT:
        case uniform_t::FLOAT2:
        case uniform_t::FLOAT3:
        case uniform_t::FLOAT4:
        case uniform_t::MAT4:
             return "SG_UNIFORMTYPE_FLOAT4";
        case uniform_t::INT:
        case uniform_t::INT2:
        case uniform_t::INT3:
        case uniform_t::INT4:
            return "SG_UNIFORMTYPE_INT4";
        default: return "FIXME";
    }
}

static const char* img_type_to_sokol_type_str(image_t::type_t type) {
    switch (type) {
        case image_t::IMAGE_TYPE_2D:    return "SG_IMAGETYPE_2D";
        case image_t::IMAGE_TYPE_CUBE:  return "SG_IMAGETYPE_CUBE";
        case image_t::IMAGE_TYPE_3D:    return "SG_IMAGETYPE_3D";
        case image_t::IMAGE_TYPE_ARRAY: return "SG_IMAGETYPE_ARRAY";
        default: return "INVALID";
    }
}

static const char* img_basetype_to_sokol_sampletype_str(image_t::sampletype_t sampletype) {
    switch (sampletype) {
        case image_t::IMAGE_SAMPLETYPE_FLOAT: return "SG_IMAGESAMPLETYPE_FLOAT";
        case image_t::IMAGE_SAMPLETYPE_SINT:  return "SG_IMAGESAMPLETYPE_SINT";
        case image_t::IMAGE_SAMPLETYPE_UINT:  return "SG_IMAGESAMPLETYPE_UINT";
        case image_t::IMAGE_SAMPLETYPE_DEPTH: return "SG_IMAGESAMPLETYPE_DEPTH";
        default: return "INVALID";
    }
}

static const char* smp_type_to_sokol_type_str(sampler_t::type_t type) {
    switch (type) {
        case sampler_t::SAMPLER_TYPE_SAMPLE: return "SG_SAMPLERTYPE_SAMPLE";
        case sampler_t::SAMPLER_TYPE_COMPARE: return "SG_SAMPLERTYPE_COMPARE";
        default: return "INVALID";
    }
}

static const char* sokol_define(slang_t::type_t slang) {
    switch (slang) {
        case slang_t::GLSL330:      return "SOKOL_GLCORE33";
        case slang_t::GLSL100:      return "SOKOL_GLES2";
        case slang_t::GLSL300ES:    return "SOKOL_GLES3";
        case slang_t::HLSL4:        return "SOKOL_D3D11";
        case slang_t::HLSL5:        return "SOKOL_D3D11";
        case slang_t::METAL_MACOS:  return "SOKOL_METAL";
        case slang_t::METAL_IOS:    return "SOKOL_METAL";
        case slang_t::METAL_SIM:    return "SOKOL_METAL";
        case slang_t::WGSL:         return "SOKOL_WGPU";
        default: return "<INVALID>";
    }
}

static const char* sokol_backend(slang_t::type_t slang) {
    switch (slang) {
        case slang_t::GLSL330:      return "SG_BACKEND_GLCORE33";
        case slang_t::GLSL100:      return "SG_BACKEND_GLES2";
        case slang_t::GLSL300ES:    return "SG_BACKEND_GLES3";
        case slang_t::HLSL4:        return "SG_BACKEND_D3D11";
        case slang_t::HLSL5:        return "SG_BACKEND_D3D11";
        case slang_t::METAL_MACOS:  return "SG_BACKEND_METAL_MACOS";
        case slang_t::METAL_IOS:    return "SG_BACKEND_METAL_IOS";
        case slang_t::METAL_SIM:    return "SG_BACKEND_METAL_SIMULATOR";
        case slang_t::WGSL:         return "SG_BACKEND_WGPU";
        default: return "<INVALID>";
    }
}

static void write_header(const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    L("/*\n");
    L("    #version:{}# (machine generated, don't edit!)\n\n", args.gen_version);
    L("    Generated by sokol-shdc (https://github.com/floooh/sokol-tools)\n\n");
    L("    Cmdline: {}\n\n", args.cmdline);
    L("    Overview:\n\n");
    for (const auto& item: inp.programs) {
        const program_t& prog = item.second;

        const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
        const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
        assert(vs_src && fs_src);
        L("        Shader program '{}':\n", prog.name);
        L("            Get shader desc: {}{}_shader_desc(sg_query_backend());\n", mod_prefix(inp), prog.name);
        L("            Vertex shader: {}\n", prog.vs_name);
        L("                Attribute slots:\n");
        const snippet_t& vs_snippet = inp.snippets[vs_src->snippet_index];
        for (const attr_t& attr: vs_src->refl.inputs) {
            if (attr.slot >= 0) {
                L("                    ATTR_{}{}_{} = {}\n", mod_prefix(inp), vs_snippet.name, attr.name, attr.slot);
            }
        }
        for (const uniform_block_t& ub: vs_src->refl.uniform_blocks) {
            L("                Uniform block '{}':\n", ub.struct_name);
            L("                    C struct: {}{}_t\n", mod_prefix(inp), ub.struct_name);
            L("                    Bind slot: SLOT_{}{} = {}\n", mod_prefix(inp), ub.struct_name, ub.slot);
        }
        for (const image_t& img: vs_src->refl.images) {
            L("                Image '{}':\n", img.name);
            L("                    Image Type: {}\n", img_type_to_sokol_type_str(img.type));
            L("                    Sample Type: {}\n", img_basetype_to_sokol_sampletype_str(img.sample_type));
            L("                    Multisampled: {}\n", img.multisampled);
            L("                    Bind slot: SLOT_{}{} = {}\n", mod_prefix(inp), img.name, img.slot);
        }
        for (const sampler_t& smp: vs_src->refl.samplers) {
            L("                Sampler '{}':\n", smp.name);
            L("                    Type: {}\n", smp_type_to_sokol_type_str(smp.type));
            L("                    Bind slot: SLOT_{}{} = {}\n", mod_prefix(inp), smp.name, smp.slot);
        }
        L("            Fragment shader: {}\n", prog.fs_name);
        for (const uniform_block_t& ub: fs_src->refl.uniform_blocks) {
            L("                Uniform block '{}':\n", ub.struct_name);
            L("                    C struct: {}{}_t\n", mod_prefix(inp), ub.struct_name);
            L("                    Bind slot: SLOT_{}{} = {}\n", mod_prefix(inp), ub.struct_name, ub.slot);
        }
        for (const image_t& img: fs_src->refl.images) {
            L("                Image '{}':\n", img.name);
            L("                    Type: {}\n", img_type_to_sokol_type_str(img.type));
            L("                    Sample Type: {}\n", img_basetype_to_sokol_sampletype_str(img.sample_type));
            L("                    Bind slot: SLOT_{}{} = {}\n", mod_prefix(inp), img.name, img.slot);
        }
        for (const sampler_t& smp: fs_src->refl.samplers) {
            L("                Sampler '{}':\n", smp.name);
            L("                    Type: {}\n", smp_type_to_sokol_type_str(smp.type));
            L("                    Bind slot: SLOT_{}{} = {}\n", mod_prefix(inp), smp.name, smp.slot);
        }
        L("\n");
    }
    L("\n");
    L("    Shader descriptor structs:\n\n");
    for (const auto& item: inp.programs) {
        const program_t& prog = item.second;
        L("        sg_shader {} = sg_make_shader({}{}_shader_desc(sg_query_backend()));\n", prog.name, mod_prefix(inp), prog.name);
    }
    L("\n");
    for (const spirvcross_source_t& src: spirvcross.sources) {
        if (src.refl.stage == stage_t::VS) {
            const snippet_t& vs_snippet = inp.snippets[src.snippet_index];
            L("    Vertex attribute locations for vertex shader '{}':\n\n", vs_snippet.name);
            L("        sg_pipeline pip = sg_make_pipeline(&(sg_pipeline_desc){{\n");
            L("            .layout = {{\n");
            L("                .attrs = {{\n");
            for (const attr_t& attr: src.refl.inputs) {
                if (attr.slot >= 0) {
                    L("                    [ATTR_{}{}_{}] = {{ ... }},\n", mod_prefix(inp), vs_snippet.name, attr.name);
                }
            }
            L("                }},\n");
            L("            }},\n");
            L("            ...}});\n");
            L("\n");
        }
    }
    L("\n");
    L("    Image bind slots, use as index in sg_bindings.vs.images[] or .fs.images[]\n\n");
    for (const image_t& img: spirvcross.unique_images) {
        L("        SLOT_{}{} = {};\n", mod_prefix(inp), img.name, img.slot);
    }
    L("\n");
    L("    Sampler bind slots, use as index in sg_bindings.vs.sampler[] or .fs.samplers[]\n\n");
    for (const sampler_t& smp: spirvcross.unique_samplers) {
        L("        SLOT_{}{} = {};\n", mod_prefix(inp), smp.name, smp.slot);
    }
    L("\n");
    for (const uniform_block_t& ub: spirvcross.unique_uniform_blocks) {
        L("    Bind slot and C-struct for uniform block '{}':\n\n", ub.struct_name);
        L("        {}{}_t {} = {{\n", mod_prefix(inp), ub.struct_name, ub.struct_name);
        for (const uniform_t& uniform: ub.uniforms) {
            L("            .{} = ...;\n", uniform.name);
        };
        L("        }};\n");
        L("        sg_apply_uniforms(SG_SHADERSTAGE_[VS|FS], SLOT_{}{}, &SG_RANGE({}));\n", mod_prefix(inp), ub.struct_name, ub.struct_name);
        L("\n");
    }
    L("*/\n");
    L("#include <stdint.h>\n");
    L("#include <stdbool.h>\n");
    L("#include <string.h>\n");
    L("#include <stddef.h>\n");
    for (const auto& header: inp.headers) {
        L("{}\n", header);
    }
}

static void write_vertex_attrs(const input_t& inp, const spirvcross_t& spirvcross) {
    // vertex attributes
    for (const spirvcross_source_t& src: spirvcross.sources) {
        if (src.refl.stage == stage_t::VS) {
            const snippet_t& vs_snippet = inp.snippets[src.snippet_index];
            for (const attr_t& attr: src.refl.inputs) {
                if (attr.slot >= 0) {
                    L("#define ATTR_{}{}_{} ({})\n", mod_prefix(inp), vs_snippet.name, attr.name, attr.slot);
                }
            }
        }
    }
}

static void write_image_bind_slots(const input_t& inp, const spirvcross_t& spirvcross) {
    for (const image_t& img: spirvcross.unique_images) {
        L("#define SLOT_{}{} ({})\n", mod_prefix(inp), img.name, img.slot);
    }
}

static void write_sampler_bind_slots(const input_t& inp, const spirvcross_t& spirvcross) {
    for (const sampler_t& smp: spirvcross.unique_samplers) {
        L("#define SLOT_{}{} ({})\n", mod_prefix(inp), smp.name, smp.slot);
    }
}

static void write_uniform_blocks(const input_t& inp, const spirvcross_t& spirvcross, slang_t::type_t slang) {
    for (const uniform_block_t& ub: spirvcross.unique_uniform_blocks) {
        L("#define SLOT_{}{} ({})\n", mod_prefix(inp), ub.struct_name, ub.slot);
        L("#pragma pack(push,1)\n");
        int cur_offset = 0;
        L("SOKOL_SHDC_ALIGN(16) typedef struct {}{}_t {{\n", mod_prefix(inp), ub.struct_name);
        for (const uniform_t& uniform: ub.uniforms) {
            int next_offset = uniform.offset;
            if (next_offset > cur_offset) {
                L("    uint8_t _pad_{}[{}];\n", cur_offset, next_offset - cur_offset);
                cur_offset = next_offset;
            }
            if (inp.ctype_map.count(uniform_type_str(uniform.type)) > 0) {
                // user-provided type names
                if (uniform.array_count == 1) {
                    L("    {} {};\n", inp.ctype_map.at(uniform_type_str(uniform.type)), uniform.name);
                }
                else {
                    L("    {} {}[{}];\n", inp.ctype_map.at(uniform_type_str(uniform.type)), uniform.name, uniform.array_count);
                }
            }
            else {
                // default type names (float)
                if (uniform.array_count == 1) {
                    switch (uniform.type) {
                        case uniform_t::FLOAT:      L("    float {};\n", uniform.name); break;
                        case uniform_t::FLOAT2:     L("    float {}[2];\n", uniform.name); break;
                        case uniform_t::FLOAT3:     L("    float {}[3];\n", uniform.name); break;
                        case uniform_t::FLOAT4:     L("    float {}[4];\n", uniform.name); break;
                        case uniform_t::INT:        L("    int {};\n", uniform.name); break;
                        case uniform_t::INT2:       L("    int {}[2];\n", uniform.name); break;
                        case uniform_t::INT3:       L("    int {}[3];\n", uniform.name); break;
                        case uniform_t::INT4:       L("    int {}[4];\n", uniform.name); break;
                        case uniform_t::MAT4:       L("    float {}[16];\n", uniform.name); break;
                        default:                    L("    INVALID_UNIFORM_TYPE;\n"); break;
                    }
                }
                else {
                    switch (uniform.type) {
                        case uniform_t::FLOAT4:     L("    float {}[{}][4];\n", uniform.name, uniform.array_count); break;
                        case uniform_t::INT4:       L("    int {}[{}][4];\n",   uniform.name, uniform.array_count); break;
                        case uniform_t::MAT4:       L("    float {}[{}][16];\n", uniform.name, uniform.array_count); break;
                        default:                    L("    INVALID_UNIFORM_TYPE;\n"); break;
                    }
                }
            }
            cur_offset += uniform_size(uniform.type, uniform.array_count);
        }
        /* pad to multiple of 16-bytes struct size */
        const int round16 = roundup(cur_offset, 16);
        if (cur_offset != round16) {
            L("    uint8_t _pad_{}[{}];\n", cur_offset, round16-cur_offset);
        }
        L("}} {}{}_t;\n", mod_prefix(inp), ub.struct_name);
        L("#pragma pack(pop)\n");
    }
}

static void write_common_decls(slang_t::type_t slang, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    if (args.output_format == format_t::SOKOL_IMPL) {
        L("#if !defined(SOKOL_GFX_INCLUDED)\n");
        L("  #error \"Please include sokol_gfx.h before {}\"\n", pystring::os::path::basename(args.output));
        L("#endif\n");
    }
    L("#if !defined(SOKOL_SHDC_ALIGN)\n");
    L("  #if defined(_MSC_VER)\n");
    L("    #define SOKOL_SHDC_ALIGN(a) __declspec(align(a))\n");
    L("  #else\n");
    L("    #define SOKOL_SHDC_ALIGN(a) __attribute__((aligned(a)))\n");
    L("  #endif\n");
    L("#endif\n");
    if (args.output_format == format_t::SOKOL_IMPL) {
        for (const auto& item: inp.programs) {
            const program_t& prog = item.second;
            L("const sg_shader_desc* {}{}_shader_desc(sg_backend backend);\n", mod_prefix(inp), prog.name);
            if (args.reflection) {
                L("int {}{}_attr_slot(const char* attr_name);\n", mod_prefix(inp), prog.name);
                L("int {}{}_image_slot(sg_shader_stage stage, const char* img_name);\n", mod_prefix(inp), prog.name);
                L("int {}{}_sampler_slot(sg_shader_stage stage, const char* smp_name);\n", mod_prefix(inp), prog.name);
                L("int {}{}_uniformblock_slot(sg_shader_stage stage, const char* ub_name);\n", mod_prefix(inp), prog.name);
                L("size_t {}{}_uniformblock_size(sg_shader_stage stage, const char* ub_name);\n", mod_prefix(inp), prog.name);
                L("int {}{}_uniform_offset(sg_shader_stage stage, const char* ub_name, const char* u_name);\n", mod_prefix(inp), prog.name);
                L("sg_shader_uniform_desc {}{}_uniform_desc(sg_shader_stage stage, const char* ub_name, const char* u_name);\n", mod_prefix(inp), prog.name);
            }
        }
    }
    write_vertex_attrs(inp, spirvcross);
    write_image_bind_slots(inp, spirvcross);
    write_sampler_bind_slots(inp, spirvcross);
    write_uniform_blocks(inp, spirvcross, slang);
}

static void write_shader_sources_and_blobs(const input_t& inp,
                                           const spirvcross_t& spirvcross,
                                           const bytecode_t& bytecode,
                                           slang_t::type_t slang)
{
    for (int snippet_index = 0; snippet_index < (int)inp.snippets.size(); snippet_index++) {
        const snippet_t& snippet = inp.snippets[snippet_index];
        if ((snippet.type != snippet_t::VS) && (snippet.type != snippet_t::FS)) {
            continue;
        }
        int src_index = spirvcross.find_source_by_snippet_index(snippet_index);
        assert(src_index >= 0);
        const spirvcross_source_t& src = spirvcross.sources[src_index];
        int blob_index = bytecode.find_blob_by_snippet_index(snippet_index);
        const bytecode_blob_t* blob = 0;
        if (blob_index != -1) {
            blob = &bytecode.blobs[blob_index];
        }
        std::vector<std::string> lines;
        pystring::splitlines(src.source_code, lines);
        /* first write the source code in a comment block */
        L("/*\n");
        for (const std::string& line: lines) {
            L("    {}\n", util::replace_C_comment_tokens(line));
        }
        L("*/\n");
        if (blob) {
            std::string c_name = fmt::format("{}{}_bytecode_{}", mod_prefix(inp), snippet.name, slang_t::to_str(slang));
            L("static const uint8_t {}[{}] = {{\n", c_name.c_str(), blob->data.size());
            const size_t len = blob->data.size();
            for (size_t i = 0; i < len; i++) {
                if ((i & 15) == 0) {
                    L("    ");
                }
                L("{:#04x},", blob->data[i]);
                if ((i & 15) == 15) {
                    L("\n");
                }
            }
            L("\n}};\n");
        }
        else {
            /* if no bytecode exists, write the source code, but also a byte array with a trailing 0 */
            std::string c_name = fmt::format("{}{}_source_{}", mod_prefix(inp), snippet.name, slang_t::to_str(slang));
            const size_t len = src.source_code.length() + 1;
            L("static const char {}[{}] = {{\n", c_name.c_str(), len);
            for (size_t i = 0; i < len; i++) {
                if ((i & 15) == 0) {
                    L("    ");
                }
                L("{:#04x},", src.source_code[i]);
                if ((i & 15) == 15) {
                    L("\n");
                }
            }
            L("\n}};\n");
        }
    }
}

static void write_stage(const char* indent,
                        const char* stage_name,
                        const spirvcross_source_t& src,
                        const std::string& src_name,
                        const bytecode_blob_t* blob,
                        const std::string& blob_name,
                        slang_t::type_t slang)
{
    if (blob) {
        L("{}desc.{}.bytecode.ptr = {};\n", indent, stage_name, blob_name);
        L("{}desc.{}.bytecode.size = {};\n", indent, stage_name, blob->data.size());
    }
    else {
        L("{}desc.{}.source = {};\n", indent, stage_name, src_name);
        const char* d3d11_tgt = nullptr;
        if (slang == slang_t::HLSL4) {
            d3d11_tgt = (0 == strcmp("vs", stage_name)) ? "vs_4_0" : "ps_4_0";
        }
        else if (slang == slang_t::HLSL5) {
            d3d11_tgt = (0 == strcmp("vs", stage_name)) ? "vs_5_0" : "ps_5_0";
        }
        if (d3d11_tgt) {
            L("{}desc.{}.d3d11_target = \"{}\";\n", indent, stage_name, d3d11_tgt);
        }
    }
    L("{}desc.{}.entry = \"{}\";\n", indent, stage_name, src.refl.entry_point);
    for (int ub_index = 0; ub_index < uniform_block_t::NUM; ub_index++) {
        const uniform_block_t* ub = find_uniform_block_by_slot(src.refl, ub_index);
        if (ub) {
            L("{}desc.{}.uniform_blocks[{}].size = {};\n", indent, stage_name, ub_index, roundup(ub->size, 16));
            L("{}desc.{}.uniform_blocks[{}].layout = SG_UNIFORMLAYOUT_STD140;\n", indent, stage_name, ub_index);
            if (slang_t::is_glsl(slang) && (ub->uniforms.size() > 0)) {
                if (ub->flattened) {
                    L("{}desc.{}.uniform_blocks[{}].uniforms[0].name = \"{}\";\n", indent, stage_name, ub_index, ub->struct_name);
                    L("{}desc.{}.uniform_blocks[{}].uniforms[0].type = {};\n", indent, stage_name, ub_index, uniform_type_to_flattened_sokol_type_str(ub->uniforms[0].type));
                    L("{}desc.{}.uniform_blocks[{}].uniforms[0].array_count = {};\n", indent, stage_name, ub_index, roundup(ub->size, 16) / 16);
                }
                else {
                    for (int u_index = 0; u_index < (int)ub->uniforms.size(); u_index++) {
                        const uniform_t& u = ub->uniforms[u_index];
                        L("{}desc.{}.uniform_blocks[{}].uniforms[{}].name = \"{}.{}\";\n", indent, stage_name, ub_index, u_index, ub->inst_name, u.name);
                        L("{}desc.{}.uniform_blocks[{}].uniforms[{}].type = {};\n", indent, stage_name, ub_index, u_index, uniform_type_to_sokol_type_str(u.type));
                        L("{}desc.{}.uniform_blocks[{}].uniforms[{}].array_count = {};\n", indent, stage_name, ub_index, u_index, u.array_count);
                    }
                }
            }
        }
    }
    for (int img_index = 0; img_index < image_t::NUM; img_index++) {
        const image_t* img = find_image_by_slot(src.refl, img_index);
        if (img) {
            L("{}desc.{}.images[{}].image_type = {};\n", indent, stage_name, img_index, img_type_to_sokol_type_str(img->type));
            L("{}desc.{}.images[{}].sample_type = {};\n", indent, stage_name, img_index, img_basetype_to_sokol_sampletype_str(img->sample_type));
            L("{}desc.{}.images[{}].multisampled = {};\n", indent, stage_name, img_index, img->multisampled ? "true" : "false");
        }
    }
    for (int smp_index = 0; smp_index < sampler_t::NUM; smp_index++) {
        const sampler_t* smp = find_sampler_by_slot(src.refl, smp_index);
        if (smp) {
            L("{}desc.{}.samplers[{}].type = {};\n", indent, stage_name, smp_index, smp_type_to_sokol_type_str(smp->type));
        }
    }
    // FIXME: combined image samplers
}

static void write_shader_desc_init(const char* indent, const program_t& prog, const input_t& inp, const spirvcross_t& spirvcross, const bytecode_t& bytecode, slang_t::type_t slang) {
    int vs_snippet_index = inp.snippet_map.at(prog.vs_name);
    int fs_snippet_index = inp.snippet_map.at(prog.fs_name);
    int vs_src_index = spirvcross.find_source_by_snippet_index(vs_snippet_index);
    int fs_src_index = spirvcross.find_source_by_snippet_index(fs_snippet_index);
    assert((vs_src_index >= 0) && (fs_src_index >= 0));
    const spirvcross_source_t& vs_src = spirvcross.sources[vs_src_index];
    const spirvcross_source_t& fs_src = spirvcross.sources[fs_src_index];
    int vs_blob_index = bytecode.find_blob_by_snippet_index(vs_snippet_index);
    int fs_blob_index = bytecode.find_blob_by_snippet_index(fs_snippet_index);
    const bytecode_blob_t* vs_blob = 0;
    const bytecode_blob_t* fs_blob = 0;
    if (vs_blob_index != -1) {
        vs_blob = &bytecode.blobs[vs_blob_index];
    }
    if (fs_blob_index != -1) {
        fs_blob = &bytecode.blobs[fs_blob_index];
    }
    std::string vs_src_name, fs_src_name;
    std::string vs_blob_name, fs_blob_name;
    if (vs_blob_index != -1) {
        vs_blob_name = fmt::format("{}{}_bytecode_{}", mod_prefix(inp), prog.vs_name, slang_t::to_str(slang));
    }
    else {
        vs_src_name = fmt::format("{}{}_source_{}", mod_prefix(inp), prog.vs_name, slang_t::to_str(slang));
    }
    if (fs_blob_index != -1) {
        fs_blob_name = fmt::format("{}{}_bytecode_{}", mod_prefix(inp), prog.fs_name, slang_t::to_str(slang));
    }
    else {
        fs_src_name = fmt::format("{}{}_source_{}", mod_prefix(inp), prog.fs_name, slang_t::to_str(slang));
    }

    /* write shader desc */
    for (int attr_index = 0; attr_index < attr_t::NUM; attr_index++) {
        const attr_t& attr = vs_src.refl.inputs[attr_index];
        if (attr.slot >= 0) {
            if (slang_t::is_glsl(slang)) {
                L("{}desc.attrs[{}].name = \"{}\";\n", indent, attr_index, attr.name);
            }
            else if (slang_t::is_hlsl(slang)) {
                L("{}desc.attrs[{}].sem_name = \"{}\";\n", indent, attr_index, attr.sem_name);
                L("{}desc.attrs[{}].sem_index = {};\n", indent, attr_index, attr.sem_index);
            }
        }
    }
    write_stage(indent, "vs", vs_src, vs_src_name, vs_blob, vs_blob_name, slang);
    write_stage(indent, "fs", fs_src, fs_src_name, fs_blob, fs_blob_name, slang);
    L("{}desc.label = \"{}{}_shader\";\n", indent, mod_prefix(inp), prog.name);
}

static std::string func_prefix(const args_t& args) {
    if (args.output_format != format_t::SOKOL_IMPL) {
        return std::string("static inline ");
    }
    else {
        return std::string();
    }
}

static void write_shader_desc_func(const program_t& prog, const args_t& args, const input_t& inp,
                                   const std::array<spirvcross_t,slang_t::NUM>& spirvcross,
                                   const std::array<bytecode_t,slang_t::NUM>& bytecode)
{
    L("{}const sg_shader_desc* {}{}_shader_desc(sg_backend backend) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    for (int i = 0; i < slang_t::NUM; i++) {
        slang_t::type_t slang = (slang_t::type_t) i;
        if (args.slang & slang_t::bit(slang)) {
            if (args.ifdef) {
                L("  #if defined({})\n", sokol_define(slang));
            }
            L("  if (backend == {}) {{\n", sokol_backend(slang));
            L("    static sg_shader_desc desc;\n");
            L("    static bool valid;\n");
            L("    if (!valid) {{\n");
            L("      valid = true;\n");
            write_shader_desc_init("      ", prog, inp, spirvcross[i], bytecode[i], slang);
            L("    }}\n");
            L("    return &desc;\n");
            L("  }}\n");
            if (args.ifdef) {
                L("  #endif /* {} */\n", sokol_define(slang));
            }
        }
    }
    L("  return 0;\n");
    L("}}\n");
}

static void write_attr_slot_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    assert(vs_src);

    L("{}int {}{}_attr_slot(const char* attr_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)attr_name;\n");
    for (const attr_t& attr: vs_src->refl.inputs) {
        if (attr.slot >= 0) {
            L("  if (0 == strcmp(attr_name, \"{}\")) {{\n", attr.name);
            L("    return {};\n", attr.slot);
            L("  }}\n");
        }
    }
    L("  return -1;\n");
    L("}}\n");
}

static void write_image_slot_stage(const spirvcross_source_t* src) {
    for (const image_t& img: src->refl.images) {
        if (img.slot >= 0) {
            L("    if (0 == strcmp(img_name, \"{}\")) {{\n", img.name);
            L("      return {};\n", img.slot);
            L("    }}\n");
        }
    }
}

static void write_sampler_slot_stage(const spirvcross_source_t* src) {
    for (const sampler_t& smp: src->refl.samplers) {
        if (smp.slot >= 0) {
            L("    if (0 == strcmp(smp_name, \"{}\")) {{\n", smp.name);
            L("      return {};\n", smp.slot);
            L("    }}\n");
        }
    }
}

static void write_image_slot_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
    assert(vs_src && fs_src);

    L("{}int {}{}_image_slot(sg_shader_stage stage, const char* img_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)stage; (void)img_name;\n");
    if (!vs_src->refl.images.empty()) {
        L("  if (SG_SHADERSTAGE_VS == stage) {{\n");
        write_image_slot_stage(vs_src);
        L("  }}\n");
    }
    if (!fs_src->refl.images.empty()) {
        L("  if (SG_SHADERSTAGE_FS == stage) {{\n");
        write_image_slot_stage(fs_src);
        L("  }}\n");
    }
    L("  return -1;\n");
    L("}}\n");
}

static void write_sampler_slot_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
    assert(vs_src && fs_src);

    L("{}int {}{}_sample_slot(sg_shader_stage stage, const char* smp_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)stage; (void)smp_name;\n");
    if (!vs_src->refl.samplers.empty()) {
        L("  if (SG_SHADERSTAGE_VS == stage) {{\n");
        write_sampler_slot_stage(vs_src);
        L("  }}\n");
    }
    if (!fs_src->refl.samplers.empty()) {
        L("  if (SG_SHADERSTAGE_FS == stage) {{\n");
        write_sampler_slot_stage(fs_src);
        L("  }}\n");
    }
    L("  return -1;\n");
    L("}}\n");
}

static void write_uniformblock_slot_stage(const spirvcross_source_t* src) {
    for (const uniform_block_t& ub: src->refl.uniform_blocks) {
        if (ub.slot >= 0) {
            L("    if (0 == strcmp(ub_name, \"{}\")) {{\n", ub.struct_name);
            L("      return {};\n", ub.slot);
            L("    }}\n");
        }
    }
}

static void write_uniformblock_slot_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
    assert(vs_src && fs_src);

    L("{}int {}{}_uniformblock_slot(sg_shader_stage stage, const char* ub_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)stage; (void)ub_name;\n");
    if (!vs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_VS == stage) {{\n");
        write_uniformblock_slot_stage(vs_src);
        L("  }}\n");
    }
    if (!fs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_FS == stage) {{\n");
        write_uniformblock_slot_stage(fs_src);
        L("  }}\n");
    }
    L("  return -1;\n");
    L("}}\n");
}

static void write_uniformblock_size_stage(const spirvcross_source_t* src, const input_t& inp) {
    for (const uniform_block_t& ub: src->refl.uniform_blocks) {
        if (ub.slot >= 0) {
            L("    if (0 == strcmp(ub_name, \"{}\")) {{\n", ub.struct_name);
            L("      return sizeof({}{}_t);\n", mod_prefix(inp), ub.struct_name);
            L("    }}\n");
        }
    }
}

static void write_uniformblock_size_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
    assert(vs_src && fs_src);

    L("{}size_t {}{}_uniformblock_size(sg_shader_stage stage, const char* ub_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)stage; (void)ub_name;\n");
    if (!vs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_VS == stage) {{\n");
        write_uniformblock_size_stage(vs_src, inp);
        L("  }}\n");
    }
    if (!fs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_FS == stage) {{\n");
        write_uniformblock_size_stage(fs_src, inp);
        L("  }}\n");
    }
    L("  return 0;\n");
    L("}}\n");
}

static void write_uniform_offset_stage(const spirvcross_source_t* src) {
    for (const uniform_block_t& ub: src->refl.uniform_blocks) {
        if (ub.slot >= 0) {
            L("    if (0 == strcmp(ub_name, \"{}\")) {{\n", ub.struct_name);
            for (const uniform_t& u: ub.uniforms) {
                L("      if (0 == strcmp(u_name, \"{}\")) {{\n", u.name);
                L("        return {};\n", u.offset);
                L("      }}\n");
            }
            L("    }}\n");
        }
    }
}

static void write_uniform_offset_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
    assert(vs_src && fs_src);

    L("{}int {}{}_uniform_offset(sg_shader_stage stage, const char* ub_name, const char* u_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)stage; (void)ub_name; (void)u_name;\n");
    if (!vs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_VS == stage) {{\n");
        write_uniform_offset_stage(vs_src);
        L("  }}\n");
    }
    if (!fs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_FS == stage) {{\n");
        write_uniform_offset_stage(fs_src);
        L("  }}\n");
    }
    L("  return -1;\n");
    L("}}\n");
}

static void write_uniform_desc_stage(const spirvcross_source_t* src) {
    for (const uniform_block_t& ub: src->refl.uniform_blocks) {
        if (ub.slot >= 0) {
            L("    if (0 == strcmp(ub_name, \"{}\")) {{\n", ub.struct_name);
            for (const uniform_t& u: ub.uniforms) {
                L("      if (0 == strcmp(u_name, \"{}\")) {{\n", u.name);
                L("        desc.name = \"{}\";\n", u.name);
                L("        desc.type = {};\n", uniform_type_to_sokol_type_str(u.type));
                L("        desc.array_count = {};\n", u.array_count);
                L("        return desc;\n");
                L("      }}\n");
            }
            L("    }}\n");
        }
    }
}

static void write_uniform_desc_func(const program_t& prog, const args_t& args, const input_t& inp, const spirvcross_t& spirvcross) {
    const spirvcross_source_t* vs_src = find_spirvcross_source_by_shader_name(prog.vs_name, inp, spirvcross);
    const spirvcross_source_t* fs_src = find_spirvcross_source_by_shader_name(prog.fs_name, inp, spirvcross);
    assert(vs_src && fs_src);

    L("{}sg_shader_uniform_desc {}{}_uniform_desc(sg_shader_stage stage, const char* ub_name, const char* u_name) {{\n", func_prefix(args), mod_prefix(inp), prog.name);
    L("  (void)stage; (void)ub_name; (void)u_name;\n");
    L("  #if defined(__cplusplus)\n");
    L("  sg_shader_uniform_desc desc = {{}};\n");
    L("  #else\n");
    L("  sg_shader_uniform_desc desc = {{0}};\n");
    L("  #endif\n");
    if (!vs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_VS == stage) {{\n");
        write_uniform_desc_stage(vs_src);
        L("  }}\n");
    }
    if (!fs_src->refl.uniform_blocks.empty()) {
        L("  if (SG_SHADERSTAGE_FS == stage) {{\n");
        write_uniform_desc_stage(fs_src);
        L("  }}\n");
    }
    L("  return desc;\n");
    L("}}\n");

}

errmsg_t sokol_t::gen(const args_t& args, const input_t& inp,
                     const std::array<spirvcross_t,slang_t::NUM>& spirvcross,
                     const std::array<bytecode_t,slang_t::NUM>& bytecode)
{
    // first write everything into a string, and only when no errors occur,
    // dump this into a file (so we don't have half-written files lying around)
    file_content.clear();

    L("#pragma once\n");
    errmsg_t err;
    bool comment_header_written = false;
    bool common_decls_written = false;
    bool guard_written = false;
    for (int i = 0; i < slang_t::NUM; i++) {
        slang_t::type_t slang = (slang_t::type_t) i;
        if (args.slang & slang_t::bit(slang)) {
            errmsg_t err = check_errors(inp, spirvcross[i], slang);
            if (err.valid) {
                return err;
            }
            if (!comment_header_written) {
                comment_header_written = true;
                write_header(args, inp, spirvcross[i]);
            }
            if (!common_decls_written) {
                common_decls_written = true;
                write_common_decls(slang, args, inp, spirvcross[i]);
            }
            if (!guard_written) {
                guard_written = true;
                if (args.output_format == format_t::SOKOL_DECL) {
                    L("#if !defined(SOKOL_SHDC_DECL)\n");
                }
                else if (args.output_format == format_t::SOKOL_IMPL) {
                    L("#if defined(SOKOL_SHDC_IMPL)\n");
                }
            }
            if (args.ifdef) {
                L("#if defined({})\n", sokol_define(slang));
            }
            write_shader_sources_and_blobs(inp, spirvcross[i], bytecode[i], slang);
            if (args.ifdef) {
                L("#endif /* {} */\n", sokol_define(slang));
            }
        }
    }

    // write access functions which return sg_shader_desc pointers
    if (args.output_format != format_t::SOKOL_IMPL) {
        L("#if !defined(SOKOL_GFX_INCLUDED)\n");
        L("  #error \"Please include sokol_gfx.h before {}\"\n", pystring::os::path::basename(args.output));
        L("#endif\n");
    }
    for (const auto& item: inp.programs) {
        const program_t& prog = item.second;
        write_shader_desc_func(prog, args, inp, spirvcross, bytecode);
        if (args.reflection) {
            int slang_index = (int)slang_t::first_valid(args.slang);
            assert((slang_index >= 0) && (slang_index < slang_t::NUM));
            write_attr_slot_func(prog, args, inp, spirvcross[slang_index]);
            write_image_slot_func(prog, args, inp, spirvcross[slang_index]);
            write_sampler_slot_func(prog, args, inp, spirvcross[slang_index]);
            write_uniformblock_slot_func(prog, args, inp, spirvcross[slang_index]);
            write_uniformblock_size_func(prog, args, inp, spirvcross[slang_index]);
            write_uniform_offset_func(prog, args, inp, spirvcross[slang_index]);
            write_uniform_desc_func(prog, args, inp, spirvcross[slang_index]);
        }
    }

    if (guard_written) {
        if (args.output_format == format_t::SOKOL_DECL) {
            L("#endif /* SOKOL_SHDC_DECL */\n");
        }
        else if (args.output_format == format_t::SOKOL_IMPL) {
            L("#endif /* SOKOL_SHDC_IMPL */\n");
        }
    }

    // write result into output file
    FILE* f = fopen(args.output.c_str(), "w");
    if (!f) {
        return errmsg_t::error(inp.base_path, 0, fmt::format("failed to open output file '{}'", args.output));
    }
    fwrite(file_content.c_str(), file_content.length(), 1, f);
    fclose(f);
    return errmsg_t();
}

} // namespace shdc
